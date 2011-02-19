#!/usr/bin/env python
from sandbox import Sandbox
from ants import Ants
import time
import traceback

def run_game(game, botcmds, turntime, loadtime, turns=5000,
             output_file="testout.txt", verbose=False, serial=False):
    try:
        bot_log = [open('logs/bot%s.log' % i, 'w') for i in range(len(botcmds))]
        # create bot sandboxes
        bots = [Sandbox(*bot) for bot in botcmds]
        for b, bot in enumerate(bots):
            if not bot.is_alive:
                print('bot %s did not start' % botcmds[b])
                game.kill_player(b)

        if output_file:
            of = open(output_file + ".replay", "w")
            of.write(game.get_player_start())
            of.flush()

        print('running for %s turns' % turns)
        for turn in range(turns+1):
            print('turn %s' % turn)
            try:
                if turn == 0:
                    game.start_game()

                # send game state to each player
                for b, bot in enumerate(bots):
                    if game.is_alive(b):
                        if turn == 0:
                            start = game.get_player_start(b) + 'ready\n'
                            bot.write(start)
                            bot_log[b].write(start)
                            bot_log[b].flush()
                        else:
                            state = 'turn ' + str(turn) + '\n' + game.get_player_state(b) + 'go\n'
                            bot.write(state)
                            bot_log[b].write(state)
                            bot_log[b].flush()
                if turn > 0:
                    game.start_turn()

                # get moves from each player
                if turn == 0:
                    time_limit = float(loadtime) / 1000
                else:
                    time_limit = float(turntime) / 1000
                start_time = time.time()
                bot_finished = [not game.is_alive(b) for b in range(len(bots))]
                bot_moves = [[] for b in bots]

                # loop until received all bots send moves or are dead
                #   or when time is up
                while (sum(bot_finished) < len(bot_finished) and
                        time.time() - start_time < time_limit):
                    time.sleep(0.01)
                    for b, bot in enumerate(bots):
                        if bot_finished[b]:
                            continue # already got bot moves
                        if not bot.is_alive:
                            print('bot %s died' % b)
                            bot_finished[b] = True
                            game.kill_player(b)
                            continue # bot is dead
                        line = bot.read_line()
                        while line != None:
                            line = line.strip()
                            if line.lower() == 'go':
                                bot_finished[b] = True
                                break
                            else:
                                bot_moves[b].append(line)
                            line = bot.read_line()

                # kill timed out bots
                for b, finished in enumerate(bot_finished):
                    if not finished:
                        print("bot %s timed out" % b)
                        game.kill_player(b)
                        bots[b].kill()
                        
                # process all moves
                bot_alive = [game.is_alive(b) for b in range(len(bots))]
                if turn > 0 and not game.game_over():
                    game.do_all_moves(bot_moves)
                    game.finish_turn()
                for b, alive in enumerate(bot_alive):
                    if alive and not game.is_alive(b):
                        print("bot %s eliminated" % b)

            except:
                traceback.print_exc()
                print("Got an error running the bots.")
                raise

            if output_file:
                of.write('turn %s\n' % turn)
                of.write(game.get_state())
                of.flush()

            if verbose:
                stats = game.get_stats()
                s = 'turn %4d stats: '
                for key, values in stats:
                    s += '%s: %s' % (key, values)
                print("\r%-50s" % s)

            alive = [game.is_alive(b) for b in range(len(bots))]
            if sum(alive) <= 1:
                break

        # send bots final state and score, output to replay file
        game.finish_game()
        end_line = 'end'
        for score in game.get_scores():
            end_line += ' %s' % score
        end_line += '\n'
        print(end_line)
        for b, bot in enumerate(bots):
            if game.is_alive(b):
                state = end_line + game.get_player_state(b)
                bot.write(state)
                bot_log[b].write(state)
                bot_log[b].flush()
        if output_file:
            of.write(end_line)
            of.write(game.get_state())
            of.flush()

    except Exception:
        traceback.print_exc()
    finally:
        for log in bot_log:
            log.close()
        for bot in bots:
            if bot.is_alive:
                bot.kill()
        if output_file:
            of.close()

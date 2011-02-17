#!/usr/bin/env python
from sandbox import Sandbox
from ants import Ants
import time
import traceback

def save_image(game, turn, anno="", player=None):
    img = game.render_image(player)
    scale = 4
    new_size = (img.size[0] * scale, img.size[1] * scale)
    img = img.resize(new_size)
    img.save("playback/%s_%s.png" % (anno, str(turn).zfill(5)))

def run_game(game, botcmds, timeoutms, loadtimeoutms, num_turns=1000,
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
            of = open(output_file, "w")
            of.write(game.get_state())
            of.flush()

        print('running for %s turns' % num_turns)
        for turn in range(num_turns+1):
            print('turn %s' % turn)
            try:
                if turn == 1:
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
                    time_limit = float(loadtimeoutms) / 1000
                else:
                    time_limit = float(timeoutms) / 1000
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
                of.write(game.get_state())
                of.flush()
            #for i in range(len(bots)):
            #    save_image(game, turn, 'player%s' % i, i)
            save_image(game, turn, 'frame')
            #print(game.get_state())

            if verbose:
                stats = game.get_stats()
                s = 'turn %4d stats: '
                for key, values in stats:
                    s += '%s: %s' % (key, values)
                sys.stderr.write("\r%-50s" % s)

            alive = [game.is_alive(b) for b in range(len(bots))]
            #print('alive %s' % alive)
            if sum(alive) <= 1:
                break

        game.finish_game()
        #print(game.get_state())
    except Exception as ex:
        print(ex)
    finally:
        for log in bot_log:
            log.close()
        for bot in bots:
            if bot.is_alive:
                bot.kill()
        if output_file:
            of.close()
    return "Game Over, %s" % game.get_scores()
    f.close()

#!/usr/bin/env python
from sandbox import Sandbox
import time
import traceback
import os
import sys

def run_game(game, botcmds, options, gameid=0):
    output_json = options.get("output_json", False)
    output_dir = options.get("output_dir")
    log_input = options.get("log_input", False)
    log_output = options.get("log_output", False)
    turns = int(options["turns"])
    loadtime = float(options["loadtime"]) / 1000
    turntime = float(options["turntime"]) / 1000
    verbose = options.get("verbose", False)

    error = ''

    try:
        if output_dir:
            if log_input:
                bot_input_log = [open(os.path.join(output_dir, '%s.bot%s.input' % (gameid, i)), "w") 
                                 for i in range(len(botcmds))]
            if log_output:
                bot_output_log = [open(os.path.join(output_dir, '%s.bot%s.output' % (gameid, i)), "w") 
                                  for i in range(len(botcmds))]
        # create bot sandboxes
        bots = [Sandbox(*bot) for bot in botcmds]
        for b, bot in enumerate(bots):
            if not bot.is_alive:
                if verbose:
                    print('bot %s did not start' % botcmds[b])
                game.kill_player(b)

        if output_dir:
            of = open(os.path.join(output_dir, '%s.stream' % gameid), "w")
            of.write(game.get_player_start())
            # TODO: write player names and crap
            of.flush()

        if verbose:
            print('running for %s turns' % turns)
        for turn in range(turns+1):
            if verbose:
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
                            if output_dir and log_input:
                                    bot_input_log[b].write(start)
                                    bot_input_log[b].flush()
                        else:
                            state = 'turn ' + str(turn) + '\n' + game.get_player_state(b) + 'go\n'
                            bot.write(state)
                            if output_dir and log_input:
                                bot_input_log[b].write(state)
                                bot_input_log[b].flush()
                if turn > 0:
                    if output_dir:
                        of.write('turn %s\n' % turn)
                        of.write('score %s\n' % ' '.join([str(s) for s in game.get_scores()]))
                        of.write(game.get_state())
                        of.flush()
                    game.start_turn()

                # get moves from each player
                if turn == 0:
                    time_limit = loadtime
                else:
                    time_limit = turntime
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
                            if verbose:
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
                        if verbose:
                            print("bot %s timed out" % b)
                        if output_dir:
                            of.write('# bot %s timed out\n' % b)
                        game.kill_player(b)
                        bots[b].kill()
                                            
                # process all moves
                bot_alive = [game.is_alive(b) for b in range(len(bots))]
                if turn > 0 and not game.game_over():
                    for b, moves in enumerate(bot_moves):
                        if game.is_alive(b):
                            valid, invalid = game.do_moves(b, moves) 
                            if output_dir and log_output:
                                bot_output_log[b].write('# turn %s\n' % turn)
                                if len(valid) > 0:
                                    tmp = '\n'.join(valid) + '\n'
                                    bot_output_log[b].write(tmp)
                                    bot_output_log[b].flush()
                                    of.write(tmp)
                                    of.flush()

                    game.finish_turn()
                    
                # send ending info to eliminated bots
                for b, alive in enumerate(bot_alive):
                    if alive and not game.is_alive(b):
                        if verbose:
                            print("bot %s eliminated" % b)
                        if output_dir:
                            of.write('# bot %s eliminated\n' % b)
                        end_line = 'end\nscore %s\n' % ' '.join([str(s) for s in game.get_scores()])
                        end_line += game.get_player_state(b)
                        bots[b].write(end_line)
                        if output_dir and log_output:
                            bot_output_log[b].write(end_line)
                            bot_output_log[b].flush()

            except:
                raise

            if verbose:
                stats = game.get_stats()
                s = 'turn %4d stats: ' % turn
                for key, values in stats.items():
                    s += '%s: %s' % (key, values)
                print("\r%-50s" % s)

            alive = [game.is_alive(b) for b in range(len(bots))]
            if sum(alive) <= 1:
                break

        # send bots final state and score, output to replay file
        game.finish_game()
        score_line = 'end\n'
        score_line += 'players %s\n' % len(bots)
        score_line +='score %s\n' % ' '.join([str(s) for s in game.get_scores()])
        for b, bot in enumerate(bots):
            if game.is_alive(b):
                state = score_line + game.get_player_state(b) + 'go\n'
                bot.write(state)
                if output_dir and log_input:
                    bot_input_log[b].write(state)
                    bot_input_log[b].flush()
        if output_dir:
            of.write(score_line)
            of.write(game.get_state())
            of.flush()

            cof = open(os.path.join(output_dir, '%s.replay' % gameid), "w")
            cof.write(game.get_replay())
            cof.close()

    except Exception:
        error = traceback.format_exc()
        if verbose:
            print(error)
    finally:
        for bot in bots:
            if bot.is_alive:
                bot.kill()
        if output_dir:
            try:
                of.close()
            except:
                pass
            if log_input:
                for log in bot_input_log:
                    log.close()
            if log_output:
                for log in bot_output_log:
                    log.close()
    if output_json:
        # this isn't actually json yet, the worker will encode it
        json_response = {}
        if error:
            json_response["error"] = error
        else:
            scores = game.get_scores()
            json_response['score'] = scores
            json_response['rank'] = [sorted(set(scores)).index(x) for x in scores]
            json_response['player_info'] = [{} for x in range(len(bots))]
            json_response['replay'] = game.get_replay()
        return json_response

#!/usr/bin/env python
from sandbox import Sandbox
import time
import traceback
import os
import sys
import json

def run_game(game, botcmds, options, gameid=0):
    output = options.get("stdout", None)
    output_dir = options.get("output_dir")
    log_input = options.get("log_input", False)
    log_output = options.get("log_output", False)
    turns = int(options["turns"])
    loadtime = float(options["loadtime"]) / 1000
    turntime = float(options["turntime"]) / 1000
    verbose = options.get("verbose", False)

    error = ''

    bot_input_log = bot_output_log = stream_log = replay_log = None
    bots = []

    try:
        # create bot sandboxes
        bots = [Sandbox(*bot) for bot in botcmds]
        for b, bot in enumerate(bots):
            if not bot.is_alive:
                if verbose:
                    print >> sys.stderr, 'bot %s did not start' % botcmds[b]
                game.kill_player(b)

        # initialise file logs
        if output_dir:
            stream_log = open(os.path.join(output_dir, '%s.stream' % gameid), "w")

            replay_log = open(os.path.join(output_dir, '%s.replay' % gameid), "w")

            if log_input:
                bot_input_log = [open(os.path.join(output_dir, '%s.bot%s.input' % (gameid, i)), "w")
                                 for i in range(len(botcmds))]
            if log_output:
                bot_output_log = [open(os.path.join(output_dir, '%s.bot%s.output' % (gameid, i)), "w")
                                  for i in range(len(botcmds))]

        # initialise stdout logs
        if output == 'replay':
            if replay_log:
                replay_log = Tee(replay_log, sys.stdout)
            else:
                replay_log = sys.stdout
        elif output == 'stream':
            if stream_log:
                stream_log = Tee(stream_log, sys.stdout)
            else:
                stream_log = sys.stdout

        if stream_log:
            stream_log.write(game.get_player_start())
            stream_log.flush()

        if verbose:
            print >> sys.stderr, 'running for %s turns' % turns
        for turn in range(turns+1):
            if verbose:
                print >> sys.stderr, 'turn %s' % turn
            try:
                if turn == 0:
                    game.start_game()

                # send game state to each player
                for b, bot in enumerate(bots):
                    if game.is_alive(b):
                        if turn == 0:
                            start = game.get_player_start(b) + 'ready\n'
                            bot.write(start)
                            if bot_input_log:
                                bot_input_log[b].write(start)
                                bot_input_log[b].flush()
                        else:
                            state = 'turn ' + str(turn) + '\n' + game.get_player_state(b) + 'go\n'
                            bot.write(state)
                            if bot_input_log:
                                bot_input_log[b].write(state)
                                bot_input_log[b].flush()
                if turn > 0:
                    if stream_log:
                        stream_log.write('turn %s\n' % turn)
                        stream_log.write('score %s\n' % ' '.join([str(s) for s in game.get_scores()]))
                        stream_log.write(game.get_state())
                        stream_log.flush()
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
                                print >> sys.stderr, 'bot %s died' % b
                            bot_finished[b] = True
                            game.kill_player(b)
                            continue # bot is dead

                        # read a maximum of 100 lines per iteration
                        for x in range(100):
                            line = bot.read_line()
                            if line is None:
                                # stil waiting for more data
                                break
                            line = line.strip()
                            if line.lower() == 'go':
                                bot_finished[b] = True
                                # bot finished sending data for this turn
                                break
                            bot_moves[b].append(line)

                # kill timed out bots
                for b, finished in enumerate(bot_finished):
                    if not finished:
                        if verbose:
                            print >> sys.stderr, "bot %s timed out" % b
                        if stream_log:
                            stream_log.write('# bot %s timed out\n' % b)
                        game.kill_player(b)
                        bots[b].kill()

                # process all moves
                bot_alive = [game.is_alive(b) for b in range(len(bots))]
                if turn > 0 and not game.game_over():
                    for b, moves in enumerate(bot_moves):
                        if game.is_alive(b):
                            valid, invalid = game.do_moves(b, moves)
                            if bot_output_log:
                                bot_output_log[b].write('# turn %s\n' % turn)
                                if valid:
                                    bot_output_log[b].write('\n'.join(valid)+'\n')
                                    bot_output_log[b].flush()
                                if invalid:
                                    bot_output_log[b].write('# invalid actions:\n')
                                    bot_output_log[b].write('\n'.join(invalid)+'\n')
                                    bot_output_log[b].flush()

                    game.finish_turn()

                # send ending info to eliminated bots
                for b, alive in enumerate(bot_alive):
                    if alive and not game.is_alive(b):
                        if verbose:
                            print >> sys.stderr, "bot %s eliminated" % b
                        if stream_log:
                            stream_log.write('# bot %s eliminated\n' % b)
                        end_line = 'end\nscore %s\n' % ' '.join([str(s) for s in game.get_scores()])
                        end_line += game.get_player_state(b)
                        bots[b].write(end_line)
                        if bot_output_log:
                            bot_output_log[b].write(end_line)
                            bot_output_log[b].flush()

            except:
                raise

            if verbose:
                stats = game.get_stats()
                s = 'turn %4d stats: ' % turn
                for key, values in stats.items():
                    s += '%s: %s' % (key, values)
                print >> sys.stderr, "\r%-50s" % s

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
                if bot_input_log:
                    bot_input_log[b].write(state)
                    bot_input_log[b].flush()
        if stream_log:
            stream_log.write(score_line)
            stream_log.write(game.get_state())
            stream_log.flush()

    except Exception:
        error = traceback.format_exc()
        if verbose:
            print >> sys.stderr, error
    finally:
        for bot in bots:
            if bot.is_alive:
                bot.kill()

        # close all the open files
        if stream_log:
            stream_log.close()
        if bot_input_log:
            for log in bot_input_log:
                log.close()
        if bot_output_log:
            for log in bot_output_log:
                log.close()

    if replay_log:
        replay = game.get_replay()
        replay = {
            'challenge': game.__class__.__name__.lower(),
            'replayformat': 'storage',
            'replaydata': game.get_replay(),
        }
        json.dump(replay, replay_log,sort_keys=True)
        replay_log.close()

class Tee(object):
    """ Write to multiple files at once """
    def __init__(self, *files):
        self.files = files
    def write(self, data):
        for file in self.files:
            file.write(data)
    def flush(self):
        for file in self.files:
            file.flush()
    def close(self):
        for file in self.files:
            file.close()

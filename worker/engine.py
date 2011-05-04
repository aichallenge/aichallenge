#!/usr/bin/env python
from __future__ import print_function
from sandbox import Sandbox
import time
import traceback
import os
import sys
import json

def run_game(game, botcmds, options):
#    output = options.get('stdout', None)
#    output_dir = options.get('output_dir')
#    output_json = options.get('output_json', False)
#    log_input = options.get('log_input', False)
#    log_output = options.get('log_output', False)
#    log_stderr = options.get('log_stderr', False)
    
    # file descriptors for replay and streaming formats
    replay_log = options.get('replay_log', None)
    stream_log = options.get('stream_log', None)
    # file descriptors for bots, should be list matching # of bots
    input_logs = options.get('input_logs', [None]*len(botcmds))
    output_logs = options.get('output_logs', [None]*len(botcmds))
    error_logs = options.get('error_logs', [None]*len(botcmds))
    
    turns = int(options['turns'])
    loadtime = float(options['loadtime']) / 1000
    turntime = float(options['turntime']) / 1000
    verbose = options.get('verbose', False)
    
    location = options.get('location', 'local')
    gameid = options.get('gameid', 0)

    error = ''

    bots = []

    try:
        # create bot sandboxes
        bots = []
        for b, bot in enumerate(botcmds):
#            if log_stderr == 'file':
#                stderr_fd = open(os.path.join(output_dir, '%s.bot%s.stderr' % (gameid, b)), 'w')
#            elif log_stderr == 'stderr':
#                stderr_fd = sys.stderr
#            else:
#                stderr_fd = None
            
            sandbox = Sandbox(*bot, stderr=error_logs and error_logs[b])
            bots.append(sandbox)

            # ensure it started
            if not sandbox.is_alive:
                if verbose:
                    print('# bot %s did not start' % b, file=sys.stderr)
                game.kill_player(b)
            sandbox.pause()

        if stream_log:
            stream_log.write(game.get_player_start())
            stream_log.flush()

        if verbose:
            print('# running for %s turns' % turns)
        for turn in range(turns+1):
            try:
                if turn == 0:
                    game.start_game()

                # resume all bots
                for bot in bots:
                    if bot.is_alive:
                        bot.resume()

                # send game state to each player
                for b, bot in enumerate(bots):
                    if game.is_alive(b):
                        if turn == 0:
                            start = game.get_player_start(b) + 'ready\n'
                            bot.write(start)
                            if input_logs and input_logs[b]:
                                input_logs[b].write(start)
                                input_logs[b].flush()
                        else:
                            state = 'turn ' + str(turn) + '\n' + game.get_player_state(b) + 'go\n'
                            bot.write(state)
                            if input_logs and input_logs[b]:
                                input_logs[b].write(state)
                                input_logs[b].flush()

                # pause all bots again
                for bot in bots:
                    if bot.is_alive:
                        bot.pause()

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
                bot_moves = []

                log_lines = []
                if options.get('serial', False):
                    for bot in bots:
                        moves, log_lines = get_moves(game, [bot], time_limit)
                        bot_moves.extend(moves)
                else:
                    bot_moves, log_lines = get_moves(game, bots, time_limit)

                # handle any logs that get_moves produced
                if log_lines:
                    if verbose:
                        print('\n'.join(log_lines), file=sys.stderr)
                    if stream_log:
                        stream_log.write('\n'.join(('# '+s for s in log_lines)) + '\n')

                # process all moves
                bot_alive = [game.is_alive(b) for b in range(len(bots))]
                if turn > 0 and not game.game_over():
                    for b, moves in enumerate(bot_moves):
                        if game.is_alive(b):
                            valid, invalid = game.do_moves(b, moves)
                            if output_logs:
                                output_logs[b].write('# turn %s\n' % turn)
                                if valid:
                                    if output_logs and output_logs[b]:
                                        output_logs[b].write('\n'.join(valid)+'\n')
                                        output_logs[b].flush()
                                if invalid:
                                    if output_logs and output_logs[b]:
                                        output_logs[b].write('# invalid actions:\n')
                                        output_logs[b].write('\n'.join(invalid)+'\n')
                                        output_logs[b].flush()

                    game.finish_turn()

                # send ending info to eliminated bots
                for b, alive in enumerate(bot_alive):
                    if alive and not game.is_alive(b):
                        if verbose:
                            print('# bot %s eliminated' % b)
                        if stream_log:
                            stream_log.write('# bot %s eliminated\n' % b)
                        end_line = 'end\nscore %s\n' % ' '.join([str(s) for s in game.get_scores()])
                        end_line += game.get_player_state(b)
                        bots[b].write(end_line)
                        if output_logs and output_logs[b]:
                            output_logs[b].write(end_line)
                            output_logs[b].flush()

            except:
                raise

            if verbose:
                stats = game.get_stats()
                s = '# turn %4d stats: ' % turn
                for key, values in stats.items():
                    s += '%s: %s' % (key, values)
                print('\r%-50s' % s)

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
                if input_logs and input_logs[b]:
                    input_logs[b].write(state)
                    input_logs[b].flush()
        if stream_log:
            stream_log.write(score_line)
            stream_log.write(game.get_state())
            stream_log.flush()

    except Exception as e:
        # TODO: sanitize error output, tracebacks shouldn't be sent to workers
        error = traceback.format_exc()
        if verbose:
            print(traceback.format_exc(), file=sys.stderr)
        error = str(e)
    finally:
        for bot in bots:
            if bot.is_alive:
                bot.kill()
            bot.release()

    if error:
        game_result = { 'error': error }
    else:
        scores = game.get_scores()
        game_result = {
            'challenge': game.__class__.__name__.lower(),
            'location': location,
            'gameid': gameid,
            'player_info': [{} for x in range(len(bots))],
            'score': scores,
            'rank': [sorted(set(scores)).index(x) for x in scores],
            'replayformat': 'json',
            'replaydata': game.get_replay(),
        }
    
    if replay_log:
        json.dump(game_result, replay_log, sort_keys=True)
        
    return game_result
    
def get_moves(game, bots, time_limit):
    bot_finished = [not game.is_alive(b) for b in range(len(bots))]
    bot_moves = [[] for b in bots]
    start_time = time.time()
    log_lines = []

    # resume all bots
    for bot in bots:
        if bot.is_alive:
            bot.resume()

    # loop until received all bots send moves or are dead
    #   or when time is up
    while (sum(bot_finished) < len(bot_finished) and
            time.time() - start_time < time_limit):
        time.sleep(0.01)
        for b, bot in enumerate(bots):
            if bot_finished[b]:
                continue # already got bot moves
            if not bot.is_alive:
                log_lines.append('# bot %s died' % b)
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

    # pause all bots again
    for bot in bots:
        if bot.is_alive:
            bot.pause()

    # kill timed out bots
    for b, finished in enumerate(bot_finished):
        if not finished:
            log_lines.append('bot %s timed out' % b)
            game.kill_player(b)
            bots[b].kill()

    return bot_moves, log_lines


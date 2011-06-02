#!/usr/bin/env python
from __future__ import print_function
from sandbox import Sandbox
import time
import traceback
import os
import random
import sys
import json
import io

class Head(object):
    'Capture first part of file write and discard remainder'
    def __init__(self, file, max_capture=1024):
        self.file = file
        self.max_capture = max_capture
        self.capture = ''
        self.capture_len = 0
    def write(self, data):
        if self.file:
            self.file.write(data)
        capture_left = self.max_capture - self.capture_len
        if capture_left > 0:
            if len(data) >= capture_left:
                self.capture += data[:capture_left]
                self.capture_len = self.max_capture
            else:
                self.capture += data
                self.capture_len += len(data)
    def flush(self):
        if self.file:
            self.file.flush()
    def close(self):
        if self.file:
            self.file.close()
    def head(self):
        return self.capture

def run_game(game, botcmds, options):
    # file descriptors for replay and streaming formats
    replay_log = options.get('replay_log', None)
    stream_log = options.get('stream_log', None)
    verbose_log = options.get('verbose_log', None)
    # file descriptors for bots, should be list matching # of bots
    input_logs = options.get('input_logs', [None]*len(botcmds))
    output_logs = options.get('output_logs', [None]*len(botcmds))
    error_logs = options.get('error_logs', [None]*len(botcmds))

    capture_errors = options.get('capture_errors', False)

    turns = int(options['turns'])
    loadtime = float(options['loadtime']) / 1000
    turntime = float(options['turntime']) / 1000
    strict = options.get('strict', False)
    end_wait = options.get('end_wait', 0.0)

    location = options.get('location', 'localhost')
    game_id = options.get('game_id', 0)

    error = ''

    bots = []
    bot_status = []
    if capture_errors:
        error_logs = [Head(log) for log in error_logs]
    try:
        # create bot sandboxes
        for b, bot in enumerate(botcmds):
            bot_cwd, bot_cmd = bot
            sandbox = Sandbox(bot_cwd, secure=options.get('secure_jail', None))
            sandbox.start(bot_cmd)
            bots.append(sandbox)
            bot_status.append('survived')

            # ensure it started
            if not sandbox.is_alive:
                bot_status[-1] = 'crashed 0'
                if verbose_log:
                    verbose_log.write('bot %s did not start\n' % b)
                game.kill_player(b)
            sandbox.pause()

        if stream_log:
            stream_log.write(game.get_player_start())
            stream_log.flush()

        if verbose_log:
            verbose_log.write('running for %s turns\n' % turns)
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
                if options.get('serial', False):
                    simul_num = int(options['serial']) # int(True) is 1
                    bot_moves = [[] for b in bots]
                    error_lines = [[] for b in bots]
                    statuses = [None for b in bots]
                    bot_list = list(enumerate(bots))
                    random.shuffle(bot_list)
                    for group_num in range(0, len(bot_list), simul_num):
                        pnums, pbots = zip(*bot_list[group_num:group_num + simul_num])
                        moves, errors, status = get_moves(game, pbots, pnums,
                                time_limit, turn)
                        for p, b in enumerate(pnums):
                            bot_moves[b] = moves[p]
                            error_lines[b] = errors[p]
                            statuses[b] = status[p]
                else:
                    bot_moves, error_lines, statuses = get_moves(game, bots, range(len(bots)), time_limit, turn)

                # handle any logs that get_moves produced
                for b, errors in enumerate(error_lines):
                    if errors:
                        if error_logs and error_logs[b]:
                            error_logs[b].write('\n'.join(errors)+'\n')
                # set status for timeouts and crashes
                for b, status in enumerate(statuses):
                    if status != None:
                        bot_status[b] = status

                # process all moves
                bot_alive = [game.is_alive(b) for b in range(len(bots))]
                if turn > 0 and not game.game_over():
                    for b, moves in enumerate(bot_moves):
                        if game.is_alive(b):
                            valid, ignored, invalid = game.do_moves(b, moves)
                            if output_logs and output_logs[b]:
                                output_logs[b].write('# turn %s\n' % turn)
                                if valid:
                                    if output_logs and output_logs[b]:
                                        output_logs[b].write('\n'.join(valid)+'\n')
                                        output_logs[b].flush()
                            if ignored:
                                if error_logs and error_logs[b]:
                                    error_logs[b].write('turn %4d bot %s ignored actions:\n' % (turn, b))
                                    error_logs[b].write('\n'.join(ignored)+'\n')
                                    error_logs[b].flush()
                                if output_logs and output_logs[b]:
                                    output_logs[b].write('\n'.join(ignored)+'\n')
                                    output_logs[b].flush()
                            if invalid:
                                if strict:
                                    game.kill_player(b)
                                    bot_status[b] = 'invalid'
                                if error_logs and error_logs[b]:
                                    error_logs[b].write('turn %4d bot %s invalid actions:\n' % (turn, b))
                                    error_logs[b].write('\n'.join(invalid)+'\n')
                                    error_logs[b].flush()
                                if output_logs and output_logs[b]:
                                    output_logs[b].write('\n'.join(invalid)+'\n')
                                    output_logs[b].flush()

                    game.finish_turn()

                # send ending info to eliminated bots
                bots_eliminated = []
                for b, alive in enumerate(bot_alive):
                    if alive and not game.is_alive(b):
                        bots_eliminated.append(b)
                for b in bots_eliminated:
                    if verbose_log:
                        verbose_log.write('turn %4d bot %s eliminated\n' % (turn, b))
                    if bot_status[b] == 'survived': # could be invalid move
                        bot_status[b] = 'eliminated'
                    score_line ='score %s\n' % ' '.join([str(s) for s in game.get_scores(b)])
                    status_line = 'status %s\n' % ' '.join(map(str, game.order_for_player(b, bot_status)))
                    end_line = 'end\nplayers %s\n' % len(bots) + score_line + status_line
                    state = end_line + game.get_player_state(b) + 'go\n'
                    bots[b].write(state)
                    if input_logs and input_logs[b]:
                        input_logs[b].write(state)
                        input_logs[b].flush()
                    if end_wait:
                        bots[b].resume()
                if bots_eliminated and end_wait:
                    if verbose_log:
                        verbose_log.write('waiting {0} seconds for bots to process end turn\n'.format(end_wait))
                    time.sleep(end_wait)
                    for b in bots_eliminated:
                        bots[b].pause()

            except:
                raise

            if verbose_log:
                stats = game.get_stats()
                s = 'turn %4d stats: ' % turn
                for key, values in stats.items():
                    s += '%s: %s' % (key, values)
                verbose_log.write('%-50s\n' % s)

            alive = [game.is_alive(b) for b in range(len(bots))]
            if sum(alive) <= 1:
                break

        # send bots final state and score, output to replay file
        game.finish_game()
        score_line ='score %s\n' % ' '.join(map(str, game.get_scores()))
        status_line = 'status %s\n' % ' '.join(bot_status)
        end_line = 'end\nplayers %s\n' % len(bots) + score_line + status_line
        if stream_log:
            stream_log.write(end_line)
            stream_log.write(game.get_state())
            stream_log.flush()
        if verbose_log:
            verbose_log.write(score_line)
            verbose_log.write(status_line)
            verbose_log.flush()
        for b, bot in enumerate(bots):
            if game.is_alive(b):
                score_line ='score %s\n' % ' '.join([str(s) for s in game.get_scores(b)])
                status_line = 'status %s\n' % ' '.join(map(str, game.order_for_player(b, bot_status)))
                end_line = 'end\nplayers %s\n' % len(bots) + score_line + status_line
                state = end_line + game.get_player_state(b) + 'go\n'
                bot.write(state)
                if input_logs and input_logs[b]:
                    input_logs[b].write(state)
                    input_logs[b].flush()

    except Exception as e:
        # TODO: sanitize error output, tracebacks shouldn't be sent to workers
        error = traceback.format_exc()
        if verbose_log:
            verbose_log.write(traceback.format_exc())
        error = str(e)
    finally:
        if end_wait:
            for bot in bots:
                bot.resume()
            if verbose_log:
                verbose_log.write('waiting {0} seconds for bots to process end turn\n'.format(end_wait))
            time.sleep(end_wait)
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
            'game_id': game_id,
            'player_info': [{} for x in range(len(bots))],
            'status': bot_status,
            'score': scores,
            'rank': [sorted(set(scores), reverse=True).index(x) for x in scores],
            'replayformat': 'json',
            'replaydata': game.get_replay(),
        }
        if capture_errors:
            game_result['errors'] = [head.head() for head in error_logs]

    if replay_log:
        json.dump(game_result, replay_log, sort_keys=True)

    return game_result

def get_moves(game, bots, bot_nums, time_limit, turn):
    bot_finished = [not game.is_alive(bot_nums[b]) for b in range(len(bots))]
    bot_moves = [[] for b in bots]
    error_lines = [[] for b in bots]
    statuses = [None for b in bots]
    start_time = time.time()

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
                error_lines[b].append('turn %4d bot %s crashed' % (turn, bot_nums[b]))
                statuses[b] = 'crashed'
                line = bot.read_error()
                while line != None:
                    error_lines[b].append(line)
                    line = bot.read_error()
                bot_finished[b] = True
                game.kill_player(bot_nums[b])
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

            for x in range(100):
                line = bot.read_error()
                if line is None:
                    break
                error_lines[b].append(line)
    # pause all bots again
    for bot in bots:
        if bot.is_alive:
            bot.pause()

    # kill timed out bots
    for b, finished in enumerate(bot_finished):
        if not finished:
            error_lines[b].append('turn %4d bot %s timed out' % (turn, bot_nums[b]))
            statuses[b] = 'timeout'
            game.kill_player(bot_nums[b])
            bots[b].kill()

    return bot_moves, error_lines, statuses

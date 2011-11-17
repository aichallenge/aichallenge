#!/usr/bin/env python
from __future__ import print_function
import time
import traceback
import os
import random
import sys
import json
import io
if sys.version_info >= (3,):
    def unicode(s):
        return s

from sandbox import get_sandbox

class HeadTail(object):
    'Capture first part of file write and discard remainder'
    def __init__(self, file, max_capture=510):
        self.file = file
        self.max_capture = max_capture
        self.capture_head_len = 0
        self.capture_head = unicode('')
        self.capture_tail = unicode('')
    def write(self, data):
        if self.file:
            self.file.write(data)
        capture_head_left = self.max_capture - self.capture_head_len
        if capture_head_left > 0:
            data_len = len(data)
            if data_len <= capture_head_left:
                self.capture_head += data
                self.capture_head_len += data_len
            else:
                self.capture_head += data[:capture_head_left]
                self.capture_head_len = self.max_capture
                self.capture_tail += data[capture_head_left:]
                self.capture_tail = self.capture_tail[-self.max_capture:]
        else:
            self.capture_tail += data
            self.capture_tail = self.capture_tail[-self.max_capture:]
    def flush(self):
        if self.file:
            self.file.flush()
    def close(self):
        if self.file:
            self.file.close()
    def head(self):
        return self.capture_head
    def tail(self):
        return self.capture_tail
    def headtail(self):
        if self.capture_head != '' and self.capture_tail != '':
            sep = unicode('\n..\n')
        else:
            sep = unicode('')
        return self.capture_head + sep + self.capture_tail

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
    capture_errors_max = options.get('capture_errors_max', 510)

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
    bot_turns = []
    if capture_errors:
        error_logs = [HeadTail(log, capture_errors_max) for log in error_logs]
    try:
        # create bot sandboxes
        for b, bot in enumerate(botcmds):
            bot_cwd, bot_cmd = bot
            sandbox = get_sandbox(bot_cwd,
                    secure=options.get('secure_jail', None))
            sandbox.start(bot_cmd)
            bots.append(sandbox)
            bot_status.append('survived')
            bot_turns.append(0)

            # ensure it started
            if not sandbox.is_alive:
                bot_status[-1] = 'crashed 0'
                bot_turns[-1] = 0
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
            if turn == 0:
                game.start_game()

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
                        bot_turns[b] = turn

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
            else:
                simul_num = len(bots)

            bot_moves = [[] for b in bots]
            error_lines = [[] for b in bots]
            statuses = [None for b in bots]
            bot_list = [(b, bot) for b, bot in enumerate(bots)
                        if game.is_alive(b)]
            random.shuffle(bot_list)
            for group_num in range(0, len(bot_list), simul_num):
                pnums, pbots = zip(*bot_list[group_num:group_num + simul_num])
                moves, errors, status = get_moves(game, pbots, pnums,
                        time_limit, turn)
                for p, b in enumerate(pnums):
                    bot_moves[b] = moves[p]
                    error_lines[b] = errors[p]
                    statuses[b] = status[p]

            # handle any logs that get_moves produced
            for b, errors in enumerate(error_lines):
                if errors:
                    if error_logs and error_logs[b]:
                        error_logs[b].write(unicode('\n').join(errors)+unicode('\n'))
            # set status for timeouts and crashes
            for b, status in enumerate(statuses):
                if status != None:
                    bot_status[b] = status
                    bot_turns[b] = turn

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
                                bot_turns[b] = turn
                            if error_logs and error_logs[b]:
                                error_logs[b].write('turn %4d bot %s invalid actions:\n' % (turn, b))
                                error_logs[b].write('\n'.join(invalid)+'\n')
                                error_logs[b].flush()
                            if output_logs and output_logs[b]:
                                output_logs[b].write('\n'.join(invalid)+'\n')
                                output_logs[b].flush()

            if turn > 0:
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
                    bot_turns[b] = turn
                score_line ='score %s\n' % ' '.join([str(s) for s in game.get_scores(b)])
                status_line = 'status %s\n' % ' '.join(map(str, game.order_for_player(b, bot_status)))
                status_line += 'playerturns %s\n' % ' '.join(map(str, game.order_for_player(b, bot_turns)))
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
                bots[b].kill()

            if verbose_log:
                stats = game.get_stats()
                stat_keys = sorted(stats.keys())
                s = 'turn %4d stats: ' % turn
                if turn % 50 == 0:
                    verbose_log.write(' '*len(s))
                    for key in stat_keys:
                        values = stats[key]
                        verbose_log.write(' {0:^{1}}'.format(key, max(len(key), len(str(values)))))
                    verbose_log.write('\n')
                verbose_log.write(s)
                for key in stat_keys:
                    values = stats[key]
                    if type(values) == list:
                        values = '[' + ','.join(map(str,values)) + ']'
                    verbose_log.write(' {0:^{1}}'.format(values, max(len(key), len(str(values)))))
                verbose_log.write('\n')

            #alive = [game.is_alive(b) for b in range(len(bots))]
            #if sum(alive) <= 1:
            if game.game_over():
                break

        # send bots final state and score, output to replay file
        game.finish_game()
        score_line ='score %s\n' % ' '.join(map(str, game.get_scores()))
        status_line = 'status %s\n' % ' '.join(bot_status)
        status_line += 'playerturns %s\n' % ' '.join(map(str, bot_turns))
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
                status_line += 'playerturns %s\n' % ' '.join(map(str, game.order_for_player(b, bot_turns)))
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
        # error = str(e)
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
            'status': bot_status,
            'playerturns': bot_turns,
            'score': scores,
            'rank': [sorted(scores, reverse=True).index(x) for x in scores],
            'replayformat': 'json',
            'replaydata': game.get_replay(),
            'game_length': turn
        }
        if capture_errors:
            game_result['errors'] = [head.headtail() for head in error_logs]

    if replay_log:
        json.dump(game_result, replay_log, sort_keys=True)

    return game_result

def get_moves(game, bots, bot_nums, time_limit, turn):
    bot_finished = [not game.is_alive(bot_nums[b]) for b in range(len(bots))]
    bot_moves = [[] for b in bots]
    error_lines = [[] for b in bots]
    statuses = [None for b in bots]

    # resume all bots
    for bot in bots:
        if bot.is_alive:
            bot.resume()

    # don't start timing until the bots are started
    start_time = time.time()

    # loop until received all bots send moves or are dead
    #   or when time is up
    while (sum(bot_finished) < len(bot_finished) and
            time.time() - start_time < time_limit):
        time.sleep(0.01)
        for b, bot in enumerate(bots):
            if bot_finished[b]:
                continue # already got bot moves
            if not bot.is_alive:
                error_lines[b].append(unicode('turn %4d bot %s crashed') % (turn, bot_nums[b]))
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

    # check for any final output from bots
    for b, bot in enumerate(bots):
        if bot_finished[b]:
            continue # already got bot moves
        if not bot.is_alive:
            error_lines[b].append(unicode('turn %4d bot %s crashed') % (turn, bot_nums[b]))
            statuses[b] = 'crashed'
            line = bot.read_error()
            while line != None:
                error_lines[b].append(line)
                line = bot.read_error()
            bot_finished[b] = True
            game.kill_player(bot_nums[b])
            continue # bot is dead

        line = bot.read_line()
        while line is not None and len(bot_moves[b]) < 40000:
            line = line.strip()
            if line.lower() == 'go':
                bot_finished[b] = True
                # bot finished sending data for this turn
                break
            bot_moves[b].append(line)
            line = bot.read_line()

        line = bot.read_error()
        while line is not None and len(error_lines[b]) < 1000:
            error_lines[b].append(line)
            line = bot.read_error()

    # kill timed out bots
    for b, finished in enumerate(bot_finished):
        if not finished:
            error_lines[b].append(unicode('turn %4d bot %s timed out') % (turn, bot_nums[b]))
            statuses[b] = 'timeout'
            bot = bots[b]
            for x in range(100):
                line = bot.read_error()
                if line is None:
                    break
                error_lines[b].append(line)
            game.kill_player(bot_nums[b])
            bots[b].kill()

    return bot_moves, error_lines, statuses

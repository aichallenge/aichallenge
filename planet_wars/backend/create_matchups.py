#!/usr/bin/python

import logging
import logging.handlers
import math
import os
import random
import sys
import time
from collections import defaultdict
from datetime import datetime, timedelta

import MySQLdb
from server_info import server_info

logger = logging.getLogger('compile_logger')
logger.setLevel(logging.INFO)
_my_pid = os.getpid()
def log_message(message):
  logger.info(str(_my_pid) + ": " + message)
  print message

def get_submissions(cursor):
    cursor.execute("""SELECT leaderboard_id, timestamp FROM leaderboards
        WHERE leaderboard_id = (SELECT MAX(leaderboard_id)
            from leaderboards)""")
    row = cursor.fetchone()
    leaderboard_id = row['leaderboard_id']
    log_message("latest leaderboard is %d" % leaderboard_id)
    # check that the latest leaderboard is complete else use the previous one
    count_query = "SELECT count(*) FROM rankings where leaderboard_id = %s"
    cursor.execute(count_query % (leaderboard_id,))
    cur_num = cursor.fetchone()['count(*)']
    cursor.execute(count_query % (leaderboard_id-1,))
    prev_num = cursor.fetchone()['count(*)']
    if prev_num * 0.99 > cur_num:
        leaderboard_id -= 1
        log_message("using previous leaderboard %d as current has %d less"
                % (leaderboard_id, prev_num - cur_num,))
    cursor.execute("SELECT * FROM matchups")
    pending = []
    for row in cursor.fetchall():
        pending.append(str(row['player_one']))
        pending.append(str(row['player_two']))
    if pending:
        pending_cond = "AND s.submission_id NOT IN (" + ",".join(pending) + ")"
    else:
        pending_cond = ''
    cursor.execute("""SELECT s.*, r.rank as rank
        FROM submissions as s
        LEFT OUTER JOIN rankings as r ON r.submission_id = s.submission_id
            AND r.leaderboard_id = %s
        WHERE s.latest = 1 %s
        ORDER BY rank""" % (leaderboard_id, pending_cond))
    ranked = []
    unranked = []
    for row in cursor.fetchall():
        if row['rank'] is None:
            unranked.append(row)
        else:
            ranked.append(row)
    log_message("Got %d ranked and %d unranked active submissions"
            % (len(ranked), len(unranked)))
    return (ranked, unranked)

def get_total_ranking(cursor, ranked, unranked):
    ranking = defaultdict(list)
    for sub in ranked:
        ranking[sub['rank']].append(sub)
    no_previous = []
    for sub in unranked:
        log_message("Finding provisional rank for submission %d"
                % sub['submission_id'])
        old_rank = None
        cursor.execute("""SELECT submission_id FROM submissions
                WHERE status = 40 AND user_id = %s AND submission_id != %s
                ORDER BY submission_id DESC
            """ % (sub['user_id'], sub['submission_id']))
        log_message(" has %d old submissions" % (cursor.rowcount,))
        old_subs = cursor.fetchall()
        for old_sub in old_subs:
            cursor.execute("""SELECT leaderboard_id, rank FROM rankings
                WHERE submission_id = %d
                ORDER BY leaderboard_id DESC
                LIMIT 1""" % (old_sub['submission_id'],))
            row = cursor.fetchone()
            if row:
                old_rank = row['rank']
                break
        if old_rank is not None:
            ranking[row['rank']].append(sub)
            log_message("  set rank to %d" % (row['rank'],))
        else:
            no_previous.append(sub)
            log_message("  could not find a previous rank")
    if no_previous:
        middle_rank = max(ranking.keys()) / 2
        log_message("Setting %d submissions with no provisional rank to %d"
                % (len(no_previous), middle_rank))
        ranking[middle_rank] += no_previous
    total_ranking = []
    ranking = ranking.items()
    ranking.sort()
    for rank, tier in ranking:
        for sub in tier:
            total_ranking.append(sub)
    return total_ranking

def get_player_one_order(total_ranking):
    # this will break if last game can be over a year ago
    def sort_key(sub):
        if sub['last_game_timestamp'] is not None:
            return sub['last_game_timestamp']
        return sub['timestamp'] - timedelta(days=365)
    subs = list(total_ranking)
    subs.sort(key=sort_key, reverse=True)
    return subs

def choose_opponent(p1, ranking):
    if p1['last_game_timestamp'] == None:
        ranking = [s for s in ranking
                if s == p1 or s['last_game_timestamp'] != None]
    p1_ix = ranking.index(p1)
    p2_ix = p1_ix
    while p1_ix == p2_ix:
        offset = random.paretovariate(0.5)
        offset = int(offset if random.randint(0,1) else 0-offset)
        ix = p1_ix + offset
        if ix < 0 or ix > len(ranking) - 1:
            continue
        p2_ix = ix
    return ranking[p2_ix]

_SERVER_MAPS = None
def choose_map(cursor, player1, player2):
    global _SERVER_MAPS
    if not _SERVER_MAPS:
        cursor.execute("SELECT map_id, priority FROM maps")
        _SERVER_MAPS = {}
        for row in cursor.fetchall():
            if row['priority'] > 0:
                _SERVER_MAPS[row['map_id']] = row['priority']
    p1_id = player1['submission_id']
    p2_id = player2['submission_id']
    cursor.execute("""SELECT map_id FROM games
        WHERE (player_one = %s AND player_two = %s)
            OR (player_one = %s AND player_two = %s)""" %
            (p1_id, p2_id, p2_id, p1_id))
    counts = dict()
    for map_id in _SERVER_MAPS.keys():
        counts[map_id] = 0
    for row in cursor.fetchall():
        try:
            counts[row['map_id']] += 1
        except KeyError:
            # there are a large number of games recorded in the contest
            # database with a bad map id
            pass
    min_maps, min_played = counts.popitem()
    min_maps = [min_maps]
    for map_id, plays in counts.items():
        if plays < min_played:
            min_played = plays
            min_maps = [map_id]
        elif plays == min_played:
            min_maps.append(map_id)
    qualified_maps = [min_maps.pop()]
    high_priority = _SERVER_MAPS[qualified_maps[0]]
    for map_id in min_maps:
        if _SERVER_MAPS[map_id] < high_priority:
            high_priority = _SERVER_MAPS[map_id]
            qualified_maps = [map_id]
        elif _SERVER_MAPS[map_id] == high_priority:
            qualified_maps.append(map_id)
    return random.choice(qualified_maps)

def add_matches(cursor, max_matches):
    ranked, unranked = get_submissions(cursor)
    total_ranking = get_total_ranking(cursor, ranked, unranked)
    player_order = get_player_one_order(total_ranking)
    num_matches = 0
    while len(total_ranking) > 1 and num_matches < max_matches:
        p1 = player_order.pop()
        p2 = choose_opponent(p1, total_ranking)
        m = choose_map(cursor, p1, p2)
        cursor.execute("""INSERT matchups
                SET player_one=%s, player_two=%s, map_id=%s"""
                    % (p1['submission_id'], p2['submission_id'], m))
        log_message("%s plays %s on %s(%d)" % (
                p1['submission_id'], p2['submission_id'], m, _SERVER_MAPS[m]))
        total_ranking.remove(p1)
        total_ranking.remove(p2)
        player_order.remove(p2)
        num_matches += 1
    return num_matches

def main(run_time=0, qbuffer=6):
    start_time = time.time()
    try:
        handler = logging.handlers.RotatingFileHandler("matchup.log",
                                               maxBytes=1000000,
                                               backupCount=5)
        logger.addHandler(handler)
    except IOError:
       # couldn't start the file logger
       pass
    while True:
        connection = MySQLdb.connect(host = server_info["db_host"],
                                     user = server_info["db_username"],
                                     passwd = server_info["db_password"],
                                     db = server_info["db_name"])
        cursor = connection.cursor(MySQLdb.cursors.DictCursor)
        cursor.execute("""SELECT count(*)/5 as gpm FROM games
                WHERE timestamp > (NOW() - INTERVAL 5 minute)""")
        gpm = max(float(cursor.fetchone()['gpm']), 5.)
        cursor.execute("SELECT count(*) FROM matchups WHERE dispatch_time IS NULL")
        queue_size = cursor.fetchone()['count(*)']
        log_message("Found %d matches in queue with %d gpm" % (queue_size, gpm))
        if queue_size < gpm * 2:
            start_adding = time.time()
            num_added = add_matches(cursor, (gpm * qbuffer) - queue_size)
            log_message("Added %d new matches to queue in %.2f seconds"
                    % (num_added, time.time()-start_adding))
        cursor.close()
        connection.close()
        if time.time() - start_time >= run_time - 32:
            break
        buffertime = max(1, ((queue_size / float(gpm)) - 2.1) * 60)
        time.sleep(min(30, buffertime))

if __name__ == '__main__':
    runtime = 0
    qbuffer = 6
    if len(sys.argv) > 1:
        runtime = int(sys.argv[1])
    if len(sys.argv) > 2:
        qbuffer = float(sys.argv[2])
    main(runtime, qbuffer)


#!/usr/bin/python

import argparse
import logging
import logging.handlers
import os
import sys
from os.path import basename, splitext

import MySQLdb
from server_info import server_info

# require this many games to be played before adjusting the turn limit
MIN_GAMES_PLAYED = 100

# percentage of games hitting the turn limit before increasing the limit
LOW_MARK_PER = 0.9

# percentage of games hitting the turn limit before decreasing the limit
HIGH_MARK_PER = 0.95

# when decreasing the limit set the new limit so it would include
# this percentage of games (should be between the high and low marks above)
DEC_PER = 0.925

# when increasing the limit increase by this multiple
INC_MULTIPLE = 1.2

# never increase the turn limit beyond this
HARD_LIMIT = 1500

log = logging.getLogger('turn_adjuster')
log.setLevel(logging.INFO)

formatter = logging.Formatter("%(asctime)s - " + str(os.getpid()) +
                              " - %(levelname)s - %(message)s")

log_file = os.path.join(server_info['logs_path'], 'turn_adjuster.log')
try:
    handler = logging.handlers.RotatingFileHandler(log_file,
                                                   maxBytes=1000000,
                                                   backupCount=5)
    handler.setLevel(logging.INFO)
    handler.setFormatter(formatter)
    log.addHandler(handler)
except IOError:
    # ignore errors about file permissions
    pass

handler2 = logging.StreamHandler(sys.stdout)
handler2.setLevel(logging.INFO)
handler2.setFormatter(formatter)
log.addHandler(handler2)


def main(dry_run):
    connection = MySQLdb.connect(host = server_info["db_host"],
                                     user = server_info["db_username"],
                                     passwd = server_info["db_password"],
                                     db = server_info["db_name"])
    cursor = connection.cursor()

    cursor.execute("""select map_id, filename, max_turns, timestamp from map
            where priority >= 0""")
    maplist = cursor.fetchall()
    for map_id, map_name, map_limit, last_adjusted in maplist:
        map_name = splitext(basename(map_name))[0]
        cursor.execute("""select game_length from game
            where map_id = '%s' and timestamp > '%s' and cutoff = 0"""
            % (map_id, last_adjusted))
        game_lengths = [n for (n,) in cursor.fetchall()]
        games_played = len(game_lengths)
        if games_played < MIN_GAMES_PLAYED:
            continue
        log.info("Checking map %d (%s) limit currently %d with %d games played"
                % (map_id, map_name, map_limit, games_played))
        game_lengths.sort()

        lowmark_ix = max(int(round(LOW_MARK_PER * games_played)) - 1, 0)
        highmark_ix = max(int(round(HIGH_MARK_PER * games_played)) - 1, 0)
        lowmark_len = game_lengths[lowmark_ix]
        highmark_len = game_lengths[highmark_ix]

        new_limit = None
        if lowmark_len == map_limit:
            new_limit = min(map_limit * INC_MULTIPLE, HARD_LIMIT)
        elif highmark_len < map_limit:
            new_limit = game_lengths[int(round(DEC_PER * games_played)) - 1]
        if new_limit:
            log.info("Adjusting map %d turn limit from %d to %d"
                    % (map_id, map_limit, new_limit))
            if not dry_run:
                cursor.execute("""update map
                    set max_turns = '%s', timestamp = NOW()
                    where map_id = '%s'""" % (new_limit, map_id))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-s", "--simulate", default=False, action="store_true",
            help="Don't store any adjustments back to the database")
    args = parser.parse_args()
    main(dry_run=args.simulate)


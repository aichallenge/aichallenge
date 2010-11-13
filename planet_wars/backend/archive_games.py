#!/usr/bin/python

import logging
import logging.handlers
import os
import time
import sys

import MySQLdb
from server_info import server_info

logger = logging.getLogger('compile_logger')
logger.setLevel(logging.INFO)
_my_pid = os.getpid()
def log_message(message):
  logger.info(str(_my_pid) + ": " + message)
  print message

def main(max_games=10000):
    start_time = time.time()
    try:
        handler = logging.handlers.RotatingFileHandler("archive_games.log",
                                               maxBytes=1000000,
                                               backupCount=5)
        logger.addHandler(handler)
    except IOError:
       # couldn't start the file logger
       pass
    connection = MySQLdb.connect(host = server_info["db_host"],
                                     user = server_info["db_username"],
                                     passwd = server_info["db_password"],
                                     db = server_info["db_name"])
    cursor = connection.cursor(MySQLdb.cursors.DictCursor)
    cursor.execute("""SELECT min(player_one) as p1, min(player_two) as p2
            from games""")
    sub_mins = cursor.fetchone()
    cursor.execute("""INSERT INTO games_archive
        SELECT * FROM games
            WHERE player_one in (SELECT submission_id FROM submissions
                    WHERE latest = 0 and submission_id >= %d)
                OR player_two in (SELECT submission_id FROM submissions
                    WHERE latest = 0 and submission_id >= %d)
            ORDER BY game_id LIMIT %d"""
            % (sub_mins['p1'], sub_mins['p2'], max_games))
    log_message("copied %d old games in %.2f seconds"
            % (cursor.rowcount, time.time()-start_time))
    time.sleep(1)
    cursor.execute("""DELETE FROM games WHERE game_id in (
            SELECT game_id FROM games_archive)""")
    log_message("removed %d old games from primary table" % (cursor.rowcount,))
    log_message("total runtime %.2f seconds" % (time.time()-start_time,))

if __name__ == '__main__':
    if len(sys.argv) > 1:
        main(int(sys.argv[1]))
    else:
        main()


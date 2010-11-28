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
    cursor.execute("""INSERT INTO games_archive
        SELECT g.* FROM games g LEFT JOIN games_archive ga
            ON g.game_id = ga.game_id
            WHERE ga.game_id IS NULL
            LIMIT %d""" % (max_games,))
    copied = cursor.rowcount
    log_message("copied %d old games in %.2f seconds"
            % (copied, time.time()-start_time))
    if copied < max_games:
        time.sleep(1)
        del_start = time.time()
        max_games -= copied
        cursor.execute("""DELETE QUICK FROM games
                LIMIT %d""" % (max_games,))
        log_message("removed %d old games from primary table in %.2f seconds"
                % (cursor.rowcount, time.time() - del_start))
    log_message("total runtime %.2f seconds" % (time.time()-start_time,))

if __name__ == '__main__':
    if len(sys.argv) > 1:
        main(int(sys.argv[1]))
    else:
        main()


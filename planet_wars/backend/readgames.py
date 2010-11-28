#!/usr/bin/python

import os
import tempfile
import time

import MySQLdb
from server_info import server_info

# Connect to the database.
connection = MySQLdb.connect(host = server_info["db_host"],
                             user = server_info["db_username"],
                             passwd = server_info["db_password"],
                             db = server_info["db_name"])
cursor = connection.cursor(MySQLdb.cursors.DictCursor)
cursor.execute("""
SELECT g.game_id, g.winner, g.draw,
    u1.user_id as u1_id, u1.username as u1_name, p1.submission_id as p1_id,
    u2.user_id as u2_id, u2.username as u2_name, p2.submission_id as p2_id
  FROM games g LEFT JOIN (submissions p1, submissions p2,
    users u1, users u2)
  ON (g.player_one = p1.submission_id AND g.player_two = p2.submission_id
    AND p1.user_id = u1.user_id AND p2.user_id = u2.user_id)
  WHERE p1.latest = 1 AND p1.status = 40
    AND p2.latest = 1 AND p2.status = 40""")


pgn_fd, tmp_name = tempfile.mkstemp(dir=".")
pgn = os.fdopen(pgn_fd, 'w')

for row in cursor:
    pgn.write('[White "%s,%s,%s"]\n' % (row['u1_id'], row['u1_name'],
        row['p1_id']))
    pgn.write('[Black "%s,%s,%s"]\n' % (row['u2_id'], row['u2_name'],
        row['p2_id']))
    pgn.write('[GameID "%s"]\n' % (row['game_id'],))
    if row['draw']:
        res = '1/2-1/2'
    elif row['winner'] == row['p1_id']:
        res = '1-0'
    elif row['winner'] == row['p2_id']:
        res = '0-1'
    else:
        raise ValueError("Winner is not player 1 or 2 game_id "
                + row['g.game_id'])
    pgn.write('[Result "%s"]\n%s\n\n' % (res, res))

pgn.close()
pgn_name = "games-%s.pgn" % (time.strftime("%Y-%m-%d-%H%M"))
os.rename(tmp_name, pgn_name)
os.chmod(pgn_name, 0644)

cursor.close()
connection.close()

print "readpgn", pgn_name
print "elo"
print " mm 0 1"
print " ratings >ratings.txt"
print " x"
print "x"


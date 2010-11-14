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
  SELECT *
  FROM submissions s,
  users u,
  (
    SELECT
    user_id,
    MAX(submission_id) as maxid
    FROM submissions
    GROUP BY user_id
  ) AS x
  WHERE s.user_id = u.user_id
  AND s.user_id = x.user_id
  AND submission_id = x.maxid
  AND status = 40;
""")

players = {}
for row in cursor:
    s_id = row['submission_id']
    players[s_id] = "%s,%s,%s" % (row['user_id'], row['username'], s_id)

t = ','.join(str(k) for k in players.keys())
cursor.execute("""
  SELECT game_id, winner, loser, map_id, draw, timestamp, player_one, player_two
  FROM games
  WHERE player_one IN (%s) AND player_two IN (%s)
  AND timestamp >= timestampadd(DAY, -10, current_timestamp)""" % (t, t))

pgn_fd, tmp_name = tempfile.mkstemp(dir=".")
pgn = os.fdopen(pgn_fd, 'w')

for row in cursor:
    pgn.write('[White "%s"]\n' % (players[row['player_one']],))
    pgn.write('[Black "%s"]\n' % (players[row['player_two']],))
    pgn.write('[GameID "%s"]\n' % (row['game_id'],))
    if row['draw']:
        res = '1/2-1/2'
    elif row['winner'] == row['player_one']:
        res = '1-0'
    elif row['winner'] == row['player_two']:
        res = '0-1'
    else:
        raise ValueError("Winner is not player 1 or 2 game_id "+ row['game_id'])
    pgn.write('[Result "%s"]\n%s\n\n' % (res, res))

pgn.close()
pgn_name = "games-%s.pgn" % (time.strftime("%Y-%m-%d-%H%M"))
os.rename(tmp_name, pgn_name)
os.chmod(pgn_name, 0644)

cursor.close()
connection.close()

print "readpgn", pgn_name
print "elo"
print " mm"
print " ratings >ratings.txt"
print " x"
print "x"


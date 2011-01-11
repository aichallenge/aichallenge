#!/usr/bin/python

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
subs = {}
for row in cursor:
	del row['password']
	del row['email']
	del row['activation_code']
	subs[row['submission_id']] = row

t = ','.join(str(k) for k in subs)
cursor.execute("""
  SELECT game_id, winner, loser, map_id, draw, timestamp, player_one, player_two
  FROM games
  WHERE player_one IN (""" + str(t) + """)
  AND player_two IN (""" + str(t) + """)
  AND timestamp >= timestampadd(DAY, -3, current_timestamp)
""")

games = {}
players = set()
for row in cursor:
  if row['player_one'] not in subs or row['player_two'] not in subs:
    continue
  players.add(row['player_one'])
  players.add(row['player_two'])
  if row['draw']:
    res = (row['player_one'], row['player_two'], 1)
  else:
    res = (row['winner'], row['loser'], 2)
  games[row['game_id']] = res

cursor.close()
connection.close()

players = sorted(players)
playerlookup = {}
for i, p in enumerate(players):
  print "addplayer", "%s,%s,%d"%(subs[p]['user_id'], subs[p]['username'], p)
  playerlookup[p] = i

for g in sorted(games.keys()):
  w,l,r = games[g]
  print "addresult", playerlookup[w], playerlookup[l], r

print "elo"
print " mm"
# print " exactdist" # we don't really need this
print " ratings >ratings.txt"
print " x"
print "x"

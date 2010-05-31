#!/usr/bin/python

import MySQLdb
from mysql_login import login

conn = MySQLdb.connect(**login)
cursor = conn.cursor(MySQLdb.cursors.DictCursor)

cursor.execute('''SELECT * FROM contest_submissions s,
				contest_users u, 
				(SELECT user_id, max(submission_id) as maxid from contest_submissions group by user_id) as x
		  where s.user_id = u.user_id and s.user_id = x.user_id and submission_id = x.maxid and status = 40;''')
subs = {}
for row in cursor:
	del row['password']
	del row['email']
	del row['activation_code']
	subs[row['submission_id']] = row

t = ','.join(str(k) for k in subs)
cursor.execute('SELECT * FROM contest_games WHERE winner IN (%s) and loser IN (%s) AND timestamp >= timestampadd(DAY, -3, current_timestamp) ORDER BY game_id DESC '%(t,t))# LIMIT 40000')

games = {}
players = set()
for row in cursor:
	if row['winner'] not in subs or row['loser'] not in subs:
		continue
	players.add(row['winner'])
	players.add(row['loser'])
	res = (row['winner'], row['loser'])
	if row['draw']:
		res += (1,)
	else:
		res += (2,)
	games[row['game_id']] = res

cursor.close()
conn.close()

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
print " ratings 10>ratings.txt"
print " x"
print "x"


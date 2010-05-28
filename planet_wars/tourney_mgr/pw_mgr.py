#!/usr/bin/python

# test_db.py - play with contest database

import MySQLdb
import subprocess
import random

db_file = open('db_stuff', 'r')
db_host = db_file.readline().strip()
db_name = db_file.readline().strip()
db_user = db_file.readline().strip()
db_pwd = db_file.readline().strip()


conn = MySQLdb.connect (host = db_host,
                           user = db_user,
                           passwd = db_pwd,
                           db = db_name)
cursor = conn.cursor ()
cursor.execute ("SELECT VERSION()")
row = cursor.fetchone ()
print "server version:", row[0]
usr_store={}
map_store={}
n_key = 1

# Fetch info from database
cursor.execute ("SELECT MAX(submission_id),user_id FROM submissions WHERE status=40 GROUP BY user_id")
while (1):
	row = cursor.fetchone ()
	if row == None:
		break
	usr_store[n_key] = (row[0],row[1])
	n_key = n_key + 1

num_players = cursor.rowcount
print "%d lines returned" % (num_players)

cursor.execute ("SELECT map_id, path FROM maps")
while (1):
	row = cursor.fetchone ()
	if row == None:
		break
	map_store[row[0]] = row[1] 

# Provisional random one
while(len(usr_store) > 1):
	# Select users
	first_usr = random.choice(usr_store.keys())
	second_usr = first_usr
	while (first_usr == second_usr):
		second_usr = random.choice(usr_store.keys())
	plyr_1 = usr_store[first_usr]
	plyr_2 = usr_store[second_usr]
	del usr_store[first_usr]
	del usr_store[second_usr]

	# Select map
	the_map = random.choice(map_store.keys())

	print "Pairing: %d/%d-%d/%d on map %d" % (plyr_1[0],plyr_1[1],plyr_2[0],plyr_2[1],the_map)

	# Call engine

	# Update db with results
	pb_str = ''
	db_str = "INSERT INTO contest.games (winner,loser,map_id,timestamp,player_one,player_two,playback_string) VALUES(\'%d\',\'%d\',\'%d\',NOW(),\'%d\',\'%d\',\'%s\')" % (plyr_1[0],plyr_2[0],the_map,plyr_1[0],plyr_2[0],pb_str) 
	cursor.execute (db_str)
	#print db_str

# Find user with fewest games played that hasn't already been picked

# Determine window

# Select random ranking in window

# Add to pairings

cursor.close ()
conn.close ()


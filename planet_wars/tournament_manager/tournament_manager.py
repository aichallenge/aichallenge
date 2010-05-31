import MySQLdb
import subprocess
import random
from server_info import server_info

# Connect to the database.
connection = MySQLdb.connect(host = server_info["db_host"],
                             user = server_info["db_username"],
                             passwd = server_info["db_password"],
                             db = server_info["db_name"])
cursor = connection.cursor(MySQLdb.cursors.DictCursor)

# Get the list of currently active submissions.
cursor.execute("""
  SELECT
    user_id,
    MAX(submission_id) AS submission_id
  FROM submissions
  WHERE status = 40
  GROUP BY user_id
""")
submissions = cursor.fetchall()

# Get the list of maps.
cursor.execute("SELECT * FROM maps")
maps = cursor.fetchall()

# Are there enough players? Are there enough maps?
if len(submissions) < 2:
  print "There are fewer than two active submissions. No games can be played."
  sys.exit(0)
if len(maps) < 1:
  print "There are no maps in the database. No games can be played."
  sys.exit(0)

# Choose two different players at random
map = random.choice(maps)
player_one = random.choice(submissions)
player_two = player_one
while player_one == player_two:
  player_two = random.choice(submissions)
print str(player_one["submission_id"]) + " vs " + \
  str(player_two["submission_id"]) + " on " + str(map["name"])

# Invoke the game engine
command = "~/ai-contest/planet_wars/engine/engine " + map["path"]
engine_process = subprocess.Popen(command, \
                                  stdout=subprocess.PIPE, \
                                  stderr=subprocess.PIPE)
(output, error) = p.communicate()

cursor.close()
connection.close()

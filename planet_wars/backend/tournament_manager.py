import MySQLdb
import subprocess
import random
from server_info import server_info
import engine

# Connect to the database.
connection = MySQLdb.connect(host = server_info["db_host"],
                             user = server_info["db_username"],
                             passwd = server_info["db_password"],
                             db = server_info["db_name"])
cursor = connection.cursor(MySQLdb.cursors.DictCursor)

# Get the list of currently active submissions.
cursor.execute("""
  SELECT s.*, l.*
  FROM submissions s
  INNER JOIN (
    SELECT
      MAX(submission_id) AS submission_id
    FROM submissions
    GROUP BY user_id
  ) AS o ON o.submission_id = s.submission_id
  INNER JOIN languages l ON l.language_id = s.language_id
  WHERE s.status = 40
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
#command = [
#  "../engine/engine", # Path to the engine
#  "../maps/" + map["path"], # The map file
#  "1000", # Max turn time in milliseconds
#  "100", # Max game length in turns
#  "../submissions/" + str(player_one["submission_id"]), # Player one directory
#  player_one["command"], # Player one command
#  "../submissions/" + str(player_two["submission_id"]), # Player two directory
#  player_two["command"] # Player two command
#]
#(output, stderr) = subprocess.Popen(command, \
#                                    stdout=subprocess.PIPE, \
#                                    stderr=subprocess.PIPE).communicate()
#result = dict()
#for line in output.split("\n"):
#  (key, colon, value) = line.partition(":")
#  key = key.strip()
#  value = value.strip()
#  if key != "" and value != "":
#    result[key] = value
player_one_path = "../submissions/" + str(player_one["submission_id"])
player_two_path = "../submissions/" + str(player_two["submission_id"])
map_path = "../maps/" + map["path"]
players = [
  {"path" : player_one_path, "command" : player_one["command"]},
  {"path" : player_two_path, "command" : player_two["command"]}
]
outcome = engine.play_game(map_path, 1000, 10, players)

# Store the game outcome in the database
winner = "NULL"
loser = "NULL"
map_id = map["map_id"]
draw = 1
timestamp = "CURRENT_TIMESTAMP"
playback_string = ""
errors = ""
if "winner" in outcome:
  if outcome["winner"] == 0:
    pass
  elif outcome["winner"] == 1:
    winner = player_one["submission_id"]
    loser = player_two["submission_id"]
    draw = 0
  elif outcome["winner"] == 2:
    winner = player_two["submission_id"]
    loser = player_one["submission_id"]
    draw = 0
  else:
    errors += "Game engine reported invalid winner value: " + \
      str(outcome["winner"]) + "\n"
else:
  errors += "The engine did not report a winner."
if "playback" in outcome:
  playback_string = outcome["playback"]
if len(errors) == 0:
  cursor.execute("INSERT INTO games (winner,loser,map_id,draw,timestamp," + \
    "player_one,player_two,playback_string) VALUES (" + \
    str(winner) + "," + \
    str(loser) + "," + \
    str(map_id) + "," + \
    str(draw) + "," + \
    str(timestamp) + "," + \
    str(player_one["submission_id"]) + "," + \
    str(player_two["submission_id"]) + "," + \
    "'" + str(playback_string) + "')")
else:
  print errors

# Close the database connection
cursor.close()
connection.close()

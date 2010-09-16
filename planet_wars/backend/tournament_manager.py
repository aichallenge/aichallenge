import engine
import logging
import logging.handlers
import MySQLdb
import os
import random
from server_info import server_info
import subprocess
import sys
import time
import zlib

# Set up logging
logger = logging.getLogger('tm_logger')
logger.setLevel(logging.INFO)
handler = logging.handlers.RotatingFileHandler("tm.log",
                                               maxBytes=1000000,
                                               backupCount=5)
logger.addHandler(handler)
tm_pid = os.getpid()

def log_message(message):
  logger.info(str(tm_pid) + ": " + message)
  print message

log_message("started. time is " + str(time.time()))
log_message("parsing command-line arguments")

if len(sys.argv) != 2:
  print "USAGE: python tournament_manager.py time_limit_in_seconds"
  sys.exit(1)
time_limit = int(sys.argv[1])
log_message("time_limit: " + str(time_limit))
log_message("connecting to db")

# Connect to the database.
connection = MySQLdb.connect(host = server_info["db_host"],
                             user = server_info["db_username"],
                             passwd = server_info["db_password"],
                             db = server_info["db_name"])
cursor = connection.cursor(MySQLdb.cursors.DictCursor)
log_message("db connection established")
log_message("getting active submissions from db")

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
submissions = list(cursor.fetchall())
log_message("found " + str(len(submissions)) + " active submissions")
# Get the list of maps.
log_message("fetching map list from db")
cursor.execute("SELECT * FROM maps")
maps = cursor.fetchall()
log_message("found " + str(len(maps)) + " maps")
# Get the rankings
cursor.execute("""
  SELECT r.* from rankings r WHERE leaderboard_id = (select max(leaderboard_id) from leaderboards);
""")
rankings = cursor.fetchall()
log_message("found " + str(len(maps)) + " rankings")
# Order submissions by ranking
rankings_by_submission_id = {}
for ranking in rankings:
  rankings_by_submission_id[ranking['submission_id']] = ranking['rank']
default_rank = int(len(rankings)/2)
for submission in submissions:
  submission['rank'] = rankings_by_submission_id.get(submission['submission_id'],default_rank)
submissions.sort(key=lambda x: x['rank'])
# Are there enough players? Are there enough maps?
if len(submissions) < 2:
  log_message("There are fewer than two active submissions. No games can " + \
    "be played")
  sys.exit(0)
if len(maps) < 1:
  log_message("There are no maps in the database. No games can be played")
  sys.exit(0)
log_message("starting tournament. time is " + str(time.time()))
start_time = time.time()
while time.time() - start_time < time_limit:
  log_message("attempting to match two bots. time is " + str(time.time()))
  map = random.choice(maps)
  # Choose probably near bots to fight
  pickable_spread = random.choice([5,10,10,20,40,100,200,1000])
  player_one = random.choice(submissions)
  player_two = player_one
  while player_one == player_two:
    offset = random.randint(-pickable_spread,pickable_spread) + submissions.index(player_one)
    if offset < 0:
      continue
    if offset >= len(submissions):
      continue
    player_two = submissions[offset]
  log_message("%s (rank %d) vs %s (rank %d) on %s" % (
      player_one["submission_id"],
      player_one["rank"],
      player_two["submission_id"],
      player_two["rank"],
      map["name"]))
  # Invoke the game engine.
  player_one_path = "../submissions/" + str(player_one["submission_id"]) + "/."
  player_two_path = "../submissions/" + str(player_two["submission_id"]) + "/."
  map_path = "../maps/" + map["path"]
  players = [
    {"path" : player_one_path, "command" : player_one["command"]},
    {"path" : player_two_path, "command" : player_two["command"]}
  ]
  log_message("starting game")
  outcome = engine.play_game(map_path, 1000, 200, players, debug=False)
  log_message("game finished")
  # Store the game outcome in the database
  winner = "NULL"
  loser = "NULL"
  map_id = map["map_id"]
  draw = 1
  timestamp = "CURRENT_TIMESTAMP"
  playback_string = ""
  errors = ""
  if "error" in outcome:
    log_message("the game engine reported an error: " + outcome["error"])
  if "winner" in outcome:
    log_message("winner:" + str(outcome["winner"]))
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
    log_message("inserting game outcome into the db")
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
    
    game_id = connection.insert_id()
    compressed_playback_string = zlib.compress(playback_string,9)
    cursor = connection.cursor()
    cursor.execute(
        "INSERT INTO playback SET game_id = %s, playback_string = %s",
        (game_id, compressed_playback_string)
        )
    
    log_message("finished inserting")
  else:
    log_message(errors)

# Close the database connection
log_message("closing db connection")
cursor.close()
connection.close()
log_message("db connection closed")
log_message("exiting")

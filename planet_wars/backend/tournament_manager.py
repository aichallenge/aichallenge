import engine
from server_info import server_info
from compile_anything import compile_function, Log

import logging
import logging.handlers
import MySQLdb
import os
import random
import shutil
import subprocess
import sys
import signal
import tempfile
import time
import zlib
import urllib
import simplejson as json
import traceback

# Set up logging
logger = logging.getLogger('tm_logger')
logger.setLevel(logging.INFO)
handler = logging.handlers.RotatingFileHandler("tm.log",
                                               maxBytes=1000000,
                                               backupCount=5)
logger.addHandler(handler)
tm_pid = os.getpid()

def log_message(message):
  logger.info(str(tm_pid) + ": %s" % message)
  print message

log_message("started. time is " + str(time.time()))
log_message("parsing command-line arguments")

if len(sys.argv) != 2:
  print "USAGE: python tournament_manager.py time_limit_in_seconds"
  sys.exit(1)
time_limit = int(sys.argv[1])
log_message("time_limit: " + str(time_limit))


class GameAPIClient:
  def __init__(self, base_url, api_key):
    self.base_url = base_url
    self.api_key = api_key
  
  def get_matchup(self):
    url = self.base_url+'/api_get_matchup.php'
    url += '?api_key=%s' % self.api_key
    data = urllib.urlopen(url).read()
    # log_message(data)
    return json.loads(data)
  
  def ensure_submission_is_local(self, submission_id, language_name, platform_specific_compilation):
    submission_id = int(submission_id)
    submission_dir = "../submissions/%d" % submission_id
    if os.path.exists(submission_dir):
      log_message("Submission %d is local" % submission_id)
      return
    log_message("Downloading %d " % submission_id)
    download_dir = tempfile.mkdtemp(dir="../submissions/")
    os.chmod(download_dir, 0755)
    url = self.base_url+'/api_get_submission.php'
    url += '?api_key=%s&submission_id=%d' % (self.api_key, submission_id)
    os.system("cd %s; curl --silent '%s' | tar -xz" % (download_dir, url))

    if int(platform_specific_compilation) == 1:
      log_message("Compiling %s " % submission_id)
      os.chdir(download_dir)
      compile_log = Log()
      success = compile_function(language_name, compile_log)
      os.chdir('../../backend')
      if not success:
        shutil.rmtree(download_dir)
        log_message(compile_log.err)
        raise Exception()
    else:
      log_message("Not compiling %s " % submission_id)

    try:
      os.rename(download_dir, submission_dir)
    except OSError:
      # the submission directory was probably already created by another
      # manager, if not reraise the exception
      if not os.path.exists(submission_dir):
        raise
      shutil.rmtree(download_dir)
    
    
  def record_game(self, data):
    url = self.base_url+'/api_record_game.php'
    url += '?api_key=%s' % self.api_key
    print urllib.urlopen(url,json.dumps(data)).read()


cloud = GameAPIClient( server_info['api_base_url'], server_info['api_key'])

  

start_time = time.time()
while time.time() - start_time < time_limit:
  log_message("attempting to match two bots. time is " + str(time.time()))
  matchup = cloud.get_matchup();
  player_one = matchup['players'][0]
  player_two = matchup['players'][1]
  map_path = "../maps/" + matchup["map"]['name']
  log_message("%s (rank %s) vs %s (rank %s)" % (
      player_one["submission_id"],
      player_one["rank"],
      player_two["submission_id"],
      player_two["rank"]))
  cloud.ensure_submission_is_local(player_one["submission_id"],player_one["language_name"],player_one["platform_specific_compilation"])
  cloud.ensure_submission_is_local(player_two["submission_id"],player_two["language_name"],player_two["platform_specific_compilation"])

  
  
  # Invoke the game engine.
  player_one_path = "../submissions/" + str(player_one["submission_id"]) + "/."
  player_two_path = "../submissions/" + str(player_two["submission_id"]) + "/."
  players = [
    {"path" : player_one_path, "command" : player_one["command"], "submission_id": player_one["submission_id"]},
    {"path" : player_two_path, "command" : player_two["command"], "submission_id": player_two["submission_id"]}
  ]
  log_message("starting game")
  try:
    outcome = engine.play_game(map_path, 1000, 200, players, debug=False)
  except OSError:
    print "Game engine threw OS exception"
    print '-'*60
    traceback.print_exc(file=sys.stdout)
    print '-'*60
    time.sleep(5)
  log_message("game finished")
  
  # Store the game outcome in the database
  winner = "NULL"
  loser = "NULL"
  map_id = matchup["map"]['id']
  draw = 1
  timestamp = "CURRENT_TIMESTAMP"
  playback_string = ""
  errors = ""
  errors_data = []
  if "error" in outcome:
    log_message("the game engine reported an error: " + outcome["error"])
  if "errors_data" in outcome:
    errors_data = outcome["errors_data"]
    log_message(errors_data)
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
    
    data = {
      'winner':winner,
      'loser':loser,
      'map_id':map_id,
      'draw':draw,
      'player_one':player_one['submission_id'],
      'player_two':player_two['submission_id'],
      'playback_string':playback_string,
      'errors':errors_data
    }
    
    cloud.record_game(data)
    
    log_message("finished inserting")
  else:
    log_message(errors)

log_message("exiting")
os.kill(tm_pid, signal.SIGTERM) # Make sure all threads stop. Is there a better way?

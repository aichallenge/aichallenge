import math
import os
import signal
import shlex
import subprocess
import sys
import time
from user_sadbox import Sadbox

# Reads a text file that contains a game state, and returns the game state. A
# game state is composed of a list of fleets and a list of planets.
#   map: an absolute or relative filename that point to a text file.
def read_map_file(map):
  fleets = []
  planets = []
  f = open(map)
  for line in f:
    tokens = line.lower().strip().split(" ")
    if len(tokens) == 0:
      pass
    elif tokens[0] == "p":
      planets.append({
        "x" : float(tokens[1]),
        "y" : float(tokens[2]),
        "owner" : int(tokens[3]),
        "num_ships" : int(tokens[4]),
        "growth_rate" : int(tokens[5])
      })
    elif tokens[0] == "f":
      fleets.append({
        "owner" : int(tokens[1]),
        "num_ships" : int(tokens[2]),
        "source" : int(tokens[3]),
        "destination" : int(tokens[4]),
        "total_trip_length" : int(tokens[5]),
        "turns_remaining" : int(tokens[6])
    })
  return planets, fleets

# Carries out the point-of-view switch operation, so that each player can
# always assume that he is player number 1. There are three cases.
# 1. If pov < 0 then no pov switching is being used. Return player_id.
# 2. If player_id == pov then return 1 so that each player thinks he is
#    player number 1.
# 3. If player_id == 1 then return pov so that the real player 1 looks like
#    he is player number "pov".
# 4. Otherwise return player_id, since players other than 1 and pov are
#    unaffected by the pov switch.
def switch_pov(player_id, pov):
  if pov < 0:
    return player_id
  if player_id == pov:
    return 1
  if player_id == 1:
    return pov
  return player_id

# Generates a string representation of a planet. This is used to send data
# about the planets to the client programs.
def serialize_planet(p, pov):
  owner = switch_pov(int(p["owner"]), pov)
  message = "P " + str(p["x"]) + " " + str(p["y"]) + " " + str(owner) + \
    " " + str(int(p["num_ships"])) + " " + str(int(p["growth_rate"]))
  return message.replace(".0 ", " ")

# Generates a string representation of a fleet. This is used to send data
# about the fleets to the client programs.
def serialize_fleet(f, pov):
  owner = switch_pov(int(f["owner"]), pov)
  message = "F " + str(owner) + " " + str(int(f["num_ships"])) + " " + \
    str(int(f["source"])) + " " + str(int(f["destination"])) + " " + \
    str(int(f["total_trip_length"])) + " " + str(int(f["turns_remaining"]))
  return message.replace(".0 ", " ")

# Takes a string which contains an order and parses it, returning the order in
# dictionary format. If the order can't be parsed, return None.
def parse_order_string(s):
  tokens = s.split(" ")
  if len(tokens) == 3:
    try:
      return {
        "source" : int(tokens[0]),
        "destination" : int(tokens[1]),
        "num_ships" : int(tokens[2])
      }
    except:
      return None
  else:
    return None

# Calculates the travel time between two planets. This is the cartesian
# distance, rounded up to the nearest integer.
def travel_time(a, b):
  dx = b["x"] - a["x"]
  dy = b["y"] - a["y"]
  return int(math.ceil(math.sqrt(dx * dx + dy * dy)))

# Processes the given order, as if it was given by the given player_id. If
# everything goes well, returns True. Otherwise, returns False.
def issue_order(order, player_id, planets, fleets, order_strings):
  source_planet = planets[order["source"]]
  destination_planet = planets[order["destination"]]
  if source_planet["owner"] != player_id:
    return False
  if order["num_ships"] > source_planet["num_ships"]:
    return False
  if order["num_ships"] < 0:
    return False
  source_planet["num_ships"] -= order["num_ships"]
  t = travel_time(source_planet, destination_planet)
  fleets.append({
    "source" : order["source"],
    "destination" : order["destination"],
    "num_ships" : order["num_ships"],
    "owner" : source_planet["owner"],
    "total_trip_length" : t,
    "turns_remaining" : t
  })
  order_strings.append(str(order["source"]) + "." + \
    str(order["destination"]) + "." + str(order["num_ships"]))

# "a" is an array. This method returns the number of non-zero elements in a.
def num_non_zero(a):
  return len([x for x in a if x != 0])

# Performs the logic needed to advance the state of the game by one turn.
# Fleets move forward one tick. Any fleets reaching their destinations are
# dealt with. If there are any battles to be resolved, then they're taken
# care of.
def do_time_step(planets, fleets):
  for p in planets:
    if p["owner"] > 0:
      p["num_ships"] += p["growth_rate"]
  attackers = dict()
  traveling_fleets = []
  for f in fleets:
    f["turns_remaining"] -= 1
    if f["turns_remaining"] > 0:
      traveling_fleets.append(f)
    else:
      dest = f["destination"]
      owner = f["owner"]
      if dest not in attackers:
        attackers[dest] = dict()
      if owner not in attackers[dest]:
        attackers[dest][owner] = 0
      attackers[dest][owner] += f["num_ships"]
  fleets = traveling_fleets
  for i, p in enumerate(planets):
    if i not in attackers:
      continue
    defender = p["owner"]
    defending_forces = p["num_ships"]
    if defender in attackers[i]:
      defending_forces += attackers[i][defender]
      del attackers[i][defender]
    attacking_players = [key for key, value in attackers[i].items()]
    attacking_forces = [value for key, value in attackers[i].items()]
    current_attacker = 0
    while defending_forces > 0 and sum(attacking_forces) > 0:
      if attacking_forces[current_attacker] > 0:
        attacking_forces[current_attacker] -= 1
        defending_forces -= 1
      current_attacker += 1
      if current_attacker >= len(attacking_players):
        current_attacker = 0
    if sum(attacking_forces) > 0:
      while num_non_zero(attacking_forces) > 1:
        if attacking_forces[current_attacker] > 0:
          attacking_forces[current_attacker] -= 1
        current_attacker += 1
        if current_attacker >= len(attacking_players):
          current_attacker = 0
      for i in range(len(attacking_players)):
        if attacking_forces[i] > 0:
          p["owner"] = attacking_players[i]
          p["num_ships"] = attacking_forces[i]
    else:
      p["owner"] = defender
      p["num_ships"] = defending_forces
  return planets, fleets

# Calculates the number of players remaining
def remaining_players(planets, fleets):
  players = set()
  for p in planets:
    players.add(p["owner"])
  for f in fleets:
    players.add(f["owner"])
  players.discard(0)
  return players

# Returns a string representation of the entire game state.
def serialize_game_state(planets, fleets, pov):
  message = "\n".join([serialize_planet(p, pov) for p in planets]) + \
    "\n" + "\n".join([serialize_fleet(f, pov) for f in fleets]) + "\ngo\n"
  return message.replace("\n\n", "\n")

# Turns a list of planets into a string in playback format. This is the initial
# game state part of a game playback string.
def planet_to_playback_format(planets):
  planet_strings = []
  for p in planets:
    planet_strings.append(str(p["x"]) + "," + str(p["y"]) + "," + \
      str(p["owner"]) + "," + str(p["num_ships"]) + "," + \
      str(p["growth_rate"]))
  return ":".join(planet_strings)

# Returns True if and only if all the elements of list are True. Otherwise
# return False.
def all_true(list):
  for item in list:
    if item == False:
      return False
  return True

# Kicks the given player from the game. All their fleets disappear. All their
# planets become neutral, and keep the same number of ships.
def kick_player_from_game(player_number, planets, fleets):
  for f in fleets:
    if f["owner"] == player_number:
      f["turns_remaining"] = 1
      f["owner"] = 0
      f["num_ships"] = 0
  for p in planets:
    if p["owner"] == player_number:
      p["owner"] = 0

# Turns a list of planets into a string in playback format. This is the initial
# game state part of a game playback string.
def fleets_to_playback_format(fleets):
  fleet_strings = []
  for p in fleets:
    fleet_strings.append(str(p["owner"]) + "." + str(p["num_ships"]) + "." + \
      str(p["source"]) + "." + str(p["destination"]) + "." + \
      str(p["total_trip_length"]) + "." + str(p["turns_remaining"]))
  return ",".join(fleet_strings)

# Represents the game state in frame format. Represents one frame.
def frame_representation(planets, fleets):
  planet_string = \
    ",".join([str(p["owner"]) + "." + str(p["num_ships"]) for p in planets])
  return planet_string + "," + fleets_to_playback_format(fleets)

def num_ships_for_player(planets, fleets, player_id):
  return sum([p["num_ships"] for p in planets if p["owner"] == player_id]) + \
    sum([f["num_ships"] for f in fleets if f["owner"] == player_id])

def player_with_most_ships(planets, fleets):
  max_player = 0
  max_ships = 0
  for player in remaining_players(planets, fleets):
    ships = num_ships_for_player(planets, fleets, player)
    if ships == max_ships:
      max_player = 0
    elif ships > max_ships:
      max_ships = ships
      max_player = player
  return max_player

# Plays a game of Planet Wars.
#   map: a full or relative path to a text file containing the map that this
#        game should be played on.
#   max_turn_time: the maximum amount of time each player gets to finish each
#                  turn. A player forfeits the game if they run over their
#                  time allocation.
#   max_turns: the max length of a game in turns. If the game isn't over after
#              this many turns, the player with the most ships wins.
#   players: a list of dictionaries, each of which has information about one
#            of the players. Each dictionary should have the following keys:
#            path: the path where the player's files are located
#            command: the command that invokes the player, assuming the given
#                     path is the current working directory.
def play_game(map, max_turn_time, max_turns, players, debug=False):
  planets, fleets = read_map_file(map)
  playback = planet_to_playback_format(planets) + "|"
  clients = []
  if debug:
    sys.stderr.write("starting client programs\n")
  for i, p in enumerate(players):
    client = Sadbox(working_directory=p["path"],
                    shell_command=p["command"],
                    security_on=True)
    if client.is_alive:
      if debug:
        sys.stderr.write("    started player " + str(i+1) + "\n")
      clients.append(client)
    else:
      if debug:
        sys.stderr.write("    failed to start player " + str(i+1) + "\n")
      return {"error" : "failure_to_start_client"}
  if debug:
    sys.stderr.write("waiting for players to spin up\n")
  time.sleep(3)
  turn_number = 1
  turn_strings = []
  outcome = dict()
  remaining = remaining_players(planets, fleets)
  while turn_number <= max_turns and len(remaining) > 1:
    order_strings = []
    for i, c in enumerate(clients):
      if (i+1) not in remaining:
        continue
      message = serialize_game_state(planets, fleets, i+1)
      if debug:
        sys.stderr.write("engine > player" + str(i+1) + ":\n")
        sys.stderr.write(message)
      c.write(message)
    time.sleep(1)
    client_done = [False] * len(clients)
    start_time = time.time()
    time_limit = float(max_turn_time) / 1000
    # Get orders from players
    while not all_true(client_done) and time.time() - start_time < time_limit:
      for i, c in enumerate(clients):
        if client_done[i] or not c.is_alive or (i+1) not in remaining:
          continue
        line = c.read_line()
        if line is None:
          continue
        line = line.strip().lower()
        if line == "go":
          client_done[i] = True
        else:
          order = parse_order_string(line)
          if order is None:
            sys.stderr.write("player " + str(i+1) + " kicked for making " + \
              "an unparseable order: " + line + "\n")
            c.kill()
            kick_player_from_game(i+1, planets, fleets)
          else:
            if debug:
              sys.stderr.write("player " + str(i+1) + " order: " + line + "\n")
            issue_order(order, i+1, planets, fleets, order_strings)
      time.sleep(0)
    # Kick players that took too long to move.
    for i, c in enumerate(clients):
      if (i+1) not in remaining or client_done[i]:
        continue
      sys.stderr.write("player " + str(i+1) + " kicked for taking too " + \
        "long to move\n")
      if "timeout" not in outcome:
        outcome["timeout"] = []
      outcome["timeout"].append(i+1)
      c.kill()
      kick_player_from_game(i+1, planets, fleets)
    planets, fleets = do_time_step(planets, fleets)
    turn_strings.append(frame_representation(planets, fleets))
    remaining = remaining_players(planets, fleets)
    turn_number += 1
  for i, c in enumerate(clients):
    if not c.is_alive:
      if "fail" not in outcome:
        outcome["fail"] = []
      outcome["fail"].append(i+1)
    c.kill()
  playback += ":".join(turn_strings)
  outcome["winner"] = player_with_most_ships(planets, fleets)
  outcome["playback"] = playback
  return outcome

def main():
  players = [
    {"path" : "../submissions/123443/.", "command" : "./MyBot"},
    {"path" : "../submissions/123429/.", "command" : "java -jar MyBot.jar"}
  ]
  print "game result: " + \
    str(play_game("../maps/map7.txt", 1000, 10, players, True))

if __name__ == "__main__":
  main()

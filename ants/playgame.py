#!/usr/bin/env python
import traceback
import sys
import os
import time
from optparse import OptionParser
import random

from ants import Ants

# get engine from worker dir
sys.path.append("../worker")
from engine import run_game

def main(argv):
    usage ="Usage: %prog [options] map bot1 bot2\n\nYou must specify a map file."
    parser = OptionParser(usage=usage)

    # map to be played
    # number of players is determined by the map file
    parser.add_option("-m", "--map_file", dest="map",
                      help="Name of the map file")

    # maximum number of turns that the game will be played
    parser.add_option("-t", "--turns", dest="turns",
                      default=200, type="int",
                      help="Number of turns in the game")
    
    # the output directory will contain the replay file used by the visualizer
    # it will also contain the bot input/output logs, if requested
    parser.add_option("-o", "--output_dir", dest="output_dir",
                      help="Directory to dump replay files to.")
    parser.add_option("-j", "--output_json", dest="output_json",
                      action="store_true", default=False,
                      help="Return json result from engine.")
    parser.add_option("-I", "--log_input", dest="log_input",
                       action="store_true", default=False,
                       help="Log input streams sent to bots")
    parser.add_option("-O", "--log_output", dest="log_output",
                       action="store_true", default=False,
                       help="Log output streams from bots")

    parser.add_option("--serial", dest="serial",
                      action="store_true",
                      help="Run bots in serial, instead of parallel.")

    parser.add_option("-v", "--verbose", dest="verbose",
                      action="store_true", default=False,
                      help="Print out status as game goes.")

    parser.add_option("--turntime", dest="turntime",
                      default=1000, type="int",
                      help="Amount of time to give each bot, in milliseconds")
    parser.add_option("--loadtime", dest="loadtime",
                      default=3000, type="int",
                      help="Amount of time to give for load, in milliseconds")
    parser.add_option("-r", "--rounds", dest="rounds",
                      default=1, type="int",
                      help="Number of rounds to play")
    parser.add_option("--seed", dest="seed",
                      default=None, type="int",
                      help="Seed for the random number generator")

    # ants specific game options
    parser.add_option("--attack", dest="attack",
                      default="support",
                      help="Attack method to use for engine. (closest, occupied, support, damage)")
    parser.add_option("--food", dest="food",
                      default="sections",
                      help="Food spawning method. (none, random, sections, symmetric)")
    parser.add_option("--viewradius2", dest="viewradius2",
                      default=96, type="int",
                      help="Vision radius of ants squared")
    parser.add_option("--spawnradius2", dest="spawnradius2",
                      default=2, type="int",
                      help="Spawn radius of ants squared")
    parser.add_option("--attackradius2", dest="attackradius2",
                      default=5, type="int",
                      help="Attack radius of ants squared")

    (opts, args) = parser.parse_args(argv)
    if opts.map is None or not os.path.exists(opts.map):
        parser.print_help()
        return -1
    try:
        # this split of options is not needed, but left for documentation
        game_options = {
            "map": opts.map,
            "attack": opts.attack,
            "food": opts.food,
            "viewradius2": opts.viewradius2,
            "attackradius2": opts.attackradius2,
            "spawnradius2": opts.spawnradius2,
            "loadtime": opts.loadtime,
            "turntime": opts.turntime,
            "turns": opts.turns,
            "seed": opts.seed }
        engine_options = {
            "loadtime": opts.loadtime,
            "turntime": opts.turntime,
            "map_file": opts.map,
            "turns": opts.turns,
            "output_dir": opts.output_dir,
            "output_json": opts.output_json,
            "log_input": opts.log_input,
            "log_output": opts.log_output,
            "serial": opts.serial,
            "verbose": opts.verbose }
        random.seed(opts.seed)
        for round in range(opts.rounds):
            map_file = open(opts.map, 'r')
            game_options["map"] = map_file.read()
            map_file.close()
            game = Ants(game_options)
            bots = [('.', arg) for arg in args]
            if game.num_players != len(bots):
                print("Incorrect number of bots for map.  Need %s, got %s" % 
                      (game.num_players, len(bots)))
                break
            print('playgame round %s' % round)
            result = run_game(game, bots, engine_options, round)
            if opts.output_json:
                print result

    except Exception:
        traceback.print_exc()

    finally:
        return 1

if __name__ == "__main__":
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass
    #import cProfile
    #cProfile.run('sys.exit(main(sys.argv[1:]))')
    sys.exit(main(sys.argv[1:]))

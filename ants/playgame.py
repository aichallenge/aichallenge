#!/usr/bin/env python
import traceback
import sys
import os
import time
from engine import run_game

from ants import Ants

def main(argv):
    from optparse import OptionParser
    usage ="Usage: %prog [options] bot1 bot2"
    parser = OptionParser()

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
                      action="store_true",
                      help="Print out status as game goes.")

    parser.add_option("--turntime", dest="turntime",
                      default=1000, type="int",
                      help="Amount of time to give each bot, in milliseconds")
    parser.add_option("--loadtime", dest="loadtime",
                      default=3000, type="int",
                      help="Amount of time to give for load, in milliseconds")
    parser.add_option("--attack", dest="attack",
                      default="closest",
                      help="Attack method to use for engine. (closest, occupied)")
    parser.add_option("-r", "--rounds", dest="rounds",
                      default=1, type="int",
                      help="Number of rounds to play")

    (opts, args) = parser.parse_args(argv)
    if opts.map is None:
        print("Please specify a map to start with")
        return -1
    try:
        options = {"map": opts.map,
                   "attack": opts.attack,
                   "loadtime": opts.loadtime,
                   "turntime": opts.turntime,
                   "turns": opts.turns }
        for round in range(opts.rounds):
            game = Ants(options)
            bots = [('.', arg) for arg in args]
            if game.num_players != len(bots):
                print("Incorrect number of bots for map.  Need %s, got %s" % 
                      (game.num_players, len(bots)))
                break
            print('playgame round %s' % round)
            run_game(game, bots, opts.turntime, opts.loadtime,
                     opts.turns, opts.output_dir,
                     opts.verbose, opts.serial, round)

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

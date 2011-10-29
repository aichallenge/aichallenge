#!/usr/bin/env python
from __future__ import print_function
import traceback
import sys
import os
import time
from optparse import OptionParser, OptionGroup
import random
import cProfile
import visualizer.visualize_locally
import json
try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO

from ants import Ants

sys.path.append("../worker")
try:
    from engine import run_game
except ImportError:
    # this can happen if we're launched with cwd outside our own dir
    # get our full path, then work relative from that
    cmd_folder = os.path.dirname(os.path.abspath(__file__))
    if cmd_folder not in sys.path:
        sys.path.insert(0, cmd_folder)
    sys.path.append(cmd_folder + "/../worker")
    # try again
    from engine import run_game

# make stderr red text
try:
    import colorama
    colorama.init()
    colorize = True
    color_default = (colorama.Fore.RED)
    color_reset = (colorama.Style.RESET_ALL)
except:
    colorize = False
    color_default = None
    color_reset = None

class Colorize(object):
    def __init__(self, file, color=color_default):
        self.file = file
        self.color = color
        self.reset = color_reset
    def write(self, data):
        if self.color:
            self.file.write(''.join(self.color))
        self.file.write(data)
        if self.reset:
            self.file.write(''.join(self.reset))
    def flush(self):
        self.file.flush()
    def close(self):
        self.file.close()

if colorize:
    stderr = Colorize(sys.stderr)
else:
    stderr = sys.stderr

class Comment(object):
    def __init__(self, file):
        self.file = file
        self.last_char = '\n'
    def write(self, data):
        for char in data:
            if self.last_char == '\n':
                self.file.write('# ')
            self.file.write(char)
            self.last_char = char
    def flush(self):
        self.file.flush()
    def close(self):
        self.file.close()

class Tee(object):
    ''' Write to multiple files at once '''
    def __init__(self, *files):
        self.files = files
    def write(self, data):
        for file in self.files:
            file.write(data)
    def flush(self):
        for file in self.files:
            file.flush()
    def close(self):
        for file in self.files:
            file.close()
            
def main(argv):
    usage ="Usage: %prog [options] map bot1 bot2\n\nYou must specify a map file."
    parser = OptionParser(usage=usage)

    # map to be played
    # number of players is determined by the map file
    parser.add_option("-m", "--map_file", dest="map",
                      help="Name of the map file")

    # maximum number of turns that the game will be played
    parser.add_option("-t", "--turns", dest="turns",
                      default=1000, type="int",
                      help="Number of turns in the game")

    parser.add_option("--serial", dest="serial",
                      action="store_true",
                      help="Run bots in serial, instead of parallel.")

    parser.add_option("--turntime", dest="turntime",
                      default=1000, type="int",
                      help="Amount of time to give each bot, in milliseconds")
    parser.add_option("--loadtime", dest="loadtime",
                      default=3000, type="int",
                      help="Amount of time to give for load, in milliseconds")
    parser.add_option("-r", "--rounds", dest="rounds",
                      default=1, type="int",
                      help="Number of rounds to play")
    parser.add_option("--player_seed", dest="player_seed",
                      default=None, type="int",
                      help="Player seed for the random number generator")
    parser.add_option("--engine_seed", dest="engine_seed",
                      default=None, type="int",
                      help="Engine seed for the random number generator")
    
    parser.add_option('--strict', dest='strict',
                      action='store_true', default=False,
                      help='Strict mode enforces valid moves for bots')
    parser.add_option('--capture_errors', dest='capture_errors',
                      action='store_true', default=False,
                      help='Capture errors and stderr in game result')
    parser.add_option('--end_wait', dest='end_wait',
                      default=0, type="float",
                      help='Seconds to wait at end for bots to process end')
    parser.add_option('--secure_jail', dest='secure_jail',
                      action='store_true', default=False,
                      help='Use the secure jail for each bot (*nix only)')
    parser.add_option('--fill', dest='fill',
                      action='store_true', default=False,
                      help='Fill up extra player starts with last bot specified')
    parser.add_option('-p', '--position', dest='position',
                      default=0, type='int',
                      help='Player position for first bot specified')

    # ants specific game options
    game_group = OptionGroup(parser, "Game Options", "Options that affect the game mechanics for ants")
    game_group.add_option("--attack", dest="attack",
                          default="focus",
                          help="Attack method to use for engine. (closest, focus, support, damage)")
    game_group.add_option("--kill_points", dest="kill_points",
                          default=2, type="int",
                          help="Points awarded for killing a hill")
    game_group.add_option("--food", dest="food",
                          default="symmetric",
                          help="Food spawning method. (none, random, sections, symmetric)")
    game_group.add_option("--viewradius2", dest="viewradius2",
                          default=77, type="int",
                          help="Vision radius of ants squared")
    game_group.add_option("--spawnradius2", dest="spawnradius2",
                          default=1, type="int",
                          help="Spawn radius of ants squared")
    game_group.add_option("--attackradius2", dest="attackradius2",
                          default=5, type="int",
                          help="Attack radius of ants squared")
    game_group.add_option("--food_rate", dest="food_rate", nargs=2, type="int", default=(5,11),
                          help="Numerator of food per turn per player rate")
    game_group.add_option("--food_turn", dest="food_turn", nargs=2, type="int", default=(19,37),
                          help="Denominator of food per turn per player rate")
    game_group.add_option("--food_start", dest="food_start", nargs=2, type="int", default=(75,175),
                          help="One over percentage of land area filled with food at start")
    game_group.add_option("--food_visible", dest="food_visible", nargs=2, type="int", default=(3,5),
                          help="Amount of food guaranteed to be visible to starting ants")
    game_group.add_option("--cutoff_turn", dest="cutoff_turn", type="int", default=150,
                          help="Number of turns cutoff percentage is maintained to end game early")
    game_group.add_option("--cutoff_percent", dest="cutoff_percent", type="float", default=0.85,
                          help="Number of turns cutoff percentage is maintained to end game early")
    game_group.add_option("--scenario", dest="scenario",
                          action='store_true', default=False)
    parser.add_option_group(game_group)

    # the log directory must be specified for any logging to occur, except:
    #    bot errors to stderr
    #    verbose levels 1 & 2 to stdout and stderr
    #    profiling to stderr
    # the log directory will contain
    #    the replay or stream file used by the visualizer, if requested
    #    the bot input/output/error logs, if requested    
    log_group = OptionGroup(parser, "Logging Options", "Options that control the logging")
    log_group.add_option("-g", "--game", dest="game_id", default=0, type='int',
                         help="game id to start at when numbering log files")
    log_group.add_option("-l", "--log_dir", dest="log_dir", default=None,
                         help="Directory to dump replay files to.")
    log_group.add_option('-R', '--log_replay', dest='log_replay',
                         action='store_true', default=False),
    log_group.add_option('-S', '--log_stream', dest='log_stream',
                         action='store_true', default=False),
    log_group.add_option("-I", "--log_input", dest="log_input",
                         action="store_true", default=False,
                         help="Log input streams sent to bots")
    log_group.add_option("-O", "--log_output", dest="log_output",
                         action="store_true", default=False,
                         help="Log output streams from bots")
    log_group.add_option("-E", "--log_error", dest="log_error",
                         action="store_true", default=False,
                         help="log error streams from bots")
    log_group.add_option('-e', '--log_stderr', dest='log_stderr',
                         action='store_true', default=False,
                         help='additionally log bot errors to stderr')
    log_group.add_option('-o', '--log_stdout', dest='log_stdout',
                         action='store_true', default=False,
                         help='additionally log replay/stream to stdout')
    # verbose will not print bot input/output/errors
    # only info+debug will print bot error output
    log_group.add_option("-v", "--verbose", dest="verbose",
                         action='store_true', default=False,
                         help="Print out status as game goes.")
    log_group.add_option("--profile", dest="profile",
                         action="store_true", default=False,
                         help="Run under the python profiler")
    parser.add_option("--nolaunch", dest="nolaunch",
                      action='store_true', default=False,
                      help="Prevent visualizer from launching")
    log_group.add_option("--html", dest="html_file",
                         default=None,
                         help="Output file name for an html replay")
    parser.add_option_group(log_group)

    (opts, args) = parser.parse_args(argv)
    if opts.map is None or not os.path.exists(opts.map):
        parser.print_help()
        return -1
    try:
        if opts.profile:
            # put profile file into output dir if we can
            prof_file = "ants.profile"
            if opts.log_dir:
                prof_file = os.path.join(opts.log_dir, prof_file)
            # cProfile needs to be explitly told about out local and global context
            print("Running profile and outputting to {0}".format(prof_file,), file=stderr)
            cProfile.runctx("run_rounds(opts,args)", globals(), locals(), prof_file)
        else:
            # only use psyco if we are not profiling
            # (psyco messes with profiling)
            try:
                import psyco
                psyco.full()
            except ImportError:
                pass
            run_rounds(opts,args)
        return 0
    except Exception:
        traceback.print_exc()
        return -1

def run_rounds(opts,args):
    def get_cmd_wd(cmd, exec_rel_cwd=False):
        ''' get the proper working directory from a command line '''
        new_cmd = []
        wd = None
        for i, part in reversed(list(enumerate(cmd.split()))):
            if wd == None and os.path.exists(part):
                wd = os.path.dirname(os.path.realpath(part))
                basename = os.path.basename(part)
                if i == 0:
                    if exec_rel_cwd:
                        new_cmd.insert(0, os.path.join(".", basename))
                    else:
                        new_cmd.insert(0, part)
                else:
                    new_cmd.insert(0, basename)
            else:
                new_cmd.insert(0, part)
        return wd, ' '.join(new_cmd)
    def get_cmd_name(cmd):
        ''' get the name of a bot from the command line '''
        for i, part in enumerate(reversed(cmd.split())):
            if os.path.exists(part):
                return os.path.basename(part)
# this split of options is not needed, but left for documentation
    game_options = {
        "map": opts.map,
        "attack": opts.attack,
        "kill_points": opts.kill_points,
        "food": opts.food,
        "viewradius2": opts.viewradius2,
        "attackradius2": opts.attackradius2,
        "spawnradius2": opts.spawnradius2,
        "loadtime": opts.loadtime,
        "turntime": opts.turntime,
        "turns": opts.turns,
        "food_rate": opts.food_rate,
        "food_turn": opts.food_turn,
        "food_start": opts.food_start,
        "food_visible": opts.food_visible,
        "cutoff_turn": opts.cutoff_turn,
        "cutoff_percent": opts.cutoff_percent,
        "scenario": opts.scenario }
    if opts.player_seed != None:
        game_options['player_seed'] = opts.player_seed
    if opts.engine_seed != None:
        game_options['engine_seed'] = opts.engine_seed
    engine_options = {
        "loadtime": opts.loadtime,
        "turntime": opts.turntime,
        "map_file": opts.map,
        "turns": opts.turns,
        "log_replay": opts.log_replay,
        "log_stream": opts.log_stream,
        "log_input": opts.log_input,
        "log_output": opts.log_output,
        "log_error": opts.log_error,
        "serial": opts.serial,
        "strict": opts.strict,
        "capture_errors": opts.capture_errors,
        "secure_jail": opts.secure_jail,
        "end_wait": opts.end_wait }
    for round in range(opts.rounds):
        # initialize game
        game_id = round + opts.game_id
        with open(opts.map, 'r') as map_file:
            game_options['map'] = map_file.read()
        if opts.engine_seed:
            game_options['engine_seed'] = opts.engine_seed + round
        game = Ants(game_options)
        # initialize bots
        bots = [get_cmd_wd(arg, exec_rel_cwd=opts.secure_jail) for arg in args]
        bot_count = len(bots)
        # insure correct number of bots, or fill in remaining positions
        if game.num_players != len(bots):
            if game.num_players > len(bots) and opts.fill:
                extra = game.num_players - len(bots)
                for _ in range(extra):
                    bots.append(bots[-1])
            else:
                print("Incorrect number of bots for map.  Need {0}, got {1}"
                      .format(game.num_players, len(bots)), file=stderr)
                for arg in args:
                    print("Bot Cmd: {0}".format(arg), file=stderr)
                break
        bot_count = len(bots)
        # move position of first bot specified
        if opts.position > 0 and opts.position <= len(bots):
            first_bot = bots[0]
            bots = bots[1:]
            bots.insert(opts.position, first_bot)

        # initialize file descriptors
        if opts.log_dir and not os.path.exists(opts.log_dir):
            os.mkdir(opts.log_dir)
        if not opts.log_replay and not opts.log_stream and (opts.log_dir or opts.log_stdout):
            opts.log_replay = True
        replay_path = None # used for visualizer launch
        
        if opts.log_replay:
            if opts.log_dir:
                replay_path = os.path.join(opts.log_dir, '{0}.replay'.format(game_id))
                engine_options['replay_log'] = open(replay_path, 'w')
            if opts.log_stdout:
                if 'replay_log' in engine_options and engine_options['replay_log']:
                    engine_options['replay_log'] = Tee(sys.stdout, engine_options['replay_log'])
                else:
                    engine_options['replay_log'] = sys.stdout
        else:
            engine_options['replay_log'] = None

        if opts.log_stream:
            if opts.log_dir:
                engine_options['stream_log'] = open(os.path.join(opts.log_dir, '{0}.stream'.format(game_id)), 'w')
            if opts.log_stdout:
                if engine_options['stream_log']:
                    engine_options['stream_log'] = Tee(sys.stdout, engine_options['stream_log'])
                else:
                    engine_options['stream_log'] = sys.stdout
        else:
            engine_options['stream_log'] = None
        
        if opts.log_input and opts.log_dir:
            engine_options['input_logs'] = [open(os.path.join(opts.log_dir, '{0}.bot{1}.input'.format(game_id, i)), 'w')
                             for i in range(bot_count)]
        else:
            engine_options['input_logs'] = None
        if opts.log_output and opts.log_dir:
            engine_options['output_logs'] = [open(os.path.join(opts.log_dir, '{0}.bot{1}.output'.format(game_id, i)), 'w')
                              for i in range(bot_count)]
        else:
            engine_options['output_logs'] = None
        if opts.log_error and opts.log_dir:
            if opts.log_stderr:
                if opts.log_stdout:
                    engine_options['error_logs'] = [Tee(Comment(stderr), open(os.path.join(opts.log_dir, '{0}.bot{1}.error'.format(game_id, i)), 'w'))
                                      for i in range(bot_count)]
                else:
                    engine_options['error_logs'] = [Tee(stderr, open(os.path.join(opts.log_dir, '{0}.bot{1}.error'.format(game_id, i)), 'w'))
                                      for i in range(bot_count)]
            else:
                engine_options['error_logs'] = [open(os.path.join(opts.log_dir, '{0}.bot{1}.error'.format(game_id, i)), 'w')
                                  for i in range(bot_count)]
        elif opts.log_stderr:
            if opts.log_stdout:
                engine_options['error_logs'] = [Comment(stderr)] * bot_count
            else:
                engine_options['error_logs'] = [stderr] * bot_count
        else:
            engine_options['error_logs'] = None
        
        if opts.verbose:
            if opts.log_stdout:
                engine_options['verbose_log'] = Comment(sys.stdout)
            else:
                engine_options['verbose_log'] = sys.stdout
            
        engine_options['game_id'] = game_id 
        if opts.rounds > 1:
            print('# playgame round {0}, game id {1}'.format(round, game_id))

        # intercept replay log so we can add player names
        if opts.log_replay:
            intcpt_replay_io = StringIO()
            real_replay_io = engine_options['replay_log']
            engine_options['replay_log'] = intcpt_replay_io

        result = run_game(game, bots, engine_options)

        # add player names, write to proper io, reset back to normal
        if opts.log_replay:
            replay_json = json.loads(intcpt_replay_io.getvalue())
            replay_json['playernames'] = [get_cmd_name(arg) for arg in args]
            real_replay_io.write(json.dumps(replay_json))
            intcpt_replay_io.close()
            engine_options['replay_log'] = real_replay_io

        # close file descriptors
        if engine_options['stream_log']:
            engine_options['stream_log'].close()
        if engine_options['replay_log']:
            engine_options['replay_log'].close()
        if engine_options['input_logs']:
            for input_log in engine_options['input_logs']:
                input_log.close()
        if engine_options['output_logs']:
            for output_log in engine_options['output_logs']:
                output_log.close()
        if engine_options['error_logs']:
            for error_log in engine_options['error_logs']:
                error_log.close()
        if replay_path:
            if opts.nolaunch:
                if opts.html_file:
                    visualizer.visualize_locally.launch(replay_path, True, opts.html_file)
            else:
                if opts.html_file == None:
                    visualizer.visualize_locally.launch(replay_path,
                            generated_path="replay.{0}.html".format(game_id))
                else:
                    visualizer.visualize_locally.launch(replay_path,
                            generated_path=opts.html_file)
if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

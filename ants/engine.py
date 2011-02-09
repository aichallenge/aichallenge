#!/usr/bin/env python
import subprocess
import shlex

import threading
import Queue
import sys

from Queue import Queue, Empty
from subprocess import Popen
from threading import Thread

from ants import Ants

import copy

player_count = 4

def switch_players(m, a, b):
    m = m.clone()
    for x in range(m.width):
        for y in range(m.height):
            if m.map[x][y] == a:
                m.map[x][y] = b
            elif m.map[x][y] == b:
                m.map[x][y] = a
    for key in m.ant_list.keys():
        if m.ant_list[key] == a:
            m.ant_list[key] = b
        elif m.ant_list[key] == b:
            m.ant_list[key] = a
    return m

def save_image(map, turn, anno=""):
    img = map.render()
    scale = 4
    new_size = (img.size[0] * scale, img.size[1] * scale)
    img = img.resize(new_size)
    img.save("playback/frame" + str(turn).zfill(5) + anno + ".png")

def play_game(map_filename):
    m = AntMap(map_filename)
    players = [HunterBot() for i in range(m.num_players)]
    perspective = [[0 for i in range(m.num_players)] for i in range(m.num_players)]
    initial_food_density = 0.01
    food_amount = int(initial_food_density * m.land_area)
    m.do_food(food_amount)
    turn_count = 1
    score = [0 for i in range(len(players))]
    while not m.game_over() and turn_count < 1000:
        print "turn:", turn_count
        save_image(m, turn_count, 'start')
        for i, player in enumerate(players):
            player_number = i + 1
            reflected_map = switch_players(m, player_number, 1)
            orders = player.do_turn(reflected_map)
            m.do_orders(player_number, orders)
            save_image(m, turn_count, 'player' + str(i+1))
        m.resolve_orders()
        save_image(m, turn_count, 'move')
        m.do_death()
        save_image(m, turn_count, 'death')
        m.do_birth()
        save_image(m, turn_count, 'birth')
        m.do_food()
        save_image(m, turn_count, 'food')
        turn_count += 1
        turn_score = m.do_score()
        for i in range(len(players)+1):
            score[i] += turn_score[i]
    save_image(m, turn_count, 'start')
    print score




class BotThread(Thread):
    """
    Class to run a bot in a thread.

    Starts up a thread to talk to a bot process, and provide an
    interface to call it. Handles the timeout here.
    """
    def __init__(self, command):
        Thread.__init__(self)

        self.input = Queue()
        self.output = Queue()
        args = shlex.split(command)
        self.process = None
        self.process = Popen(args,
                             stdout=subprocess.PIPE,
                             stdin=subprocess.PIPE)
        self.daemon = False
        self.start()

    def run_step(self, game_state, timeoutms):
        """Run one step of the game state."""
        self.input.put(game_state)
        try:
            result = self.output.get(True, timeoutms/1000.0)
            return result, True
        except Empty:
            return "TIMEOUT", False
        except Crashed:
            return "CRASHED", False

    def __del__(self):
        """Clean up the process if it is still running"""
        if self.process and self.process.poll() is None:
            self.process.kill()

    def stop(self):
        try:
            self.input.put(None)
            self.process.stdin.close()
            if self.process.poll() is None:
                self.process.kill()
        except:
            print "got an exception trying to end the bot"

    def run(self):
        """Run the bot loop.  Use run_step for individual steps"""
        try:
            if self.process.poll() is not None:
                raise Exception("bot did not start")
            while True:
                print('waiting for input')
                command = self.input.get(True)
                print('got %s' % list(command))
                try:
                    if command is None:
                        print('command is None')
                        self.process.kill()
                        break
                    # not sure if this is excesive poll()'ing...
                    if self.process.poll() is not None:
                        print('poll is not None')
                        self.output.put("CRASHED")
                        break
                    print('writing to stdin %s' % command)
                    self.process.stdin.write(command)
                    self.process.stdin.flush()
            
                    result = []
                    while True:
                        if self.process.poll() is not None:
                            print "process crashed"
                            self.output.put("CRASHED")
                            return
                        print('reading from stdout')
                        line = self.process.stdout.readline()
                        print('got line from stdout %s' % list(line))
                        if line is None:
                            print('got none from line')
                            self.output.put("CRASHED")
                            return
                        result.append(line)
                        if line.startswith("go"):
                            break
                    print('got all input')
                    self.output.put("\n".join(result) + '\n')
                except:
                    self.output("CRASHED WITH EXCEPTION")
        except:
            pass


class EngineThread(Thread):
    """
    Class to run a bot in an engine.

    Need another level of threads to wait for the bot result.

    """
    def __init__(self, bot):
        Thread.__init__(self)
        self.bot = bot
        self.input = Queue()
        self.output = Queue()
        self.event = threading.Event()
        self.daemon = False
        self.start()
        self.result = None

    def run(self):
        try:
            while True:
                x = self.input.get(True)
                if x is None: break
                command, timeout = x
                print('run step %s' % list(command))
                result = self.bot.run_step(command, timeout)
                self.output.put(result)
        except:
            pass

    def send_input(self, game_state, timeoutms):
        self.result = None
        print(game_state)
        self.input.put((game_state, timeoutms))

    def wait(self, timeoutms):
        if self.result is None:
            result = self.output.get(timeoutms/1000.0)
            if result:
                self.result = result
            else:
                self.result = ("TIMEOUTWAIT", False)
        return self.result

    def stop(self):
        self.input.put(None)
        self.bot.stop()

def run_game(game, bots, timeoutms, loadtimeoutms, num_turns=1000,
             output_file="testout.txt", verbose=False, serial=False):
    try:
        if output_file:
            of = open(output_file, "w")
            of.write(game.get_state())
            of.flush()

        for turn in xrange(num_turns+1):
            # get the moves from each player
            # if both crash, draw.  if one crashes, it loses
            try:
                timeout = timeoutms
                for b in range(len(bots)):
                    if game.is_alive(b):
                        bot = bots[b]
                        if turn == 0:
                            timeout = loadtimeoutms
                            bot.send_input(game.get_player_start(b), timeout)
                        else:
                            bot.send_input(game.get_player_state(b), timeout)
                        if serial:
                            bot.wait(timeoutms)

                moves = []
                oks = []
                for b in range(len(bots)):
                    if game.is_alive(b):
                        try:
                            move, ok = bot.wait(timeout)
                        except:
                            move, ok = "EXCEPT", False
                    else:
                        move, ok = "EXCEPT", False
                    moves.append(move)
                    oks.append(ok)

            except:
                print "Got an error running the bots."
                raise

            if sum(oks) == 0:
                return "Game Over, %s" % game.get_scores()

            # update the game state
            errors = game.do_all_moves(moves)

            if output_file:
                of.write(game.get_state())
                of.flush()

            if errors.count(None) == 0:
                return "Game Over, %s" % game.get_scores()

            if verbose:
                stats = game.get_stats()
                s = 'turn %4d stats: '
                for key, values in stats:
                    s += '%s: %s' % (key, values)
                sys.stderr.write("\r%-50s" % s)

            # check if they made bad moves
            alive = [game.is_alive(b) for b in range(len(bots))]
            if sum(alive) == 0:
                return "Game Over, %s" % game.get_scores()

        return "Game Over, %s" % game.get_scores()

    finally:
        if output_file:
            of.close()

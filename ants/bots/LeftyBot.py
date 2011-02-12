#!/usr/bin/env python
from random import choice, randrange
from ants import *
import sys
import logging
from optparse import OptionParser

DIRECTIONS = ['N','E','S','W']
DIRECTION = {'N': (0, -1),
             'E': (1, 0),
             'S': (0, 1),
             'W': (-1, 0)}
RIGHT = {'N': 'E',
         'E': 'S',
         'S': 'W',
         'W': 'N'}
LEFT = {'N': 'W',
        'E': 'N',
        'S': 'E',
        'W': 'S'}

class Lefty:
    def __init__(self):
        self.straight_ants = {}
        self.lefty_ants = {}

    def do_turn(self, ants):
        new_straight = {}
        new_lefty = {}
        ant_locations = ants.my_ants()
        for ant_x, ant_y in ant_locations:

            # send new ants in a random direction
            if (not (ant_x, ant_y) in self.straight_ants and
                    not (ant_x, ant_y) in self.lefty_ants):
                direction = choice(DIRECTIONS)
                self.straight_ants[(ant_x, ant_y)] = direction

            # send straight ants in same direction
            if (ant_x, ant_y) in self.straight_ants:
                direction = self.straight_ants[(ant_x, ant_y)]
                new_x = (ant_x + DIRECTION[direction][0]) % ants.width
                new_y = (ant_y + DIRECTION[direction][1]) % ants.height
                if ants.passable(new_x, new_y):
                    if (not (new_x, new_y) in new_straight and
                            not (new_x, new_y) in new_lefty and
                            not (new_x, new_y) in ant_locations):
                        new_straight[(new_x, new_y)] = direction
                        print('M %s %s %s' % (ant_x, ant_y, direction))
                        continue
                    else:
                        # have ant wait until it is clear
                        new_straight[(ant_x, ant_y)] = direction
                        continue
                else:
                    self.lefty_ants[(ant_x, ant_y)] = RIGHT[direction]

            # send lefty ants along the wall
            if (ant_x, ant_y) in self.lefty_ants:
                # check if ant got stuck in center of map
                if (ants.passable((ant_x + 1) % ants.width, ant_y) and
                        ants.passable((ant_x - 1) % ants.width, ant_y) and
                        ants.passable(ant_x, (ant_y + 1) % ants.height) and
                        ants.passable(ant_x, (ant_y - 1) % ants.height)):
                    continue
                direction = self.lefty_ants[(ant_x, ant_y)]
                # turn left
                direction = LEFT[direction]
                # look for a valid direction clockwise
                for i in range(4):
                    new_x = (ant_x + DIRECTION[direction][0]) % ants.width
                    new_y = (ant_y + DIRECTION[direction][1]) % ants.height
                    if ants.passable(new_x, new_y):
                        if (not (new_x, new_y) in new_straight and
                                not (new_x, new_y) in new_lefty and
                                not (new_x, new_y) in ant_locations):
                            new_lefty[(new_x, new_y)] = direction
                            print('M %s %s %s' % (ant_x, ant_y, direction))
                            break
                        else:
                            # have ant wait until it is clear
                            new_lefty[(ant_x, ant_y)] = direction
                            break
                    direction = RIGHT[direction] # try another direction

        # reset lists
        self.straight_ants = new_straight
        self.lefty_ants = new_lefty

def main():
    map_data = ''
    game = Ants()
    bot = Lefty()
    while(True):
        try:
            current_line = raw_input()
            if current_line.lower() == 'ready':
                game.setup(map_data)
                game.finish_turn()
                map_data = ''
            elif current_line.lower() == 'go':
                game.update(map_data)
                try:
                    bot.do_turn(game)
                except:
                    sys.stderr.write("Unexpected error in do_turn\n")
                game.finish_turn()
                map_data = ''
            else:
                map_data += current_line + '\n'
        except EOFError:
            break

if __name__ == '__main__':
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass
    try:
        main()
    except KeyboardInterrupt:
        print('ctrl-c, leaving ...')
    except:
        sys.stderr.write("Unexpected error in main\n")
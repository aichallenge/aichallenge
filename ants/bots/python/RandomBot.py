#!/usr/bin/env python
from random import shuffle
from ants import *
import sys

DIRECTIONS = ['N','E','S','W']
DIRECTION = {'N': (0, -1),
             'E': (1, 0),
             'S': (0, 1),
             'W': (-1, 0)}

def do_turn(ants):
    destinations = []
    for ant_x, ant_y in ants.my_ants():
        # try all directions randomly until one is passable and not occupied
        shuffle(DIRECTIONS)
        for direction in DIRECTIONS:
            new_x = (ant_x + DIRECTION[direction][0]) % ants.width
            new_y = (ant_y + DIRECTION[direction][1]) % ants.height
            if not (new_x, new_y) in destinations and ants.passable(new_x, new_y):
                ants.issue_order((ant_x, ant_y, direction))
                destinations.append((new_x, new_y))
                break
        else:
            destinations.append((ant_x, ant_y))

def main():
    map_data = ''
    game = Ants()
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
                    do_turn(game)
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

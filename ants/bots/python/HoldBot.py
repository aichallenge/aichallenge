#!/usr/bin/env python
from random import shuffle
from ants import *
import sys
import traceback

DIRECTIONS = ['N','E','S','W']
DIRECTION = {'N': (0, -1),
             'E': (1, 0),
             'S': (0, 1),
             'W': (-1, 0)}

def do_turn(ants):
    pass

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
                    traceback.print_exc(file=sys.stderr)
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
        traceback.print_exc(file=sys.stderr)

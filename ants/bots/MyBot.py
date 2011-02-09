#!/usr/bin/env python
import sys
from random import choice
from ants import *

def do_turn(ants):
    for ant_x, ant_y in ants.my_ants():
        print('%s %s %s' % (ant_x, ant_y, choice(['N', 'E', 'S', 'W'])))

def main():
    map_data = ''
    ants = Ants()
    while(True):
        try:
            current_line = raw_input()
            if current_line == 'ready':
                ants.setup(map_data)
                print('go fish')
                sys.stdout.flush()
                map_data = ''
            elif current_line == 'go':
                ants.update(map_data)
                try:
                    do_turn(ants)
                except:
                    sys.stderr.write("Unexpected error in do_turn()\n")
                    raise
                print('go')
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
        import traceback
        traceback.print_exc()
        sys.stderr.write("Unexpected error in main()\n")

#!/usr/bin/env python
from random import choice, randrange
from ants import *

def do_turn(ants):
    for ant_x, ant_y in ants.my_ants():
        print('%s %s %s' % (ant_x, ant_y, choice(['n', 'e', 's', 'w'])))

def main():
    map_data = ''
    ants = Ants()
    while(True):
        try:
            current_line = raw_input()
            if current_line.lower() == 'ready':
                ants.setup(map_data)
                ants.finish_turn()
                map_data = ''
            elif current_line.lower() == 'go':
                ants.update(map_data)
                try:
                    do_turn(ants)
                except:
                    ants.error("Unexpected error in do_turn")
                ants.finish_turn()
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
        Ants().error("Unexpected error in main")
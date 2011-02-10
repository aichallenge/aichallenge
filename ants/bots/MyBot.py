#!/usr/bin/env python
from random import choice, randrange
from ants import *

def do_turn(ants):
    for ant_x, ant_y in ants.my_ants():
        print('%s %s %s' % (ant_x, ant_y, choice(['n', 'e', 's', 'w'])))

def main(f):
    map_data = ''
    ants = Ants()
    while(True):
        try:
            current_line = raw_input()
            f.write('<< ' + current_line + '\n')
            f.flush()
            if current_line == 'ready':
                ants.setup(map_data)
                ants.finish_turn(f)
                map_data = ''
            elif current_line == 'go':
                ants.update(map_data)
                try:
                    do_turn(ants)
                except:
                    ants.error("Unexpected error in do_turn")
                ants.finish_turn(f)
                map_data = ''
            else:
                map_data += current_line + '\n'
        except EOFError:
            break

if __name__ == '__main__':
    f = open('MyBot%s.log' % randrange(100), 'w')
    f.write('starting MyBot\n')
    f.flush()
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass
    try:
        main(f)
    except KeyboardInterrupt:
        print('ctrl-c, leaving ...')
    except:
        Ants().error("Unexpected error in main")
    finally:
        f.close()
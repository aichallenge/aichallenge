#!/usr/bin/env python
from ants import *

class TestBot:
    def do_turn(self, ants):
        destinations = []
        for ant_loc in ants.my_ants():
            # try all directions in order
            directions = ('n','e','s','w')
            for direction in directions:
                new_loc = ants.destination(ant_loc, direction)
                if (not new_loc in destinations and
                        ants.passable(new_loc)):
                    ants.issue_order((ant_loc, direction))
                    destinations.append(new_loc)
                    break
            else:
                destinations.append(ant_loc)

if __name__ == '__main__':
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass
    try:
        Ants.run(TestBot())
    except KeyboardInterrupt:
        print('ctrl-c, leaving ...')

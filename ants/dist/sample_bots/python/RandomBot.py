#!/usr/bin/env python
from random import shuffle
from ants import *

class RandomBot:
    def do_turn(self, ants):
        destinations = []
        for a_loc in ants.my_ants():
            # try all directions randomly until one is passable and not occupied
            directions = AIM.keys()
            shuffle(directions)
            for direction in directions:
                n_loc = ants.destination(a_loc, direction)
                if (not n_loc in destinations and
                            ants.passable(n_loc)):
                    ants.issue_order((a_loc, direction))
                    destinations.append(n_loc)
                    break
            else:
                destinations.append(a_loc)

if __name__ == '__main__':
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass
    try:
        Ants.run(RandomBot())
    except KeyboardInterrupt:
        print('ctrl-c, leaving ...')
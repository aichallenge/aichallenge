#!/usr/bin/env python
from random import choice, randrange
from ants import *
import sys
import logging
from optparse import OptionParser

class LeftyBot:
    def __init__(self):
        self.ants_straight = {}
        self.ants_lefty = {}

    def do_turn(self, ants):
        destinations = []
        new_straight = {}
        new_lefty = {}
        for a_loc in ants.my_ants():
            (a_row, a_col) = a_loc
            # send new ants in a straight line
            if (not a_loc in self.ants_straight and
                    not a_loc in self.ants_lefty):
                if a_row % 2 == 0:
                    if a_col % 2 == 0:
                        direction = 'n'
                    else:
                        direction = 's'
                else:
                    if a_col % 2 == 0:
                        direction = 'e'
                    else:
                        direction = 'w'
                self.ants_straight[a_loc] = direction

            # send ants going in a straight line in the same direction
            if a_loc in self.ants_straight:
                direction = self.ants_straight[a_loc]
                n_loc = ants.destination(a_loc, direction)
                if ants.passable(n_loc):
                    if (ants.unoccupied(n_loc) and
                            not n_loc in destinations):
                        ants.issue_order((a_loc, direction))
                        new_straight[n_loc] = direction
                        destinations.append(n_loc)
                    else:
                        # pause ant, turn and try again next turn
                        new_straight[a_loc] = LEFT[direction]
                        destinations.append(a_loc)
                else:
                    # hit a wall, start following it
                    self.ants_lefty[a_loc] = RIGHT[direction]

            # send ants following a wall, keeping it on their left
            if a_loc in self.ants_lefty:
                direction = self.ants_lefty[a_loc]
                directions = [LEFT[direction], direction, RIGHT[direction], BEHIND[direction]]
                # try 4 directions in order, attempting to turn left at corners
                for new_direction in directions:
                    n_loc = ants.destination(a_loc, new_direction)
                    if ants.passable(n_loc):
                        if (ants.unoccupied(n_loc) and
                                not n_loc in destinations):
                            ants.issue_order((a_loc, new_direction))
                            new_lefty[n_loc] = new_direction
                            destinations.append(n_loc)
                            break
                        else:
                            # have ant wait until it is clear
                            new_straight[a_loc] = RIGHT[direction]
                            destinations.append(a_loc)
                            break

        # reset lists
        self.ants_straight = new_straight
        self.ants_lefty = new_lefty

if __name__ == '__main__':
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass
    try:
        Ants.run(LeftyBot())
    except KeyboardInterrupt:
        print('ctrl-c, leaving ...')
        
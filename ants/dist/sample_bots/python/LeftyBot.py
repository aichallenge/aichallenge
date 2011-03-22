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
        for a_row, a_col in ants.my_ants():
            # send new ants in a straight line
            if (not (a_row, a_col) in self.ants_straight and
                    not (a_row, a_col) in self.ants_lefty):
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
                self.ants_straight[(a_row, a_col)] = direction

            # send ants going in a straight line in the same direction
            if (a_row, a_col) in self.ants_straight:
                direction = self.ants_straight[(a_row, a_col)]
                n_row, n_col = ants.destination(a_row, a_col, direction)
                if ants.passable(n_row, n_col):
                    if (ants.unoccupied(n_row, n_col) and
                            not (n_row, n_col) in destinations):
                        ants.issue_order((a_row, a_col, direction))
                        new_straight[(n_row, n_col)] = direction
                        destinations.append((n_row, n_col))
                    else:
                        # pause ant, turn and try again next turn
                        new_straight[(a_row, a_col)] = LEFT[direction]
                        destinations.append((a_row, a_col))
                else:
                    # hit a wall, start following it
                    self.ants_lefty[(a_row, a_col)] = RIGHT[direction]

            # send ants following a wall, keeping it on their left
            if (a_row, a_col) in self.ants_lefty:
                direction = self.ants_lefty[(a_row, a_col)]
                directions = [LEFT[direction], direction, RIGHT[direction], BEHIND[direction]]
                # try 4 directions in order, attempting to turn left at corners
                for new_direction in directions:
                    n_row, n_col = ants.destination(a_row, a_col, new_direction)
                    if ants.passable(n_row, n_col):
                        if (ants.unoccupied(n_row, n_col) and
                                not (n_row, n_col) in destinations):
                            ants.issue_order((a_row, a_col, new_direction))
                            new_lefty[(n_row, n_col)] = new_direction
                            destinations.append((n_row, n_col))
                            break
                        else:
                            # have ant wait until it is clear
                            new_straight[(a_row, a_col)] = RIGHT[direction]
                            destinations.append((a_row, a_col))
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
        
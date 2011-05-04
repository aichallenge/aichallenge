#!/usr/bin/env python
from random import shuffle
from ants import *

class HunterBot():
    def do_turn(self, ants):
        destinations = []
        for a_loc in ants.my_ants():
            targets = ants.food() + [(row, col) for (row, col), owner in ants.enemy_ants()]
            # find closest food or enemy ant
            closest_target = None
            closest_distance = 999999
            for t_loc in targets:
                dist = ants.distance(a_loc, t_loc)
                if dist < closest_distance:
                    closest_distance = dist
                    closest_target = (t_loc)
            if closest_target == None:
                # no target found, mark ant as not moving so we don't run into it
                destinations.append(a_loc)
                continue
            directions = ants.direction(a_loc, closest_target)
            shuffle(directions)
            for direction in directions:
                n_loc = ants.destination(a_loc, direction)
                if ants.unoccupied(n_loc) and not n_loc in destinations:
                    destinations.append(n_loc)
                    ants.issue_order((a_loc, direction))
                    break
            else:
                # mark ant as not moving so we don't run into it
                destinations.append((a_loc))

if __name__ == '__main__':
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass
    try:
        Ants.run(HunterBot())
    except KeyboardInterrupt:
        print('ctrl-c, leaving ...')
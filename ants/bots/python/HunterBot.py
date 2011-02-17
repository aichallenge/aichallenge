#!/usr/bin/env python
from random import shuffle
from ants import *

DIRECTION = {'N': (0, -1),
             'E': (1, 0),
             'S': (0, 1),
             'W': (-1, 0)}

class Hunter():
    def do_turn(self, ants):
        destinations = []
        for ant_x, ant_y in ants.my_ants():
            targets = ants.food() + [(x, y) for (x, y), owner in ants.enemy_ants()]
            # find closest food or enemy ant
            closest_target = None
            closest_distance = 999999
            for tar_x, tar_y in targets:
                dx = ant_x - tar_x
                dy = ant_y - tar_y
                dist = ants.distance(ant_x, ant_y, tar_x, tar_y)
                if dist < closest_distance:
                    closest_distance = dist
                    closest_target = (tar_x, tar_y)
            if closest_target == None:
                # no target found, mark ant as not moving so we don't run into it
                destinations.append((ant_x, ant_y))
                continue
            directions = ants.direction(ant_x, ant_y, closest_target[0], closest_target[1])
            shuffle(directions)
            for direction in directions:
                new_x = (ant_x + DIRECTION[direction][0]) % ants.width
                new_y = (ant_y + DIRECTION[direction][1]) % ants.height
                if ants.passable(new_x, new_y) and not (new_x, new_y) in destinations:
                    destinations.append((new_x, new_y))
                    ants.give_order((ant_x, ant_y, direction))
                    break
            else:
                # mark ant as not moving so we don't run into it
                destinations.append((ant_x, ant_y))

def main():
    map_data = ''
    game = Ants()
    bot = Hunter()
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
                    bot.do_turn(game)
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

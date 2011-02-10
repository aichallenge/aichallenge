#!/usr/bin/env python
import sys
import traceback

MY_ANT = 0
ANTS = 0
LAND = -1
FOOD = -2
WALL = -3
CONFLICT = -4
UNSEEN = -5

MAP = '?!%*.abcdefghijklmnopqrstuvwxyz'

class Ants():
    def __init__(self):
        self.width = None
        self.height = None
        self.map = None
        self.ant_list = {}
        self.food_list = []

    def setup(self, data):
        for line in data.split('\n'):
            if len(line) > 0:
                tokens = line.split()
                if tokens[0].upper() == 'D':
                    self.width = int(tokens[1])
                    self.height = int(tokens[2])

    def update(self, data):
        self.map = [[MAP.index(x) - 5 for x in y]
                        for y in data[:-1].split('\n')]
        self.ant_list = {}
        self.food_list = []
        for y in range(self.height):
            for x in range(self.width):
                if self.map[y][x] >= ANTS:
                    self.ant_list[(x,y)] = self.map[y][x]
                elif self.map[y][x] == FOOD:
                    self.food_list.append((x,y))

    def finish_turn(self):
        sys.stdout.write('go\n')
        sys.stdout.flush()

    def error(self, msg):
        traceback.print_exc(file=sys.stderr)
        sys.stderr.write(msg + '\n')
        sys.stderr.flush()

    def my_ants(self):
        return [(x, y) for (x, y), owner in self.ant_list.items()
                    if owner == MY_ANT]
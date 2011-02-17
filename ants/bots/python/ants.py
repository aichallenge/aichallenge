#!/usr/bin/env python
import sys
import traceback
import random

MY_ANT = 0
ANTS = 0
LAND = -1
FOOD = -2
WALL = -3
DEAD = -4
UNSEEN = -5

MAP_RENDER = 'abcdefghijklmnopqrstuvwxyz?!%*.'
MAP_READ = '?!%*.abcdefghijklmnopqrstuvwxyz'

class Ants():
    def __init__(self):
        self.width = None
        self.height = None
        self.map = None
        self.ant_list = {}
        self.food_list = []
        self.dead_list = []
        self.update = self.update_changes

    def setup(self, data):
        for line in data.split('\n'):
            line = line.strip().upper()
            if len(line) > 0:
                tokens = line.split()
                key = tokens[0]
                if key == 'WIDTH':
                    self.width = int(tokens[1])
                elif key == 'HEIGHT':
                    self.height = int(tokens[1])
        self.map = [[UNSEEN for col in range(self.width)]
                    for row in range(self.height)]

    def update_changes(self, data):
        # clear ant and food data
        for (col, row), owner in self.ant_list.items():
            self.map[row][col] = LAND
        self.ant_list = {}
        for col, row in self.food_list:
            self.map[row][col] = LAND
        self.food_list = []
        for col, row in self.dead_list:
            self.map[row][col] = LAND
        self.dead_list = []

        # update map and create new ant and food lists
        for line in data.split('\n'):
            line = line.strip().upper()
            if len(line) > 0:
                tokens = line.split()
                if len(tokens) >= 3:
                    col = int(tokens[1])
                    row = int(tokens[2])
                    if tokens[0] == 'A':
                        owner = int(tokens[3])
                        self.map[row][col] = owner
                        self.ant_list[(col, row)] = owner
                    elif tokens[0] == 'F':
                        self.map[row][col] = FOOD
                        self.food_list.append((col, row))
                    elif tokens[0] == 'L':
                        self.map[row][col] = LAND
                    elif tokens[0] == 'W':
                        self.map[row][col] = WALL
                    elif tokens[0] == 'D':
                        self.map[row][col] = DEAD

    def update_map(self, data):
        self.map = [[MAP_READ.index(x) - 5 for x in y]
                        for y in data[:-1].split('\n')]
        self.ant_list = {}
        self.food_list = []
        for y in range(self.height):
            for x in range(self.width):
                if self.map[y][x] >= ANTS:
                    self.ant_list[(x,y)] = self.map[y][x]
                elif self.map[y][x] == FOOD:
                    self.food_list.append((x,y))

    def issue_order(self, order):
        sys.stdout.write('O %s %s %s\n' % (order[0], order[1], order[2]))
        sys.stdout.flush()
        
    def finish_turn(self):
        sys.stdout.write('go\n')
        sys.stdout.flush()

    def my_ants(self):
        return [(x, y) for (x, y), owner in self.ant_list.items()
                    if owner == MY_ANT]

    def enemy_ants(self):
        return [((x, y), owner) for (x, y), owner in self.ant_list.items()
                    if owner != MY_ANT]

    def food(self):
        return self.food_list[:]

    def passable(self, x, y):
        return self.map[y][x] in (LAND, DEAD)

    def distance(self, x1, y1, x2, y2):
        x1 = x1 % self.width
        x2 = x2 % self.width
        y1 = y1 % self.height
        y2 = y2 % self.height
        d_x = min(abs(x1 - x2), self.width - abs(x1 - x2))
        d_y = min(abs(y1 - y2), self.height - abs(y1 - y2))
        return d_x + d_y

    def direction(self, x1, y1, x2, y2):
        d = []
        x1 = x1 % self.width
        x2 = x2 % self.width
        y1 = y1 % self.height
        y2 = y2 % self.height
        if x1 < x2:
            if x2 - x1 >= self.width//2:
                d.append('W')
            if x2 - x1 <= self.width//2:
                d.append('E')
        if x2 < x1:
            if x1 - x2 >= self.width//2:
                d.append('E')
            if x1 - x2 <= self.width//2:
                d.append('W')

        if y1 < y2:
            if y2 - y1 >= self.height//2:
                d.append('N')
            if y2 - y1 <= self.height//2:
                d.append('S')
        if y2 < y1:
            if y1 - y2 >= self.height//2:
                d.append('S')
            if y1 - y2 <= self.height//2:
                d.append('N')
        return d

    def render_text(self):
        tmp = ''
        for row in self.map:
            tmp += '# %s\n' % ''.join([MAP_RENDER[col] for col in row])
        return tmp

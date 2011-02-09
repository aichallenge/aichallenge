#!/usr/bin/env python

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
        self.map = [[MAP.index(col) - 5 for col in row]
                        for row in data[:-1].split('\n')]
        self.ant_list = {}
        self.food_list = []
        for row in range(self.height):
            for col in range(self.width):
                if self.map[row][col] >= ANTS:
                    self.ant_list[(col,row)] = self.map[row][col]
                elif self.map[row][col] == FOOD:
                    self.food_list.append((col,row))
        
    def my_ants(self):
        return [(col, row) for (col, row), owner in self.ant_list.items()
                    if owner == MY_ANT]
    
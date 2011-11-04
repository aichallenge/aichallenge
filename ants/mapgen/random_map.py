#!/usr/bin/env python
from map import *
from random import choice, randint, randrange, shuffle, seed

class RandomMap(Map):
    def __init__(self, options={}):
        self.name = 'random'
        self.rows = options.get('rows', (40,120))
        self.cols = options.get('cols', (40,120))
        self.players = options.get('players', (2,26))
        self.land = options.get('land', (80, 95))

    def generate(self):
        rows = self.get_random_option(self.rows)
        cols = self.get_random_option(self.cols)
        players = self.get_random_option(self.players)
        land = self.get_random_option(self.land)

        # initialize map
        self.map = [[LAND]*cols for _ in range(rows)]

        # place water
        water = rows*cols*(100-land)//100
        row = 0
        col = 0
        for _ in range(water):
            while self.map[row][col] == WATER:
                row = randint(0, rows-1)
                col = randint(0, cols-1)
            self.map[row][col] = WATER

        # place player starts
        for player in range(players):
            while self.map[row][col] != LAND:
                row = randint(0, rows-1)
                col = randint(0, cols-1)
            self.map[row][col] = player

def main():
    new_map = RandomMap()
    new_map.generate()
    new_map.toText()

if __name__ == '__main__':
    main()

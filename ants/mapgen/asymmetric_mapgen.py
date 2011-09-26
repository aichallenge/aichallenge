#!/usr/bin/env python

import math
import random
from collections import deque

#functions
def gcd(a, b):
    while b:
        a, b = b, a%b
    return a

def lcm(a, b):
    if a == 0 and b == 0:
        return 0
    else:
        return abs(a*b)/gcd(a,b)

#map class
class AsymmetricMap():
    cdirections = ['N', 'E', 'S', 'W']
    directions = {'N': (-1,0), 'S': (1,0), 'E': (0,1), 'W': (0,-1)}

    #game parameters
    min_players = 4
    max_players = 10
    min_dim = 70
    max_dim = 120
    min_start_distance = 30

    min_land_proportion = 0.6
    max_land_proportion = 0.9

    #map parameters
    no_players = 0
    rows = cols = 0
    row_t = col_t = 0
    land_squares = 0
    map_data = []
    basis = []
    a_loc = c_locs = []
    no_extra_walks = 70


    #makes a map by carving it out of water
    def random_walk_map(self):
        self.pick_dimensions()
        self.map_data = [ ['%' for c in range(self.cols)] for r in range(self.rows) ]

        self.add_ants()
        self.basis = self.get_basis()

        self.start_walks()
        self.add_land()

    def random_map(self):
        self.pick_dimensions()
        self.map_data = [ ['%' for c in range(self.cols)] for r in range(self.rows) ]
        self.add_ants()
        self.basis = self.get_basis()
        self.add_random_land()

    #outputs the map
    def print_map(self):
        print "rows", self.rows
        print "cols", self.cols
        print "players", self.no_players
        for row in self.map_data:
            print 'm', ''.join(row)

    #picks the dimensions of the map
    def pick_dimensions(self):
        while True:
            while True:
                self.rows = random.randint(self.min_dim, self.max_dim)
                self.cols = random.randint(self.min_dim, self.max_dim)

                self.row_t = random.randint(3, self.rows-3)
                self.col_t = random.randint(3, self.cols-3)

                #makes sure no two players start in the same row or column
                if self.rows/gcd(self.row_t, self.rows) == self.cols/gcd(self.col_t, self.cols):
                    break

            self.no_players = lcm(self.rows/gcd(self.row_t, self.rows),
                                  self.cols/gcd(self.col_t, self.cols) )

            #forces a valid number of players all starting at a valid distance
            if self.no_players >= self.min_players and self.no_players <= self.max_players and self.is_valid_start():
                break

    #returns the distance between two squares
    def distance(self, loc1, loc2):
        d1 = abs(loc1[0] - loc2[0])
        d2 = abs(loc1[1] - loc2[1])
        dr = min(d1, self.rows - d1)
        dc = min(d2, self.cols - d2)
        return math.sqrt(dr*dr + dc*dc)

    #randomly picks a location inside the map
    def pick_square(self):
        return [random.randint(0, self.rows-1), random.randint(0, self.cols-1)]

    #starts two random walks from the players starting ants
    def start_walks(self):
        self.c_locs = [self.a_loc, self.a_loc]
        for w in range(self.no_extra_walks):
            c_loc = self.pick_square()
            for n in range(self.no_players):
                self.c_locs.append(self.pick_square())
                self.c_loc = self.get_translate_loc(c_loc)

    #randomly walks the ants in a direction
    def walk_ants(self):
        #walks the locations in a random direction
        for c in range(len(self.c_locs)):
            d = self.cdirections[random.randint(0, 3)]
            n_loc = self.get_loc(self.c_locs[c], d)
            self.c_locs[c] = n_loc

    #returns the new location after moving in a particular direction
    def get_loc(self, loc, direction):
        dr, dc = self.directions[direction]
        return [(loc[0]+dr)%self.rows, (loc[1]+dc)%self.cols ]

    #returns the new location after translating it by (rtranslate, ctranslate)
    def get_translate_loc(self, loc):
        return [(loc[0]+self.row_t)%self.rows,
                (loc[1]+self.col_t)%self.cols ]

    #returns the new location after translating it by (rtranslate, ctranslate)
    def get_translate_loc2(self, loc, row_t, col_t):
        return [(loc[0]+row_t)%self.rows,
                (loc[1]+col_t)%self.cols ]


    #fills in all symmetrically equivalent squares with the given type
    def fill_squares(self, loc, type):
        value = type
        for n in range(self.no_players):
            self.map_data[loc[0] ][loc[1] ] = value
            if type == '0':
                value = chr(ord(value)+1)
            loc = self.get_translate_loc(loc)

    #checks whether the players start far enough apart
    def is_valid_start(self):
        loc = n_loc = [0,0]
        for n in range(self.no_players-1):
            n_loc = self.get_translate_loc(n_loc)
            if self.distance(loc, n_loc) < self.min_start_distance:
                return False
        return True

    #checks whether the players can reach every non-wall square
    def is_valid(self):
        start_loc = self.a_loc
        visited = [ [False for c in range(self.cols)] for r in range(self.rows)]
        visited[start_loc[0] ][start_loc[1] ] = True
        squaresVisited = 1

        stack = [start_loc]
        while stack != []:
            c_loc = stack.pop()
            for d in self.directions:
                n_loc = self.get_loc(c_loc, d)

                if visited[n_loc[0] ][n_loc[1] ] == False and self.map_data[n_loc[0] ][n_loc[1] ] != '%':
                    stack.append(n_loc)
                    visited[n_loc[0] ][n_loc[1] ] = True
                    squaresVisited += 1

        if squaresVisited == self.land_squares:
            return True
        return False

    #adds ants to the map
    def add_ants(self):
        self.land_squares = self.no_players
        self.a_loc = self.c_loc = self.pick_square()
        self.fill_squares(self.a_loc, '0')

        for c_loc in self.c_locs:
            self.land_squares += self.no_players
            self.fill_squares(c_loc, '.')

    #returns a basis for the squares under translation
    def get_basis(self):
        visited = [ [False for c in range(self.cols)] for r in range(self.rows)]
        visited[self.a_loc[0]][self.a_loc[1]] = True

        basis = [self.a_loc]
        queue = deque([self.a_loc])
        while len(queue) > 0:
            c_loc = queue.popleft()
            for d in self.directions:
                n_loc = self.get_loc(c_loc, d)

                if visited[n_loc[0] ][n_loc[1] ] == False:
                    basis.append(n_loc)
                    queue.append(n_loc)
                    for n in range(self.no_players):
                        visited[n_loc[0] ][n_loc[1] ] = True
                        n_loc = self.get_translate_loc(n_loc)

        return basis

    #randomly adds land to the map
    def add_random_land(self):
        no_land_squares = random.randint(int(self.min_land_proportion*self.rows*self.cols),
                                          int(self.max_land_proportion*self.rows*self.cols))

        while self.land_squares < no_land_squares or not self.is_valid():
            for w in range(self.no_extra_walks):
                for n in range(self.no_players):
                    c_loc = self.basis[random.randint(0, len(self.basis)-1)]
                    c_loc = self.get_translate_loc2(c_loc, n*self.row_t, n*self.col_t)
                    while self.map_data[c_loc[0]][c_loc[1]] != '%':
                        c_loc = self.pick_square()
                        c_loc = self.get_translate_loc2(c_loc, n*self.row_t, n*self.col_t)

                    self.map_data[c_loc[0]][c_loc[1]] = '.'
                    self.land_squares += 1

    #adds land to a map of water
    def add_land(self):
        no_land_squares = random.randint(int(self.min_land_proportion*self.rows*self.cols),
                                          int(self.max_land_proportion*self.rows*self.cols))

        while self.land_squares < no_land_squares or not self.is_valid():
            self.walk_ants()

            for c_loc in self.c_locs:
                if self.map_data[c_loc[0]][c_loc[1]] == '%':
                    self.land_squares += 1
                    self.map_data[c_loc[0]][c_loc[1]] = '.'

if __name__ == '__main__':
    example_map = AsymmetricMap()
    example_map.random_walk_map()
    example_map.print_map()


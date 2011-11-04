#!/usr/bin/env python

import math
import random
from collections import deque
import sys
from optparse import OptionParser

#direction information
cdirections = ['N', 'E', 'S', 'W']
directions = {'N': (-1,0), 'S': (1,0), 'E': (0,1), 'W': (0,-1)}

#game parameters
min_players = 2
max_players = 8

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
class Grid():
    #sets up a grid with valid parameters for tile symmetry
    def tile_symmetric_grid(self, no_players,
                                  min_dimensions, max_dimensions,
                                  min_starting_distance,
                                  min_block_size, max_block_size):
        self.no_players = no_players
        self.min_dimensions = min_dimensions
        self.max_dimensions = max_dimensions
        self.min_starting_distance = min_starting_distance
        self.min_block_size = min_block_size
        self.max_block_size = max_block_size

        if not self.pick_tile_dimensions():
            return False

        self.squares = [ ['%' for c in range(self.cols)] for r in range(self.rows) ]

        self.add_starting_hills()
        a_block = self.make_block(self.h_loc, self.block_size)
        self.add_block_land(a_block)
        return True

    #sets up a grid with valid parameters for rotational symmetry
    def rotationally_symmetric_grid(self, no_players,
                                          min_dimensions, max_dimensions,
                                          min_starting_distance,
                                          min_block_size, max_block_size,
                                          r_sym_type):
        self.no_players = no_players
        self.min_dimensions = min_dimensions
        self.max_dimensions = max_dimensions
        self.r_sym_type = r_sym_type
        self.min_starting_distance = min_starting_distance
        self.min_block_size = min_block_size
        self.max_block_size = max_block_size

        if not self.pick_rotational_dimensions():
            return False

        self.squares = [ ['%' for c in range(self.cols)] for r in range(self.rows) ]

        self.add_starting_hills()
        a_block = self.make_block(self.h_loc, self.block_size)
        self.add_block_land(a_block)
        return True

    #picks valid dimensions for a tile symmetric grid
    def pick_tile_dimensions(self):
        original_no_players = self.no_players
        for d_attempt in range(200000):
            self.block_size = random.randint(self.min_block_size, self.max_block_size)
            self.rows = random.randint(self.min_dimensions, self.max_dimensions)
            self.cols = random.randint(self.rows, self.max_dimensions)
            self.rows += 2*self.block_size - self.rows%(2*self.block_size)
            self.cols += 2*self.block_size - self.cols%(2*self.block_size)

            self.row_t = random.randint(3, self.rows-3)
            self.col_t = random.randint(3, self.cols-3)

            if original_no_players == -1:
                self.no_players = lcm(self.rows/gcd(self.row_t, self.rows),
                                      self.cols/gcd(self.col_t, self.cols))

            self.h_loc = self.random_loc()

            if self.rows <= self.max_dimensions and \
               self.cols <= self.max_dimensions and \
               self.no_players == lcm(self.rows/gcd(self.row_t, self.rows),
                                  self.cols/gcd(self.col_t, self.cols) ) and \
               self.no_players >= min_players and \
               self.no_players <= max_players and\
               self.rows/gcd(self.row_t, self.rows) == \
                    self.cols/gcd(self.col_t, self.cols) and \
               self.row_t%(2*self.block_size) == 0 and \
               self.col_t%(2*self.block_size) == 0 and \
               self.is_valid_start():
                return True
        return False

    #picks valid dimensions for a rotationally symmetric grid
    def pick_rotational_dimensions(self):
        original_no_players = self.no_players
        original_r_sym_type = self.r_sym_type
        for d_attempt in range(100):
            #picks number of players if it is not given
            if original_no_players == -1:
                if original_r_sym_type > 3:
                    self.no_players = 2
                elif original_r_sym_type > 1:
                    self.no_players = 2**random.randint(1,2)
                else:
                    self.no_players = 2**random.randint(1,3)

            #picks a symmetry type if one is not given
            if original_r_sym_type == -1:
                if self.no_players == 2:
                    self.r_sym_type = random.randint(1, 5)
                elif self.no_players == 4:
                    self.r_sym_type = random.randint(1, 3)
                elif self.no_players == 8:
                    self.r_sym_type = 1;


            self.block_size = random.randint(self.min_block_size, self.max_block_size)
            self.rows = random.randint(self.min_dimensions, self.max_dimensions)
            self.cols = random.randint(self.rows, self.max_dimensions)
            self.rows += 2*self.block_size - self.rows%(2*self.block_size)
            self.cols += 2*self.block_size - self.cols%(2*self.block_size)

            if (self.no_players == 2 and self.r_sym_type > 3) or \
               (self.no_players == 4 and self.r_sym_type > 1) or \
                self.no_players == 8:
                self.cols = self.rows

            visited = [ [False for c in range(self.cols)] for r in range(self.rows)]
            for a_attempt in range(2*self.rows):
                while True:
                    self.h_loc = self.random_loc()
                    if not visited[self.h_loc[0]][self.h_loc[1]]:
                        break

                visited[self.h_loc[0]][self.h_loc[1]] = True

                if self.rows <= self.max_dimensions and \
                   self.cols <= self.max_dimensions and \
                   self.is_valid_start():
                    return True
        return False

    #works out a list of loctations that generates the set of locations under the given symmetry
    def generate_basis_information(self):
        self.basis_locs = []
        self.is_basis_block = [ [False for c in range(self.cols)] for r in range(self.rows)]
        self.is_basis_loc = [ [False for c in range(self.cols)] for r in range(self.rows)]
        visited = [ [False for c in range(self.cols)] for r in range(self.rows)]

        a_block = self.make_block(self.h_loc, self.block_size)

        queue = deque([a_block[0]])

        self.is_basis_block[a_block[0][0]][a_block[0][1]] = True
        for loc in a_block:
            self.is_basis_loc[loc[0]][loc[1]] = True
            self.basis_locs.append(loc)
            s_locs = self.get_symmetric_locs(loc)
            for s_loc in s_locs:
                visited[s_loc[0]][s_loc[1]] = True

        while queue:
            c_loc = queue.popleft()
            c_block = self.make_block(c_loc, self.block_size)

            for d in directions:
                n_block = self.get_adjacent_block(c_block, d)
                n_loc = n_block[0]
                if not visited[n_loc[0]][n_loc[1]]:
                    queue.append(n_loc)

                    self.is_basis_block[n_loc[0]][n_loc[1]] = True
                    for loc in n_block:
                        self.is_basis_loc[loc[0]][loc[1]] = True
                        self.basis_locs.append(loc)
                        s_locs = self.get_symmetric_locs(loc)
                        for s_loc in s_locs:
                            visited[s_loc[0]][s_loc[1]] = True

    #returns a list of directions in random order
    def random_directions(self):
        r_directions = []
        t = random.randint(0, 3)
        for i in range(len(directions)):
            r_directions.append(cdirections[(i+t)%4])
        return r_directions

    #randomly picks a location inside the map
    def random_loc(self):
        return [random.randint(0, self.rows-1), random.randint(0, self.cols-1)]

    #returns the new location after moving in a particular direction
    def get_loc(self, loc, direction):
        dr, dc = directions[direction]
        return [(loc[0]+dr)%self.rows, (loc[1]+dc)%self.cols ]

    #returns the new location after translating it by t_amount = [rt, ct]
    def get_translate_loc(self, loc, t_amount):
        return [(loc[0]+t_amount[0])%self.rows,
                (loc[1]+t_amount[1])%self.cols ]

    #returns a symmetrically equivalent location as specified by num
    def get_symmetric_loc(self, loc, num):
        if num == 1:   #horizontal
            return [loc[0], self.cols - loc[1]-1]
        elif num == 2: #vertical
            return [self.rows - loc[0]-1, loc[1]]
        elif num == 3: #horizontal and vertial
            return [self.rows - loc[0]-1, self.cols - loc[1]-1]
        elif num == 4: #diagonal/transpose
            return [loc[1], loc[0]]
        elif num == 5: # horizontal then vertical then diagonal
            return [self.rows - loc[1]-1, self.cols - loc[0]-1]
        elif num == 6: # horizontal then diagonal
            return [self.rows - loc[1]-1, loc[0]]
        elif num == 7: # vertical then diagonal
            return [loc[1], self.cols-loc[0]-1]

    #returns a list of the symmetric locations for all players
    def get_symmetric_locs(self, loc):
        locs = [loc]

        if self.symmetry == "tile":
            n_loc = loc
            for n in range(self.no_players-1):
                n_loc = self.get_translate_loc(n_loc, [self.row_t, self.col_t])
                locs.append(n_loc)
        elif self.symmetry == "rotational":
            if self.no_players == 2:
                locs.append(self.get_symmetric_loc(loc, self.r_sym_type))
            elif self.no_players == 4:
                if self.r_sym_type == 1:
                    locs.append(self.get_symmetric_loc(loc, 1))
                    locs.append(self.get_symmetric_loc(loc, 2))
                    locs.append(self.get_symmetric_loc(loc, 3))
                elif self.r_sym_type == 2:
                    locs.append(self.get_symmetric_loc(loc, 3))
                    locs.append(self.get_symmetric_loc(loc, 4))
                    locs.append(self.get_symmetric_loc(loc, 5))
                elif self.r_sym_type == 3:
                    locs.append(self.get_symmetric_loc(loc, 3))
                    locs.append(self.get_symmetric_loc(loc, 6))
                    locs.append(self.get_symmetric_loc(loc, 7))
            elif self.no_players == 8:
                for n in range(self.no_players-1):
                    locs.append(self.get_symmetric_loc(loc, n+1))
        return locs

    #makes a block inside the map
    def make_block(self, loc, block_size):
        block = []

        for row_t in range(block_size):
            for col_t in range(block_size):
                block.append(self.get_translate_loc(loc, [row_t, col_t]))
        return block

    #returns the new block after moving in a particular direction
    def get_block(self, block, direction):
        n_block = []
        for loc in block:
            n_block.append(self.get_loc(loc, direction))
        return n_block

    #returns the adjacent block in a given direction
    def get_adjacent_block(self, block, direction):
        for n in range(int(math.sqrt(len(block)))):
            block = self.get_block(block, direction)
        return block


    #returns the euclidean distance (squared) between two squares
    def dist(self, loc1, loc2):
        d1 = abs(loc1[0] - loc2[0])
        d2 = abs(loc1[1] - loc2[1])
        dr = min(d1, self.rows - d1)
        dc = min(d2, self.cols - d2)
        return dr*dr + dc*dc

    #checks whether the players start far enough apart
    def is_valid_start(self):
        h_locs = self.get_symmetric_locs(self.h_loc)
        for n in range(self.no_players-1):
            if self.dist(h_locs[0], h_locs[n+1]) < self.min_starting_distance:
                return False
        return True

    #checks whether the hills start far enough apart
    def is_valid_hill_loc(self, h_loc):
        if self.squares[h_loc[0]][h_loc[1]] != '.':
            return False

        h_locs = self.get_symmetric_locs(h_loc)
        for n in range(len(h_locs)-1):
            if self.dist(h_locs[0], h_locs[n+1]) < self.min_starting_distance:
                return False

        for c_loc in self.h_locs:
            if self.dist(c_loc, h_loc) < self.min_starting_distance:
                return False
        return True

    #adds land information to the grid
    def add_land(self, loc):
        if self.squares[loc[0]][loc[1]] == '%':
            self.squares[loc[0]][loc[1]] = '.'

    #add land information for a block
    def add_block_land(self, block):
        for loc in block:
            self.add_land(loc)

    #adds ants to the map
    def add_starting_hills(self):
        h_locs = self.get_symmetric_locs(self.h_loc)
        player = '0'
        for n in range(self.no_players):
            self.squares[h_locs[n][0]][h_locs[n][1]] = player
            player = chr(ord(player)+1)

    #adds extra hills to the map
    def add_extra_hills(self):
        self.h_locs = self.get_symmetric_locs(self.h_loc)

        for h in range(self.no_hills-1):
            for d_attempt in range(100):
                h_loc = self.random_loc()
                if self.is_valid_hill_loc(h_loc):
                    break

            if not self.is_valid_hill_loc(h_loc):
                return

            player = '0'
            h_locs = self.get_symmetric_locs(h_loc)
            for n in range(self.no_players):
                self.squares[h_locs[n][0]][h_locs[n][1]] = player
                self.h_locs.append(h_locs[n])
                player = chr(ord(player)+1)

    #outputs the grid in the expected format
    def print_grid(self):
        print "rows", self.rows
        print "cols", self.cols
        print "players", self.no_players
        #self.print_food_spawn_info()
        for row in self.squares:
            print 'm', ''.join(row)

    #adds land to a water map using backtracking "recursively"
    def add_land_with_recursive_backtracking(self):
        stack = []
        c_loc = self.h_loc
        c_block = self.make_block(c_loc, self.block_size)
        visited = [ [False for c in range(self.cols)] for r in range(self.rows)]

        while True:
            visited[c_loc[0]][c_loc[1]] = True
            neighbour_found = False

            r_directions = self.random_directions()
            for d in r_directions:
                n_block = self.get_adjacent_block(c_block, d)
                n_loc = n_block[0]

                if not self.is_basis_block[n_loc[0]][n_loc[1]]: #can't carve here
                    continue

                t_block = self.get_adjacent_block(n_block, d)
                t_loc = t_block[0]
                f_loc = t_block[0]
                f_block = t_block

                if not self.is_basis_block[t_loc[0]][t_loc[1]]:
                    f_loc = c_loc
                    f_block = self.make_block(c_loc, self.block_size)

                if not visited[t_loc[0]][t_loc[1]]:
                    if self.is_basis_block[t_loc[0]][t_loc[1]]:
                        stack.append(c_loc)
                        self.add_block_land(n_block)
                        self.add_block_land(f_block)
                    elif random.randint(1,3) == 1:
                        self.add_block_land(n_block)

                    c_loc = f_loc
                    c_block = self.make_block(c_loc, self.block_size)
                    neighbour_found = True
                    visited[t_loc[0]][t_loc[1]] = True
                    break

            if not neighbour_found:
                if stack:
                    c_loc = stack.pop()
                    c_block = self.make_block(c_loc, self.block_size)
                else:
                    break

    #adds extra land blocks to the map
    def add_extra_land_blocks(self):
        extra_locs = random.randint(2, 12)
        for extra_loc in range(extra_locs):
            block_found = False
            for b_attempt in range(100):
                c_block = self.make_block(self.h_loc, self.block_size)

                r_directions = self.random_directions()
                for d in r_directions:
                    n_block = self.get_adjacent_block(c_block, d)
                    if self.is_basis_block[n_block[0][0]][n_block[0][1]]:
                        c_block = n_block
                        break

                for i in range(15):
                    r_directions = self.random_directions()
                    for d in r_directions:
                        n_block = c_block
                        n_block = self.get_adjacent_block(n_block, d)
                        n_block = self.get_adjacent_block(n_block, d)
                        if self.is_basis_block[n_block[0][0]][n_block[0][1]]:
                            c_block = n_block
                            break

                if self.squares[c_block[0][0]][c_block[0][1]] == '%':
                    for d in directions:
                        n_block = self.get_adjacent_block(c_block, d)
                        if self.is_basis_block[n_block[0][0]][n_block[0][1]] and\
                           self.squares[n_block[0][0]][n_block[0][1]] == '.':
                            block_found = True
                            break
                    if block_found:
                        break

            if not block_found:
                return
            for loc in c_block:
                if self.is_basis_loc[loc[0]][loc[1]]:
                    self.add_land(loc)

    #adds extra land locations to the map
    def add_extra_land_locs(self):
        visited = [ [False for c in range(self.cols)] for r in range(self.rows)]
        w_locs = []

        stack = [self.h_loc]
        visited[self.h_loc[0]][self.h_loc[1]] = True

        while stack:
            c_loc = stack.pop()
            for d in directions:
                n_loc = self.get_loc(c_loc, d)

                if not visited[n_loc[0]][n_loc[1]]:
                    if self.is_basis_loc[n_loc[0]][n_loc[1]] and \
                       self.squares[n_loc[0]][n_loc[1]] == '%':
                            w_locs.append(n_loc)
                    elif self.squares[n_loc[0]][n_loc[1]] == '.':
                        stack.append(n_loc)

                    visited[n_loc[0]][n_loc[1]] = True

        locs_to_add = int(0.5*len(w_locs))
        for w in range(locs_to_add):
            r_square = random.randint(0, len(w_locs)-1)
            self.add_land(w_locs[r_square])
            w_locs.remove(w_locs[r_square])
            if len(w_locs) == 0:
                break

    #makes the map symmetric
    def make_symmetric(self):
        for loc in self.basis_locs:
            if self.squares[loc[0]][loc[1]] == '.':
                s_locs = self.get_symmetric_locs(loc)
                for s_loc in s_locs:
                    self.add_land(s_loc)

    #randomly translates the map
    def translate(self):
        old_map = [ ['%' for c in range(self.cols)] for r in range(self.rows) ]
        for r in range(self.rows):
            for c in range(self.cols):
                old_map[r][c] = self.squares[r][c]

        t_loc = [random.randint(1, self.rows-2), random.randint(1, self.cols-2)]

        self.h_loc = self.get_translate_loc(self.h_loc, t_loc)

        for r in range(self.rows):
            for c in range(self.cols):
                o_loc = self.get_translate_loc([r,c], t_loc)
                self.squares[r][c] = old_map[o_loc[0]][o_loc[1]]

def main(argv):
    usage ="""Usage: %prog [options]\n"""
    parser = OptionParser(usage=usage)
    parser.add_option("--no_players", dest="no_players",
                      type="int", default=-1,
                      help="Minimum number of players to be used")

    parser.add_option("--min_hills", dest="min_hills",
                      type="int", default=1,
                      help="Minimum number of hills for each player")

    parser.add_option("--max_hills", dest="max_hills",
                      type="int", default=9,
                      help="Maximum number of hills for each player")

    parser.add_option("--min_dimensions", dest="min_dimensions",
                      type="int", default=60,
                      help="Minimum number of rows/cols to be used")
    parser.add_option("--max_dimensions", dest="max_dimensions",
                      type="int", default=150,
                      help="Maximum number of rows/cols to be used")

    parser.add_option("--min_starting_distance", dest="min_starting_distance",
                      type="int", default=10**2,
                      help="Minimum starting distance between ants")

    parser.add_option("--symmetry", dest="symmetry",
                      type="string", default="",
                      help="Type of symmetry to be used")
    parser.add_option("--rotational_symmetry", dest="rotational_symmetry",
                      type="int", default=-1,
                      help="Number of players to be used")

    parser.add_option("--min_block_size", dest="min_block_size",
                      type="int", default=3,
                      help="Minimum block size to be used")
    parser.add_option("--max_block_size", dest="max_block_size",
                      type="int", default=4,
                      help="Maximum block size to be used")
    parser.add_option("--seed", dest="seed",
                      type="int", default=None,
                      help="Seed to initialize the random number generator.")

    (opts,_) = parser.parse_args(argv)

    #makes sure the parameters are valid
    if (opts.no_players < min_players and opts.no_players != -1)\
            or opts.no_players > max_players:
        print "Invalid number of players"
        return
    if opts.min_hills < 1 or opts.max_hills < opts.min_hills:
        print "Invalid min/max number of hills per player"
        return
    if opts.min_dimensions < 1 or opts.max_dimensions < opts.min_dimensions:
        print "Invalid min/max dimensions parameters"
        return
    if opts.min_block_size < 1 or opts.max_block_size < opts.min_block_size:
        print "Invalid min/max block size parameters"
        return
    if opts.symmetry == "rotational":
        if opts.no_players != -1 and opts.no_players != 2 and\
           opts.no_players != 4 and opts.no_players != 8:
            print "Invalid number of players for a rotationally symmetric map"
            return
        if opts.rotational_symmetry != -1:
            if (opts.no_players == 2 and (opts.rotational_symmetry < 1 or \
                                          opts.rotational_symmetry > 5))  \
                or (opts.no_players == 4 and (opts.rotational_symmetry < 1 or \
                                              opts.rotational_symmetry > 3))  \
                or (opts.no_players == 8 and opts.rotational_symmetry != 1)   \
                or (opts.rotational_symmetry < 0 or opts.rotational_symmetry > 5):
                print "Invalid rotational symmetry type for the number of players"
                return
    
    random.seed(opts.seed)

    #creates the map
    grid = Grid()

    #works out how many hills to have
    grid.no_hills = random.randint(opts.min_hills, opts.max_hills)

    #works out the type of symmetry
    if opts.symmetry == "rotational":
        grid.symmetry = "rotational"
    elif opts.symmetry == "tile":
        grid.symmetry = "tile"
    elif opts.symmetry:
        print "invalid symmetry type"
        return
    else:
        if (opts.no_players == -1 or opts.no_players%2 == 0) \
                and random.randint(0,5):
            grid.symmetry = "rotational"
        else:
            grid.symmetry = "tile"

    #constructs a water filled grid
    if grid.symmetry == "rotational":
        if not grid.rotationally_symmetric_grid(opts.no_players,
                                         opts.min_dimensions, opts.max_dimensions,
                                         opts.min_starting_distance,
                                         opts.min_block_size, opts.max_block_size,
                                         opts.rotational_symmetry):
            print "Failed to create a valid rotationally symmetric grid with", \
                         opts.no_players, "players"
            return

    elif grid.symmetry == "tile":
        if not grid.tile_symmetric_grid(opts.no_players,
                                        opts.min_dimensions, opts.max_dimensions,
                                        opts.min_starting_distance,
                                        opts.min_block_size, opts.max_block_size):
            print "Failed to create a valid tile symmetric grid with", \
                         opts.no_players, "players and block size", grid.block_size
            return

    grid.generate_basis_information()
    grid.add_land_with_recursive_backtracking()
    grid.add_extra_land_blocks()
    grid.add_extra_land_locs()
    grid.make_symmetric()
    grid.add_extra_hills()
    grid.translate() #this will make it (even) harder to determine some symmetries

    grid.print_grid()

if __name__ == '__main__':
    main(sys.argv[1:])


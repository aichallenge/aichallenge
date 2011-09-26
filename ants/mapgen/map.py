#!/usr/bin/env python
import sys
from random import randint, choice, seed
from collections import deque
from itertools import product
from sys import maxint

MY_ANT = 0
ANTS = 0
DEAD = -1
LAND = -2
FOOD = -3
WATER = -4
UNSEEN = -5
MAP_RENDER = '0123456789?%*.!'

AIM = {'n': (-1, 0),
       'e': (0, 1),
       's': (1, 0),
       'w': (0, -1)}

class Map(object):
    def __init__(self, options={}):
        super(Map, self).__init__()
        self.name = "blank"
        self.map = [[]]
        self.random_seed = options.get('seed', None)
        if self.random_seed == None:
            self.random_seed = randint(-maxint-1, maxint)
        seed(self.random_seed)

    def generate(self):
        raise Exception("Not Implemented")

    def get_random_option(self, option):
        if type(option) == tuple:
            if len(option) == 2:
                return randint(*option)
            elif len(option) == 1:
                return option[0]
            elif len(option) == 0:
                raise Exception("Invalid option: 0 length tuple")
            else:
                return choice(option)
        elif type(option) in (list, set):
            if len(option) > 0:
                return choice(option)
            else:
                raise Exception("Invalid option: 0 length list")
        elif type(option) in (int, float, str):
            return option
        else:
            raise Exception("Invalid option: type {0} not supported".format(type(option)))

    def toPNG(self, fd=sys.stdout):
        raise Exception("Not Implemented")

    def toText(self, fd=sys.stdout):
        players = set()
        for row in self.map:
            for c in row:
                if c >= ANTS:
                    players.add(c)
        fd.write("# map_type {0}\n# random_seed {1}\nplayers {2}\nrows {3}\ncols {4}\n"
                 .format(self.name,
                         self.random_seed,
                         len(players),
                         len(self.map),
                         len(self.map[0])))
        for row in self.map:
            fd.write("m {0}\n".format(''.join([MAP_RENDER[c] for c in row])))

    def manhatten_distance(self, loc1, loc2, size):
        rows, cols = size
        row1, col1 = loc1
        row2, col2 = loc2
        row1 = row1 % rows
        row2 = row2 % rows
        col1 = col1 % cols
        col2 = col2 % cols
        d_col = min(abs(col1 - col2), cols - abs(col1 - col2))
        d_row = min(abs(row1 - row2), rows - abs(row1 - row2))
        return d_row + d_col

    def euclidean_distance2(self, loc1, loc2, size):
        rows, cols = size
        row1, col1 = loc1
        row2, col2 = loc2
        row1 = row1 % rows
        row2 = row2 % rows
        col1 = col1 % cols
        col2 = col2 % cols
        d_col = min(abs(col1 - col2), cols - abs(col1 - col2))
        d_row = min(abs(row1 - row2), rows - abs(row1 - row2))
        return d_row**2 + d_col**2

    def destination(self, loc, direction, size):
        rows, cols = size
        row, col = loc
        d_row, d_col = AIM[direction]
        return ((row + d_row) % rows, (col + d_col) % cols)

    def section(self, block_size=1):
        rows = len(self.map)
        cols = len(self.map[0])
        visited = [[False] * cols for _ in range(rows)]

        def is_block_free(loc):
            row, col = loc
            for d_row in range(-block_size, block_size+1):
                for d_col in range(-block_size, block_size+1):
                    h_row = (row + d_row) % rows
                    h_col = (col + d_col) % cols
                    if self.map[h_row][h_col] == WATER:
                        return False
            return True

        def mark_block(loc, m, ilk):
            row, col = loc
            for d_row in range(-block_size, block_size+1):
                for d_col in range(-block_size, block_size+1):
                    h_row = (row + d_row) % rows
                    h_col = (col + d_col) % cols
                    m[h_row][h_col] = ilk

        def find_open_spot():
            for row, col in product(range(rows), range(cols)):
                if is_block_free((row, col)) and not visited[row][col]:
                    return (row, col)
            else:
                return None

        # list of contiguous areas
        areas = []

        # flood fill map for each separate area
        while find_open_spot():
            # maintain lists of visited and seen squares
            # visited will not overlap, but seen may
            area_visited = [[False] * cols for _ in range(rows)]
            area_seen = [[False] * cols for _ in range(rows)]

            squares = deque()
            row, col = find_open_spot()

            #seen_area = open_block((row, col))
            squares.appendleft((row, col))

            while len(squares) > 0:
                row, col = squares.pop()
                visited[row][col] = True
                area_visited[row][col] = True
                area_seen[row][col] = True
                for d_row, d_col in ((1,0), (0,1), (-1,0), (0,-1)):
                    s_row = (row + d_row) % rows
                    s_col = (col + d_col) % cols
                    if not visited[s_row][s_col] and is_block_free((s_row, s_col)):
                        visited[s_row][s_col] = True
                        mark_block((s_row, s_col), area_seen, True)
                        squares.appendleft((s_row, s_col))

            # check percentage filled
            #areas.append(1.0 * seen_area / land_area)
            visited_list = []
            seen_list = []
            for row in range(rows):
                for col in range(cols):
                    if area_visited[row][col]:
                        visited_list.append((row, col))
                    elif area_seen[row][col]:
                        seen_list.append((row, col))
            areas.append([visited_list, seen_list])

        # sort by largest area first
        areas.sort(key=lambda area: len(area[0]), reverse=True)
        return areas

    def fill_small_areas(self):
        # keep largest contiguous area as land, fill the rest with water
        count = 0
        areas = self.section(0)
        for area in areas[1:]:
            for row, col in area[0]:
                self.map[row][col] = WATER
                count += 1
        #print("fill {0}".format(count))

    def make_wider(self):
        # make sure the map has more columns than rows
        rows = len(self.map)
        cols = len(self.map[0])
        if rows > cols:
            map = [[LAND] * rows for _ in range(cols)]
            for row in range(rows):
                for col in range(cols):
                    map[col][row] = self.map[row][col]
            self.map = map

    def tile(self, grid):
        rows = len(self.map)
        cols = len(self.map[0])
        row_sym, col_sym = grid

        # select random mirroring
        row_mirror = 0
        if row_sym % 2 == 0:
            #if row_sym % 4 == 0:
            #    row_mirror = choice((0,4))
            row_mirror = choice((row_mirror, 2))
            row_mirror = 2

        col_mirror = 0
        if col_sym % 2 == 0:
            #if col_sym % 4 == 0:
            #    col_mirror = choice((0,4))
            col_mirror = choice((col_mirror, 2))
            col_mirror = 2

        # perform tiling
        t_rows = rows * row_sym
        t_cols = cols * col_sym
        ant = 0
        map = [[LAND]*t_cols for _ in range(t_rows)]
        for t_row in range(t_rows):
            for t_col in range(t_cols):
                # detect grid location
                g_row = t_row // rows
                g_col = t_col // cols
                if row_mirror == 2 and g_row % 2 == 1:
                    row = rows - 1 - (t_row % rows)
                else:
                    row = t_row % rows
                if col_mirror == 2 and g_col % 2 == 1:
                    col = cols - 1 - (t_col % cols)
                else:
                    col = t_col % cols
                try:
                    map[t_row][t_col] = self.map[row][col]
                except:
                    print("issue")
                if self.map[row][col] == ANTS:
                    map[t_row][t_col] = ant
                    ant += 1
        self.map = map

    def translate(self, offset):
        d_row, d_col = offset
        rows = len(self.map)
        cols = len(self.map[0])
        map = [[LAND] * cols for _ in range(rows)]
        for row in range(rows):
            for col in range(cols):
                o_row = (d_row + row) % rows
                o_col = (d_col + col) % cols
                map[o_row][o_col] = self.map[row][col]
        self.map = map

    def allowable(self):
        # all squares must be accessible from all other squares
        # fill small areas can fix this
        areas = self.section(0)
        if len(areas) > 1:
            return "Map not 100% accessible"
        land_area = len(areas[0][0])

        # 66% of the map must not be blockable
        # or must be accessable by a 3x3 block
        areas = self.section(1)
        area_visited, area_seen = areas[0]
        if 1.0 * (len(area_seen) + len(area_visited)) / land_area < 0.66:
            return "Map is too blockable"

        # all starting ants must be in the largest area
        ants = {}
        rows = len(self.map)
        cols = len(self.map[0])
        for row in range(rows):
            for col in range(cols):
                if self.map[row][col] >= ANTS:
                    if (row, col) not in area_seen and (row, col) not in area_visited:
                        return "Starting ants not in unblockable area"
                    ants[(row, col)] = self.map[row][col]

        return None

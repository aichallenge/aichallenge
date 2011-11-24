#!/usr/bin/env python
import sys
from random import randint, choice, seed
from collections import deque
from itertools import product
try:
    from sys import maxint
except ImportError:
    from sys import maxsize as maxint
from copy import deepcopy
import heapq
from collections import defaultdict

MY_ANT = 0
ANTS = 0
DEAD = -1
LAND = -2
FOOD = -3
WATER = -4
UNSEEN = -5

PLAYER_ANT = 'abcdefghij'
HILL_ANT = string = 'ABCDEFGHIJ'
PLAYER_HILL = string = '0123456789'
MAP_OBJECT = '?%*.!'
MAP_RENDER = PLAYER_HILL + HILL_ANT + PLAYER_ANT + MAP_OBJECT
ALLOWABLE = list(range(30)) + [LAND, FOOD, WATER]

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
        for r, row in enumerate(self.map):
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

    def get_distances(self, start_loc, end_locs, size):
        'get a list of distances from 1 location to a bunch of others'
        end_locs = end_locs[:]
        rows, cols = size
        visited = {}
        open_nodes = deque()
        open_nodes.append((start_loc, 0))
        if start_loc in end_locs:
            yield open_nodes[-1]
            end_locs.remove(start_loc)
        while open_nodes and len(end_locs) > 0:
            (row, col), dist = open_nodes.popleft()
            for n_loc in [((row + 1) % rows, col),
                          ((row - 1) % rows, col),
                          (row, (col + 1) % cols),
                          (row, (col - 1) % cols)]:
                if n_loc not in visited:
                    if self.map[n_loc[0]][n_loc[1]] != WATER:
                        new_dist = dist + 1
                        visited[n_loc] = new_dist
                        open_nodes.append((n_loc, new_dist))
                        if n_loc in end_locs:
                            #print('# get distances: {0}'.format(open_nodes[-1]))
                            yield open_nodes[-1]
                            end_locs.remove(n_loc)
                    else:
                        visited[n_loc] = None
    
    def get_path(self, loc1, loc2, size, block=1, ignore=None):
        'get path from 1 location to another as a list of locations'
        # make a node class to calc F, G and H automatically
        def nodeMaker(distance=self.manhatten_distance, dest=loc2, size=size):
            class Node:
                def __init__(self, loc, parent, G):
                    self.loc = loc
                    self.parent = parent
                    self.G = G
                    self.H = distance(loc, dest, size)
                    self.F = self.G + self.H
                def __lt__(self, other):
                    return self.F < other.F
                    
            return Node
        Node = nodeMaker()
        
        # heap list to help get lowest F cost
        open_nodes = []
        # lists indexed by location
        closed_list = {}
        open_list = {}
        block_offsets = list(product(range(block), range(block)))
        
        def add_open(node, open_nodes=open_nodes, open_list=open_list):
            heapq.heappush(open_nodes, (node.F, node))
            open_list[node.loc] = node
        
        def get_open(open_nodes=open_nodes, open_list=open_list):
            _, node = heapq.heappop(open_nodes)
            del open_list[node.loc]
            return node
        
        def replace_open(new_node, open_nodes=open_nodes, open_list=open_list):
            old_node = open_list[new_node.loc]
            open_nodes.remove((old_node.F, old_node))
            heapq.heapify(open_nodes)
            add_open(new_node)

        def build_path(node):
            path = []
            while node:
                path.append(node.loc)
                node = node.parent
            path.reverse()
            return path            
        
        # A* search
        # add starting square to open list
        if block > 1:
            # find open block position on starting location
            for o_loc in product(range(-block+1,1), range(-block+1,1)):
                s_loc = self.dest_offset(loc1, o_loc, size)
                for d_loc in block_offsets:
                    b_row, b_col = self.dest_offset(s_loc, d_loc, size)
                    if self.map[b_row][b_col] == WATER and (ignore is None or (b_row, b_col) not in ignore):
                        break
                else:
                    # no water found, use this start location
                    add_open(Node(s_loc, None, 0))
                    break
        else:
            add_open(Node(loc1, None, 0))
        while len(open_nodes) > 0:
            # get lowest F cost node
            node = get_open()
            # switch to closed list
            closed_list[node.loc] = node
            # check if found distination
            if node.loc == loc2:
                # build path
                return build_path(node)
            # expand node
            for d in AIM:
                loc = self.destination(node.loc, d, size)
                # ignore if closed or not traversable
                # ignore list is a set of locations to assume as traversable
                # block is a block size that must fit, the loc is the upper left corner
                if loc in closed_list:
                    continue
                skip = False
                for d_loc in block_offsets:
                    b_row, b_col = self.dest_offset(loc, d_loc, size)
                    if self.map[b_row][b_col] == WATER and (ignore is None or (b_row, b_col) not in ignore):
                        skip = True
                        break
                if skip:
                    continue
                    
                if loc in open_list:
                    old_node = open_list[loc]
                    # check for shortest path
                    if node.G + 1 < old_node.G:
                        # replace node
                        replace_open(Node(loc, node, node.G + 1))
                else:
                    # add to open list
                    add_open(Node(loc, node, node.G + 1))
        return None
                                        
    def destination(self, loc, direction, size):
        rows, cols = size
        row, col = loc
        d_row, d_col = AIM[direction]
        return ((row + d_row) % rows, (col + d_col) % cols)

    def dest_offset(self, loc, d_loc, size):
        rows, cols = size
        d_row, d_col = d_loc
        row, col = loc
        return ((row + d_row) % rows, (col + d_col) % cols)
    
    def section(self, block_size=1):
        '''split map into sections that can be travesered by a block
        
        block_size 1 is a 3x3 block (1 step each direction)'''
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

    def offset_aim(self, offset, aim):
        """ Return proper offset given an orientation
        """
        # eight possible orientations
        row, col = offset
        if aim == 0:
            return offset
        elif aim == 1:
            return -row, col
        elif aim == 2:
            return row, -col
        elif aim == 3:
            return -row, -col
        elif aim == 4:
            return col, row
        elif aim == 5:
            return -col, row
        elif aim == 6:
            return col, -row
        elif aim == 7:
            return -col, -row

    def map_similar(self, loc1, loc2, aim, player):
        """ find if map is similar given loc1 aim of 0 and loc2 ant of player
            return a map of translated enemy locations
        """
        enemy_map = {}
        rows = len(self.map)
        cols = len(self.map[0])
        size = (rows, cols)
        for row in range(rows):
            for col in range(cols):
                row0, col0 = self.dest_offset(loc1, (row, col), size)
                row1, col1 = self.dest_offset(loc2, self.offset_aim((row, col), aim), size)
                # compare locations
                ilk0 = self.map[row0][col0]
                ilk1 = self.map[row1][col1]
                if ilk0 == 0 and ilk1 != player:
                    # friendly ant not in same location
                    return None
                elif ilk0 > 0 and (ilk1 < 0 or ilk1 == player):
                    # enemy ant not in same location
                    return None
                elif ilk0 < 0 and ilk1 != ilk0:
                    # land or water not in same location
                    return None
                if ilk0 >= 0 and enemy_map != None:
                    enemy_map[ilk0] = ilk1
        return enemy_map

    def get_map_symmetry(self):
        """ Get orientation for each starting hill
        """
        size = (len(self.map), len(self.map[0]))
        # build list of all hills
        player_hills = defaultdict(list) # list of hills for each player
        for row, squares in enumerate(self.map):
            for col, square in enumerate(squares):
                if square >= 0:
                    player_hills[square].append((row, col))
        # list of
        #     list of tuples containing
        #         location, aim, and enemy map dict
        orientations = [[(player_hills[0][0], 0,
            dict([(i, i, ) for i in range(self.players)]))]]
        for player in range(1, self.players):
            if len(player_hills[player]) != len(player_hills[0]):
                raise Exception("Invalid map",
                                "This map is not symmetric.  Player 0 has {0} hills while player {1} has {2} hills."
                                .format(len(player_hills[0]), player, len(player_hills[player])))
            new_orientations = []
            for player_hill in player_hills[player]:
                for aim in range(8):
                # check if map looks similar given the orientation
                    enemy_map = self.map_similar(player_hills[0][0], player_hill, aim, player)
                    if enemy_map != None:
                        # produce combinations of orientation sets
                        for hill_aims in orientations:
                            new_hill_aims = deepcopy(hill_aims)
                            new_hill_aims.append((player_hill, aim, enemy_map))
                            new_orientations.append(new_hill_aims)
            orientations = new_orientations
            if len(orientations) == 0:
                raise Exception("Invalid map",
                                "This map is not symmetric. Player {0} does not have an orientation that matches player 0"
                                .format(player))
        # ensure types of hill aims in orientations are symmetric
        # place food set and double check symmetry
        valid_orientations = []
        for hill_aims in orientations:
            fix = []
            for loc, aim, enemy_map in hill_aims:
                row, col = self.dest_offset(loc, self.offset_aim((1,2), aim), size)
                fix.append(((row, col), self.map[row][col]))
                self.map[row][col] = FOOD
            for loc, aim, enemy_map in hill_aims:
                if self.map_similar(hill_aims[0][0], loc, aim, enemy_map[0]) is None:
                    break
            else:
                valid_orientations.append(hill_aims)
            for (row, col), ilk in reversed(fix):
                self.map[row][col] = ilk
        if len(valid_orientations) == 0:
            raise Exception("Invalid map",
                            "There are no valid orientation sets")
        return valid_orientations
                
    def allowable(self, check_sym=True, check_dist=True):
        # Maps are limited to at most 200x200 squares
        size = (len(self.map), len(self.map[0]))
        if size[0] > 200 or size[1] > 200:
            return "Map is too large"
                
        # Maps are limited to 10 players
        for row, squares in enumerate(self.map):
            for col, square in enumerate(squares):
                if square not in ALLOWABLE:
                    return "Maps are limited to 10 players and must contain the following characters: A-Ja-j0-9.*%"
                
        # Maps must be symmetric
        if check_sym:
            try:
                self.get_map_symmetry()
            except Exception as e:
                raise
                return "Map is not symmetric" + str(e)
        
        # Hills must be between 24 and 250 steps away from other hills (details still being worked on)
        if check_dist:
            hills = {}
            for row, squares in enumerate(self.map):
                for col, square in enumerate(squares):
                    if square >= 0 and square < 20:
                        owner = square % 10
                        loc = (row, col)
                        hills[loc] = owner
                        for hill_loc, hill_owner in hills.items():
                            if hill_owner != owner:
                                # check distance
                                hill_dist = len(self.get_path(loc, hill_loc, size))
                                if hill_dist is None:
                                    return "Map has hills without a path between them"
                                elif hill_dist < 24:
                                    return "Map has hills too close"
                                elif hill_dist > 250:
                                    return "Map has hills too far apart"
                            

        # Maps must not contain islands
        # all squares must be accessible from all other squares
        # fill small areas can fix this
        areas = self.section(0)
        if len(areas) > 1:
            area_visited, area_seen = areas[0]
            for loc in area_visited:
                if self.map[loc[0]][loc[1]] == LAND:
                    self.map[loc[0]][loc[1]] = UNSEEN                
            return "Map not 100% accessible"
        land_area = len(areas[0][0])

        # Maps must be mostly traversable by a 3x3 block
        # 66% of the map must not be blockable
        # or must be accessable by a 3x3 block
        areas = self.section(1)
        area_visited, area_seen = areas[0]
        if 1.0 * (len(area_seen) + len(area_visited)) / land_area < 0.66:
            for loc in area_visited:
                if self.map[loc[0]][loc[1]] == LAND:
                    self.map[loc[0]][loc[1]] = UNSEEN
            return "Map is too blockable: %s" % str(1.0 * (len(area_seen) + len(area_visited)) / land_area)

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

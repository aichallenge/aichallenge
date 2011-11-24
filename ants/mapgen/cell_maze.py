#!/usr/bin/python
from __future__ import print_function
import sys

def log(msg):
    msg = '# ' + str(msg) + '\n'
    sys.stderr.write(msg)

def report(msg):
    msg = '# ' + str(msg) + '\n'
    sys.stdout.write(msg)
    
from random import randrange, random, choice, seed, shuffle, sample, paretovariate, betavariate
from math import sqrt
#import Image, ImageDraw, ImageChops
from itertools import combinations
from collections import defaultdict

from map import *
euclidean_distance = None

class CellMazeMap(Map):
    def __init__(self, options={}):
        super(CellMazeMap, self).__init__(options)
        report('random seed {0}'.format(self.random_seed))
        self.name = 'cell_maze'
        self.players = self.get_random_option(options.get('players', max(2, min(10, int((betavariate(2.5, 3.0) * 10) + 1)))))
        report('players {0}'.format(self.players))
        self.area = self.get_random_option(options.get('area', self.players * randrange(400, 40000 / self.players)))
        report('area {0}'.format(self.area))
        self.cell_width = self.get_random_option(options.get('cell_width', min(paretovariate(2), 7.0)))
        report('cell width: {0}'.format(self.cell_width))
        self.cell_size = self.get_random_option(options.get('cell_size', int(min(paretovariate(2) + 4.0, 20.0) + int(self.cell_width) + 1)))
        report('cell size: {0}'.format(self.cell_size))
        self.openness = self.get_random_option(options.get('openness', betavariate(1.0, 3.0)))
        report('openness: {0}'.format(self.openness))
                                    
    def generate(self):

        def distance(x1, y1, x2, y2, width, height):
            x1 = x1 % width
            x2 = x2 % width
            y1 = y1 % height
            y2 = y2 % height
            d_x = min(abs(x1 - x2), width - abs(x1 - x2))
            d_y = min(abs(y1 - y2), height - abs(y1 - y2))
            return d_x**2 + d_y**2
        
        def mid_point(loc1, loc2, size):
            if loc1 == (31,24) and loc2 == (3,25):
                pass
            row1, col1 = loc1
            row2, col2 = loc2
            rows, cols = size
            row1, row2 = sorted((row1, row2))
            col1, col2 = sorted((col1, col2))
            m_row = (row1 + row2)//2
            if row2 - row1 > rows//2:
                m_row = (m_row + rows//2) % rows
            m_col = (col1 + col2)//2
            if col2 - col1 > cols//2:
                m_col = (m_col + cols//2) % cols
            return m_row, m_col
        
        def row_distance(row1, row2, rows):
            return min(abs(row1-row2),rows-abs(row1-row2))
        
        def col_distance(col1, col2, cols):
            return min(abs(col1-col2),cols-abs(col1-col2))
        
        def manhatten_distance(loc1, loc2, size):
            row1, col1 = loc1
            row2, col2 = loc2
            rows, cols = size
            d_row = min(abs(row1-row2),rows-abs(row1-row2))
            d_col = min(abs(col1-col2),cols-abs(col1-col2))
            return d_row + d_col
        
        def chebychev_distance(loc1, loc2, size):
            row1, col1 = loc1
            row2, col2 = loc2
            rows, cols = size
            d_row = min(abs(row1-row2),rows-abs(row1-row2))
            d_col = min(abs(col1-col2),cols-abs(col1-col2))
            return max(d_row, d_col)
        
        # calc distance table for size of matrix
        euclidean_cache = {}
        def euclidean_distance(loc1, loc2, size):
            key = (loc1, loc2, size)
            if key in euclidean_cache:
                return euclidean_cache[key]
            row1, col1 = loc1
            row2, col2 = loc2
            rows, cols = size
            d_row = min(abs(row1-row2),rows-abs(row1-row2))
            d_col = min(abs(col1-col2),cols-abs(col1-col2))
            #key = (d_row, d_col)
            #if key in euclidean_cache:
            #    return euclidean_cache[key]
            value = sqrt(d_row**2 + d_col**2)
            euclidean_cache[key] = value
            return value

        # calc distance table for size of matrix
        euclidean_cache_2 = {}
        def euclidean_distance_2(loc1, loc2, size):
            key = (loc1, loc2, size)
            if key in euclidean_cache_2:
                return euclidean_cache_2[key]
            row1, col1 = loc1
            row2, col2 = loc2
            rows, cols = size
            d_row = min(abs(row1-row2),rows-abs(row1-row2))
            d_col = min(abs(col1-col2),cols-abs(col1-col2))
            #key = (d_row, d_col)
            #if key in euclidean_cache:
            #    return euclidean_cache[key]
            value = d_row**2 + d_col**2
            euclidean_cache_2[key] = value
            return value
        
        def destination(loc, d, size):
            """ Returns the location produced by offsetting loc by d """
            return ((loc[0] + d[0]) % size[0], (loc[1] + d[1]) % size[1])
            
        def copy(value, size, step):
            return size+value
        def mirror(value, size, step):
            return size*2-value-1
        def flip(value, size, step):
            return size-value-1
        def shear(value, size, step):
            return (size//step[1] * step[0] + value) % size
        
        def both_point(point, size, step, funcs):
            vert_func, horz_func = funcs
            return (vert_func(point[0], size[0], step), horz_func(point[1], size[1], step))
        def vert_point(point, size, step, funcs):
            return (funcs[0](point[0], size[0], step), point[1])
        def horz_point(point, size, step, funcs):
            return (point[0], funcs[0](point[1], size[1], step))
        # TODO: ensure square or change output size
        def flip_point(point, size, funcs):
            return (funcs[0](point[1], size[1]), funcs[1](point[0], size[0]))
        
        def vert_increase(size, count):
            return (size[0]*count, size[1])
        def horz_increase(size, count):
            return (size[0], size[1]*count)
        
        vert_copy = (vert_point, (copy,), vert_increase)
        vert_shear = (both_point, (copy, shear), vert_increase)
        vert_mirror = (vert_point, (mirror,), vert_increase)
        vert_rotate = (both_point, (mirror, flip), vert_increase)
        horz_copy = (horz_point, (copy,), horz_increase)
        horz_shear = (both_point, (shear, copy), horz_increase)
        horz_mirror = (horz_point, (mirror,), horz_increase)
        horz_rotate = (both_point, (flip, mirror), horz_increase)    
        extend_report = {vert_copy: "vert_copy",
                         vert_shear: "vert_shear",
                         vert_mirror: "vert_mirror",
                         vert_rotate: "vert_rotate",
                         horz_copy: "horz_copy",
                         horz_shear: "horz_shear",
                         horz_mirror: "horz_mirror",
                         horz_rotate: "horz_rotate"}
        def extend(funcs, points, size, count=2, step=0, shear=1):
            # shear is used for a MxN grid where shearing occurs in both directions
            point_func, trans_funcs, increase_func = funcs
            report(extend_report[funcs])
            if type(points) == list:
                points = {point: x for x, point in enumerate(points)}
            #rows, cols = size
            new_points = {}
            for point, id in points.items():
                new_points[point] = id
                for c in range(1,count):
                    new_points[point_func(point,                               # point
                                          increase_func(size, c),              # size
                                          ((c * step) % count, count * shear), # step
                                          trans_funcs)] = id                   # vert/horz funtions
            return new_points, increase_func(size, count)
                      
        def make_symmetric(points, size, players, grid):
            row_sym, col_sym = grid
            # TODO: fix shearing in both directions for 2x2 and 3x3
            # TODO: 90 degree rotate

            #print_points(points, size)
            step = 0
            if row_sym % 2 == 0 and random() < 0.5:
                points, size = extend(choice((vert_mirror, vert_rotate)), points, size)
                if row_sym//2 > 1:
                    points, size = extend(vert_copy, points, size, row_sym//2)
            elif row_sym > 1:
                # pick random step for shearing
                # value of 0 is no shearing
                step = randrange(row_sym)
                if step > 0:
                    report("vert shear step: {0}".format(step))                
                points, size = extend(vert_shear if step > 0 else vert_copy, points, size, row_sym, step)
            #print_points(points, size)
        
            if col_sym % 2 == 0 and random() < 0.5:
                points, size = extend(choice((horz_mirror, horz_rotate)), points, size)
                if col_sym//2 > 1:
                    points, size = extend(horz_copy, points, size, col_sym//2)
            elif col_sym > 1:
                # pick random step for shearing, except of vert shearing has occured
                # value of 0 is no shearing
                if step > 0:
                    step = 0
                else:
                    step = randrange(col_sym)
                if step > 0:
                    report("horz shear step: {0}".format(step))     
                points, size = extend(horz_shear if step > 0 else horz_copy, points, size, col_sym, step, row_sym)
            #print_points(points, size)
            
            return points, size, grid
        
        def random_points(count, size, spacing):
            spacing_2 = spacing ** 2
            # ensure each random point is on a unique row and col
            rows, cols = size
            # matrix of available spots left
            matrix = {row: list(range(cols)) for row in range(rows)}
            
            # offsets for removing points
            offsets = []
            for d_row in range(-spacing, spacing+1):
                for d_col in range(-spacing, spacing+1):
                    if d_row ** 2 + d_col ** 2 <= spacing_2:
                        offsets.append((d_row, d_col))
            
            def remove_point(loc):
                for d_row, d_col in offsets:
                    n_row, n_col = destination(loc, (d_row, d_col), size)
                    if n_row in matrix:
                        if n_col in matrix[n_row]:
                            matrix[n_row].remove(n_col)
                        if len(matrix[n_row]) == 0:
                            del matrix[n_row]
                                    
            points = []
            for _ in range(count):
                row = choice(list(matrix.keys()))
                col = choice(matrix[row])
                point = (row, col)
                points.append(point)
                remove_point(point)
        
                if len(matrix) == 0:
                    break
                
            return points
        
        def random_points_unique(count, size, spacing, distance):
            # ensure each random point is on a unique row and col
            rows, cols = size
            # matrix of available spots left
            matrix = {row: list(range(cols)) for row in range(rows)}
            # lists of available rows and cols, will be reset
            avail_rows = list(matrix.keys())
            avail_cols = list(set([col for cols in matrix.values() for col in cols]))
            
            def remove_point(loc):
                row, col = loc
                for d_row in range(-spacing, spacing+1):
                    for d_col in range(-spacing, spacing+1):
                        n_loc = destination(loc, (d_row, d_col), size)
                        if distance(loc, n_loc, size) < spacing:
                            n_row, n_col = n_loc
                            if n_row in matrix:
                                if n_col in matrix[n_row]:
                                    matrix[n_row].remove(n_col)
                                if len(matrix[n_row]) == 0:
                                    del matrix[n_row]
                                    
            points = []
            failures = 0
            for _ in range(count):
                while True:
                    row = choice(avail_rows)
                    col = choice(list(set(avail_cols) & set(matrix[row])))
                    point = (row, col)
#                    for other_point in points:
#                        if distance(point, other_point, size) < spacing:
#                            # remove individual point
#                            sys.exit(1)
#                            failures += 1
#                            if failures > 100000:
#                                return points
#                            break
#                    else:
#                        break
                points.append(point)
                remove_point(point)
                
                # remove row and cols from lists
                avail_rows.remove(point[0])
                avail_rows = list(set(avail_rows) & set(matrix.keys()))
                avail_cols.remove(point[1])
                avail_cols = list(set(avail_cols) & set([col for cols in matrix.values() for col in cols]))
        
                # reset row or col list if all have been used
                if len(avail_rows) == 0:
                    avail_rows = list(matrix.keys())
                if len(avail_cols) == 0:
                    avail_cols = list(set([col for cols in matrix.values() for col in cols]))
        
                if len(avail_rows) == 0 or len(avail_cols) == 0:
                    return points
                
            return points

        # undirected node graph
        # key is start
        # value is another start
        # only added if neighbors share a location with only each other
        #  or must have water tuple with just these neighbors
        def build_neighbors():
            new_neighbor = defaultdict(list)
            for path in sorted(self.water.keys()):
                if len(path) == 2:
                    new_neighbor[path[0]].append(path[1])
                    new_neighbor[path[1]].append(path[0])
            return new_neighbor

        # remove small gaps between cells (aka paths)
        # check if the gap is traversable by a 3x3 block
        def print_start_conn(neighbor, neighbor_old):
            sys.stdout.write('   ')
            for col in sorted(neighbor.keys()):
                sys.stdout.write('{0:2}'.format(MAP_RENDER[col % 30]))
            sys.stdout.write('\n')
            for row in sorted(neighbor.keys()):
                sys.stdout.write('{0:2}'.format(MAP_RENDER[row % 30]))
                for col in sorted(neighbor.keys()):
                    if row in neighbor[col]:
                        sys.stdout.write(' X')
                    elif row in neighbor_old[col]:
                        sys.stdout.write(' =')
                    else:
                        sys.stdout.write(' .')
                sys.stdout.write('\n')
                                        
        def cells(size, starts,
                  distance=euclidean_distance,
                  row_distance=row_distance,
                  col_distance=col_distance):
            rows, cols = size
            size = (rows, cols)
            self.map = [[LAND for col in range(cols)] for row in range(rows)]
            
            # a comp (complement) is a set of points that act as the same cell
            # a set of locations will have the same comp id
            # it is used for symmetric maze carving
            
            # ensure points is a dict with id's
            # key is location, value is the comp id
            if type(starts) != dict:
                new_starts = {}
                for comp, point in enumerate(starts):
                    new_starts[point] = comp
                starts = new_starts

            # reverse index of starts
            # key is comp id, value is list of locations
            starts_by_comp = defaultdict(list)
            for point, comp in starts.items():
                starts_by_comp[comp].append(point)
                
            # list of water to remove when carving a passage between nodes
            # key is tuple of nearest starts (locations)
            # value is list of locations
            self.water = defaultdict(list)
            
            # for each square, find closest starting points
            for row in range(rows):
                for col in range(cols):
                    distances = {}
                    for loc in starts.keys():
                        distances[loc] = distance((row, col), loc, size)
                    cutoff = min(distances.values()) + self.cell_width
                    closest = [point for point, d in distances.items() if d <= cutoff]
                    comps = set([starts[point] for point in closest])
                    if len(closest) > 1:
                        if len(comps) > 1:
                            self.map[row][col] = WATER
                            # find all starting points that contributed to the water wall,
                            # add to water wall dict
                            nearest = sorted(comps)
                            self.water[tuple(nearest)].append((row, col))
                        else:
                            # multiple points closest belonging to same complement set
                            # only one contiguous section should be removed
                            pass
                    else:
                        # note: a cell could wrap around vertically or horizontally
                        #       depending on the placement of other cells
                        #       this draws a wall halfway around on the other side
                        n_row, n_col = closest[0]
                        if (row_distance(row, n_row, rows) >= rows//2 or
                            col_distance(col, n_col, cols) >= cols//2):
                            self.map[row][col] = WATER # this water can't be carved
            

            neighbor = build_neighbors()
            neighbor_old = build_neighbors()

            def fuse_cells(cell1, cell2):
                # join some cells together as one, fix data structures to match
                #print('# fuse %s and %s' % (MAP_RENDER[cell1], MAP_RENDER[cell2]))
                # fix water
                for path in self.water.keys():
                    if cell2 in path:
                        #print('# fuse del water path %s' % (path,))
                        if cell1 in path:
                            new_path = tuple(sorted(n for n in path if n != cell2))
                        else:
                            new_path = tuple(sorted(cell1 if n == cell2 else n for n in path))
#                        if new_path not in water:
#                            print('# fuse add water path %s' % (new_path,))
                        for loc in self.water[path]:
                            if new_path == (cell1,):
                                self.map[loc[0]][loc[1]] = LAND                        
                            elif loc not in self.water[new_path]:
                                self.water[new_path].append(loc)
                        del self.water[path]
                # fix starts
                for loc in starts.keys():
                    if starts[loc] == cell2:
                        starts[loc] = cell1
            
            # remove paths between cells that are not large enough
            wide_paths = {}
            restart_path_check = True
            while restart_path_check:
                for path in sorted(self.water.keys()):
                    if len(path) == 2:                        
                        if any(self.get_path(starts_by_comp[path[0]][0], dest_loc, size, 3, self.water[path])
                               for dest_loc in starts_by_comp[path[1]]):
                            # cells have path large enough to keep
                            wide_paths[path] = self.water[path][:]
                        else:
                            if len(neighbor[path[0]]) == 1 or len(neighbor[path[1]]) == 1:
                                #print('# fuse %s to %s' % path)
                                fuse_cells(*path)
                                neighbor = build_neighbors()
                                # we should restart the path checks since the openings between cells have changed
                                break
                            else:
                                #print("# removing %s to %s" % (MAP_RENDER[path[0]], MAP_RENDER[path[1]]))
                                neighbor[path[0]].remove(path[1])
                                neighbor[path[1]].remove(path[0])
#                        else:
#                            print "keeping %s to %s" % path
                else:
                    # did not break due to fusing cells, stop outer loop
                    restart_path_check = False

            growing_tree(neighbor, carve, extra_nodes=neighbor_old)

        # carve passages function to pass to maze function
        # note: a path from one point to another could have 2 walls if they touch
        #       left and right or top and bottom due to wrapping
        #       the path midpoint attempts to choose one and only one wall
        def carve(path, ilk=LAND, inclusive=False):
            if inclusive:
                path = [p for p in self.water.keys() if len(set(path) - set(p)) == 0]
            elif type(path) != list:
                path = [tuple(sorted(path))]
            for p in path:
                #print('# carve {0}: {1}'.format(' -> '.join(map(str, p)), water[p]))
                #m_row, m_col = mid_point(starts[path[0]], starts[path[1]], size)
                for row, col in self.water[p]:
                    #if (row_distance(m_row, row, rows) > rows//4 or
                    #        col_distance(m_col, col, cols) > cols//4):
                    #    continue
                    self.map[row][col] = ilk
                #self.toText()
            return path

        def fail_sections(sections):
            for i, (area_visited, area_seen) in enumerate(sections):
                for loc in area_visited:
                    if self.map[loc[0]][loc[1]] == LAND:
                        self.map[loc[0]][loc[1]] = i
            self.toText()
            sys.exit(2)
                          
        def ensure_connected(carve):
            # I'm too lazy to search for disconnected nodes and fuse cells for proper 3x3 block traversal
            # blow up random walls until valid
            sections = self.section(0)
            neighbor = build_neighbors() # includes removed neighbors
            if len(sections) > 1:
                log('connecting maze, sections: {0}'.format(len(sections)))
                # look for largest blocks of squares first, since this will make it easiest to traverse
                for path in (path for l, path in
                             sorted((-len(locs), path) for path, locs in self.water.items()
                             if len(path) == 2 and
                             path not in self.carved_paths)):
                    carve(path, LAND)
                    new_sections = self.section(0)
                    if len(new_sections) < len(sections):
                        log('maze connected, sections: {0}'.format(len(new_sections)))
                        #print('# found %s: %s' % (path, self.water[path]))
                        block_sections = [s for s in self.section(1) if len(s[0]) > 9]
                        if len(block_sections) > len(new_sections):
                            log('opening connection, sections: {0}'.format(len(block_sections)))
                            # carved path not wide enough
                            for third in neighbor.keys():
                                # find start c that connects to both a and b
                                ac = tuple(sorted([third, path[0]]))
                                bc = tuple(sorted([third, path[1]]))
                                abc = tuple(sorted([third, path[0], path[1]]))
                                if (ac in self.water.keys() and bc in self.water.keys()):
                                    for p in (ac, bc, abc):
                                        if p not in self.carved_paths:
                                            carve(p, LAND)                                       
                                    #log('# found %s' % (tuple(sorted([third, path[0], path[1]])),))
                                    new_block_sections = [s for s in self.section(1) if len(s[0]) > 9]
                                    if len(new_block_sections) <= len(new_sections):
                                        # triple connection successful, fix data structures
                                        for p in (ac, bc, abc):
                                            if path in self.end_braids:
                                                self.end_braids.remove(path)
                                            if path in self.middle_braids:
                                                self.middle_braids.remove(path)
                                            self.carved_paths.append(p)
                                        break # goes to # connection successful line
                                    # next statement only run in triple not successful
                                    for p in (ac, bc, abc):
                                        if p not in self.carved_paths:
                                            carve(p, WATER)
                            else:
                                log('failed to connect wide path')
                        # connection successful, fix data structures
                        if path in self.end_braids:
                            self.end_braids.remove(path)
                        if path in self.middle_braids:
                            self.middle_braids.remove(path)
                        self.carved_paths.append(path)
                        if len(new_sections) == 1:
                            break
                        else:
                            sections = new_sections
                            log('connecting maze, sections: {0}'.format(len(sections)))
                            continue
                    # next statement only runs if connection not found
                    carve(path, WATER)
                else:
                    log('failed to connect')
                    
        def growing_tree(nodes, carve, openness=0.2, extra_nodes=None):
            visited = set()
            self.carved_paths = []
            # track paths that would cause loops (aka braids) in maze
            self.middle_braids = [] # loops connecting passageways
            self.end_braids = []    # loops connecting dead end
            
            def prims(cells):
                return randrange(len(cells))
            def recursivebacktracker(cells):
                return -1
            def oldest(cells):
                return 0
            mazetype_report = {prims: 'prims',
                               recursivebacktracker: 'recursive backtracker',
                               oldest: 'growing tree oldest'}
            
            mazetype = choice((prims, recursivebacktracker, recursivebacktracker, oldest))
            mazetype = recursivebacktracker
            report('maze type {0}'.format(mazetype_report[mazetype]))
            
            while len(visited) < len(nodes):
                if len(visited) > 0:
                    # if starting over, we have just carved a set of nodes that is
                    # disconnected from the rest
                    # attempt to fix by finding 2 carved nodes that share a wall
                    # with an uncarved node
                    if extra_nodes != None:
                        cells = None
                        for uncarved in [n for n in extra_nodes.keys() if n not in visited]:
                            #print('# checking uncarved %s -> %s' % (uncarved, extra_nodes[uncarved]))
                            for carved1, carved2 in combinations(sorted(extra_nodes[uncarved]), 2):
                                if carved1 in visited and carved2 in visited:
                                    #print('# maze trying %s - %s' % (carved1, carved2))
                                    if carved1 in extra_nodes[carved2]:
                                        triple = (uncarved, carved1, carved2)                                        
                                        print('# maze joining %s to %s and %s' % triple)
                                        for p in combinations(triple, 2):
                                            self.carved_paths.extend(carve(p))
                                        self.carved_paths.extend(carve((uncarved, carved1, carved2), inclusive=True))
                                        cells = [uncarved]
                                        break
                            if cells != None:
                                break
                        if cells == None:
                            cells = [choice([n for n in nodes.keys() if n not in visited])]
                    else:
                        cells = [choice([n for n in nodes.keys() if n not in visited])]
                else:
                    # first time carving
                    cells = [choice(list(nodes.keys()))]
                #print('# maze starting at %s' % cells[0])
                visited.add(cells[0])
                while len(cells) > 0:
                    index = mazetype(cells)
                    cell = cells[index]
                    unvisited = [node for node in nodes[cell] if not node in visited]
                    if len(unvisited) > 0:
                        next = choice(unvisited)
                        carve((cell, next))
                        self.carved_paths.append(tuple(sorted([cell, next])))
                        visited.add(next)
                        cells.append(next)
                    else:
                        braids = [tuple(sorted([c, cell])) for c in nodes[cell] if tuple(sorted([c, cell])) not in self.carved_paths]
                        for braid in braids:
                            if len(braids) == len(nodes[cell]) - 1:
                                self.end_braids.append(braid)
                            else:
                                self.middle_braids.append(braid)
                        cells.pop(index)

        def set_openness(carve, openness=0.2):
            # favor dead ends before passage ways to keep map maze like
            open_braid_count = int((len(self.end_braids) + len(self.middle_braids)) * openness)
            open_braids = sample(self.end_braids, min(open_braid_count, len(self.end_braids)))
            open_braid_count -= len(open_braids)
            open_braids.extend(sample(self.middle_braids, open_braid_count))
            # open up braids
            for braid in open_braids:
                carve(braid, LAND)
                self.carved_paths.append(braid)
            # close up other paths
            for braid in self.end_braids + self.middle_braids:
                if braid not in open_braids:
                    carve(braid, WATER)
                    
        def make_euclidean_distance(size):
            rows, cols = size
            dist_table = [ [ sqrt(min(y,cols-y)**2 + min(x,rows-x)**2)
                            for y in range(cols) ]
                          for x in range(rows) ]
            def distance(loc1, loc2, size):
                return dist_table[loc1[0]-loc2[0]][loc1[1]-loc2[1]]
            return distance
        
        def make_linear_distance(size):
            dist_table = [ min(x, size-x) for x in range(size) ]
            def distance(x1, x2, size):
                return dist_table[x1 - x2]
            return distance

        def print_points(points, size, fd=sys.stderr):
            for r in range(size[0]):
                for c in range(size[1]):
                    if (r, c) in points:
                        fd.write(MAP_RENDER[points[(r,c)] % 30])
                    else:
                        fd.write('.')
                fd.write('\n')
            fd.write('\n')
            
        def pick_size():
            # pick random grid size
            divs = [i for i in range(1,self.players+1) if self.players%i==0]
            row_sym = choice(divs)
            col_sym = self.players/row_sym
            if row_sym > col_sym:
                row_sym, col_sym = col_sym, row_sym
            grid = (row_sym, col_sym) 
            
            min_val = 0.5 * row_sym / col_sym
            max_val = 5.0 * row_sym / col_sym
            aspect_ratio = (random() * (max_val - min_val) + min_val) * col_sym / row_sym
            
            rows = sqrt(self.area / aspect_ratio)
            cols = self.area / rows
            rows = int(rows / row_sym)
            cols = int(cols / col_sym)
            
            # modulo to ensure shearing will come out even
            size = (rows - rows % col_sym, cols - cols % row_sym)
            return size, grid
        
        size, grid = pick_size()
        while size[0] * grid[0] > 200 or size[1] * grid[1] > 200:
            size, grid = pick_size()
        report('grid size: {0}'.format(grid))
        report('tile size: {0} {1}'.format(size, 1.0*size[1]/size[0]))
        report('map size: {0} {1}'.format((size[0]*grid[0], size[1]*grid[1]),
                                       1.0 * size[1]*grid[1] / size[0]*grid[0]))

        points = {p: i for i, p in enumerate(random_points(1000, size, self.cell_size))}
        log('point count: {0}'.format(len(points)))
        # print_points(points, size)
        
        sym_points, sym_size, new_grid = make_symmetric(points, size, self.players, grid)
        log('symmetric point count: {0}'.format(len(sym_points)))
        # print_points(sym_points, sym_size)
    
        distance = make_euclidean_distance(size)
        errors = cells(sym_size, sym_points,
                     distance=make_euclidean_distance(sym_size),
                     row_distance=make_linear_distance(sym_size[0]),
                     col_distance=make_linear_distance(sym_size[1]))
        ensure_connected(carve)

        # set initial openness of maze
        # adjust until hill distances are within proper ranges
        set_openness(carve, self.openness)
    
        for _ in range(100):
            comps = list(set(sym_points.values()))
            min_dist = 1000
            max_dist = 0
            for comp in comps[:]:
                comp_locs = [point for point, c in sym_points.items() if c == comp]
                hill_dists = [dist for _, dist in self.get_distances(comp_locs[0], comp_locs[1:], sym_size)]
                if len(hill_dists) > 0:
                    hill_avg = sum(hill_dists)/len(hill_dists)
                    hill_min = min(hill_dists)
                    hill_max = max(hill_dists)
                    if hill_min > hill_avg * 0.5:
                        min_dist = min(min_dist, hill_min)
                        max_dist = max(max_dist, hill_max)
                        if hill_min < 24 or hill_max > 250:
                            comps.remove(comp)
                    else:
                        comps.remove(comp)
                else:
                    comps.remove(comp)
            if len(comps) == 0:
                if max_dist < 24:
                    if self.openness == 0:
                        # fail
                        sys.exit(1)
                    self.openness = max(0, self.openness - 0.01)
                    log('decreasing maze openness to {0}'.format(self.openness))
                elif min_dist > 250:
                    if self.openness == 1:
                        # fail
                        sys.exit(1)
                    self.openness = min(1, self.openness + 0.01)
                    log('increasing maze openness to {0}'.format(self.openness))
                set_openness(carve, self.openness)
            else:
                report('final openness {0}'.format(self.openness))
                break
            
        # find good hills
        hill_comp = choice(comps)
        hill_points = [point for point, c in sym_points.items() if c == hill_comp]

        for i, hill in enumerate(hill_points):
            self.map[hill[0]][hill[1]] = i
        
        self.fill_small_areas()
        self.make_wider()
        return errors

def main():
    new_map = CellMazeMap()
    
    # check that all land area is accessable
    reason = new_map.generate()
    if not reason:
        reason = new_map.allowable(check_sym=True, check_dist=True)
    if reason is not None:
        print('# ' + reason)
        
    #new_map.food_paths()
    new_map.toText()

if __name__ == '__main__':
    #import cProfile
    #cProfile.run('main()')
    main()
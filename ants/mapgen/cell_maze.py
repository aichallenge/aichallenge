#!/usr/bin/python
from __future__ import print_function
    
from random import randrange, random, choice, paretovariate, betavariate, shuffle, sample
from math import sqrt, ceil
from optparse import OptionParser
from copy import deepcopy
from itertools import combinations

from map import *
euclidean_distance = None

class CellMazeMap(Map):
    def __init__(self, options={}):
        options['name'] = 'cell maze'
        super(CellMazeMap, self).__init__(options)
        self.players = options.get('players', max(2, min(10, int((betavariate(2.5, 3.0) * 10) + 1))))
        self.area = options.get('area', randrange(900 * self.players, min(25000, 5000 * self.players)))
        self.cell_width = options.get('cell_width', min(paretovariate(2), 7.0))
        self.cell_size = options.get('cell_size', min(paretovariate(2) + max(5.0 + self.cell_width, self.cell_width * 2), 20.0))
        self.openness = options.get('openness', betavariate(1.0, 3.0))
        self.aspect_ratio = options.get('aspect_ratio', None)
        self.grid = options.get('grid', None)
        self.maze_type = options.get('maze_type', choice(['prims','backtrack','growing']))
        self.v_sym = options.get('v_sym', None)
        self.v_step = options.get('v_step', None)
        self.h_sym = options.get('h_sym', None)
        self.h_step = options.get('h_step', None)
        self.hills = options.get('hills', None)
        self.grandularity = options.get('grandularity', 1)

        self.report('players {0}'.format(self.players))
        self.report('area {0}, {1} ({2:.1f}^2) per player'.format(self.area, self.area//self.players, sqrt(self.area/self.players)))
        self.report('cell width: {0}'.format(self.cell_width))
        self.report('cell size: {0}'.format(self.cell_size))
        self.report('openness: {0}'.format(self.openness))
        if self.grid is not None:
            self.report('grid: {0}'.format(self.grid))
        self.report('maze type {0}'.format(self.maze_type))
        if self.v_sym is not None:
            self.report('vertical symmetry: {0}'.format(self.v_sym))
        if self.v_step is not None:
            self.report('vertical shear step: {0}'.format(self.v_step))
        if self.h_sym is not None:
            self.report('horizontal symmetry: {0}'.format(self.h_sym))
        if self.h_step is not None:
            self.report('horizontal shear step: {0}'.format(self.h_step))
        if self.hills is not None:
            self.report('hills per player: {0}'.format(self.hills))
            
        self.min_hill_dist = 20
        self.max_hill_dist = 150
                                    
    def generate(self):
        
        def destination(loc, d, size):
            """ Returns the location produced by offsetting loc by d """
            return ((loc[0] + d[0]) % size[0], (loc[1] + d[1]) % size[1])

        def pick_size(grid=None, aspect_ratio=None):
            if grid is None:
                # pick random grid size
                divs = [i for i in range(1,self.players+1) if self.players%i==0]
                row_sym = choice(divs)
                col_sym = self.players//row_sym
            else:
                row_sym, col_sym = grid
            # the min/max values for aspect ratio are tuned for more grid rows than columns
            if row_sym > col_sym:
                row_sym, col_sym = col_sym, row_sym
                self.v_sym, self.h_sym = self.h_sym, self.v_sym
                self.v_step, self.h_step = self.h_step, self.v_step
            grid = (row_sym, col_sym) 
                
            min_val = 0.5 * row_sym / col_sym
            max_val = 5.0 * row_sym / col_sym

            if aspect_ratio is None:
                aspect_ratio = (random() * (max_val - min_val) + min_val) * col_sym / row_sym
            
            rows = sqrt(self.area / aspect_ratio)
            cols = self.area / rows
            rows = int(rows / row_sym)
            cols = int(cols / col_sym)
            
            # modulo to ensure shearing will come out even
            size = (rows - rows % col_sym, cols - cols % row_sym)
            if grid[0] * size[0] > 200:
                rows = 200 // row_sym
                cols = self.area // (rows * row_sym * col_sym)
                size = (rows - rows % col_sym, cols - cols % row_sym)
            elif grid[1] * size[1] > 200:
                cols = 200 // col_sym
                rows = self.area // (cols * col_sym * row_sym)
                size = (rows - rows % col_sym, cols - cols % row_sym)
            
            return size, grid

        def random_points(size, spacing, grandularity=1, count=1000):
            spacing_2 = int(ceil(spacing ** 2))
            spacing = int(ceil(spacing))
            # ensure each random point is on a unique row and col
            rows, cols = size
            # matrix of available spots left
            # range is from 1 to keep points from touching on mirror lines
            matrix = {row: list(range(0,cols,grandularity))
                      for row in range(0,rows,grandularity)}
            
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
                
            return sorted(points)
                               
        def make_symmetric(points, size, players, grid, v_sym=None, v_step=None, h_sym=None, h_step=None):
            def copy(value, size, step):
                return size+value
            def mirror(value, size, step):
                return size * 2 - value - 1
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
                self.report(extend_report[funcs])
                #rows, cols = size
                new_points = []
                for sym_points in points:
                    if type(sym_points) != list:
                        sym_points = [sym_points]
                    new_sym_points = sym_points[:]
                    for point in sym_points:
                        for c in range(1,count):
                            new_sym_points.append(
                                point_func(point,                    # point
                                increase_func(size, c),              # size
                                ((c * step) % count, count * shear), # step
                                trans_funcs)                         # vert/horz functions
                            )
                    new_points.append(new_sym_points)
                return new_points, increase_func(size, count)
        
            row_sym, col_sym = grid
            # TODO: 90 degree rotate

            #print_points(points, size)
            step = 0
            # limit shearing to values that produce good maps
            sym_step = {2: [0, 1],
                        3: [0, 1, 2],
                        4: [0, 1, 2, 3],
                        5: [1, 2, 3, 4],
                        6: [2, 3, 4], 
                        7: [2, 3, 4, 5],
                        8: [2, 3, 4, 5, 6],
                        9: [2, 3, 4, 5, 6, 7],
                        10: [3, 4, 5, 6, 7]}
            if row_sym > 1:
                if v_sym is None:
                    if row_sym % 2 == 0 and random() < 0.5 and row_sym <= 6:
                        v_sym = choice((vert_mirror, vert_rotate)) if row_sym < 6 else vert_rotate
                    else:
                        if v_step is None:
                            step = choice(sym_step[row_sym])
                        else:
                            step = v_step
                        v_sym = vert_shear
                else:
                    v_sym = [key for key, value in extend_report.items()
                             if value == 'vert_' + v_sym][0]
                    if v_sym == vert_shear:
                        if v_step is None:
                            step = choice(sym_step[row_sym])
                        else:
                            step = v_step
                    else:
                        step = 0
                if step > 0:
                    self.report("vert shear step: {0}".format(step))                
                if v_sym in (vert_mirror, vert_rotate):
                    points, size = extend(v_sym, points, size)
                    if row_sym//2 > 1:
                        points, size = extend(vert_copy, points, size, row_sym//2)
                else:
                    points, size = extend(v_sym, points, size, row_sym, step)

            if col_sym > 1:
                if step > 0:
                    # can't shear, mirror or rotate if vert shearing happened
                    h_sym = horz_copy
                    step = 0
                else:
                    if h_sym is None:
                        if col_sym % 2 == 0 and random() < 0.5 and col_sym <= 6:
                            h_sym = choice((horz_mirror, horz_rotate)) if col_sym < 6 else horz_rotate
                        else:
                            if v_sym in (vert_mirror, vert_rotate):
                                # can't shear after mirror or rotate
                                h_step = 0
                                h_sym = horz_copy
                            else:
                                if h_step is None:
                                    step = choice(sym_step[col_sym])
                                else:
                                    step = h_step
                                h_sym = horz_shear
                    else:
                        h_sym = [key for key, value in extend_report.items()
                                 if value == 'horz_' + h_sym][0]
                        if h_sym == horz_shear:
                            if v_sym in (vert_mirror, vert_rotate):
                                # can't shear after mirror or rotate
                                h_step = 0
                                h_sym = horz_copy
                            else:
                                if h_step is None:
                                    step = choice(sym_step[col_sym])
                                else:
                                    step = h_step
                        else:
                            step = 0
                if step > 0:
                    self.report("horz shear step: {0}".format(step))                
                if h_sym in (horz_mirror, horz_rotate):
                    points, size = extend(h_sym, points, size)
                    if col_sym//2 > 1:
                        points, size = extend(horz_copy, points, size, col_sym//2)
                else:
                    points, size = extend(h_sym, points, size, col_sym, step, row_sym)
            
            return points, size
        
        def build_neighbors(paths):
            # undirected node graph
            # key is start
            # value is another start
            # only added if neighbors share a location with only each other
            #  or must have water tuple with just these neighbors
            new_neighbor = defaultdict(list)
            for path in sorted(paths):
                if len(path) == 2 and path[0] != path[1]:
                    new_neighbor[path[0]].append(path[1])
                    new_neighbor[path[1]].append(path[0])
            return new_neighbor

        def build_paths(points, size, distance):
            rows, cols = size
            # list of water to remove when carving a passage between nodes
            # key is tuple of nearest starts (locations)
            # value is list of locations
            paths = {}
#            mirror_points = {}
            
            # for each square, find closest starting points
            for row in range(rows):
                for col in range(cols):
                    distances = {}
                    #for loc in points.keys():
                    for comp, sym_points in enumerate(points):
                        for point in sym_points:
                            distances[(point, comp)] = distance((row, col), point, size)
                    cutoff = min(distances.values()) + self.cell_width
                    closest = [point_comp for point_comp, d in distances.items()
                               if d <= cutoff]
                    comps = tuple(sorted(set([comp for point, comp in closest])))
                    if len(closest) > 1:
                        if len(comps) > 1:
                            self.map[row][col] = WATER
                            # find all starting points that contributed to the water wall,
                            # add to water wall dict
#                        else:
#                            if distance(closest[0][0], closest[1][0], size) > self.cell_width:
#                                comps = (comps[0], comps[0])
#                                # store unique points in path structure so we can later check if traversable
#                                if comps not in mirror_points:
#                                    mirror_points[comps] = []
#                                if closest not in mirror_points[comps]:
#                                    mirror_points[comps].append(closest)
                    if comps not in paths:
                        paths[comps] = []
                    paths[comps].append((row, col))

                    
#            # close small gaps in mirrored/rotated paths between same comps
#            for path, closests in mirror_points.items():
#                for closest in closests:
#                    if not self.get_path(closest[0][0], closest[1][0], size, 3):
#                        for row, col in paths[path]:
#                            self.map[row][col] = FOOD
#                        del paths[path]
#                        break
                        
            return paths

        def fuse_cells(paths, cell1, cell2):
            paths = deepcopy(paths)
            self.log("fuse {0} and {1}".format(cell1, cell2))
            # join some cells together as one, fix data structures to match
            for path in list(paths.keys()):
                if cell2 in path:
                    if cell1 in path:
                        new_path = tuple(sorted(n for n in path if n != cell2))
                    else:
                        new_path = tuple(sorted(cell1 if n == cell2 else n for n in path))
                    for loc in paths[path]:
                        if new_path == (cell1,):
                            self.map[loc[0]][loc[1]] = LAND
                        if new_path not in paths:
                            paths[new_path] = [loc]
                        elif loc not in paths[new_path]:
                            paths[new_path].append(loc)
                    del paths[path]
            return paths
                    
        def neighbor_dist(neighbors, start, end):
            dist = 0
            visited = set([start])
            frontier = set(neighbors[start])
            while len(frontier) > 0:
                dist += 1
                if end in frontier:
                    return dist
                else:
                    visited.update(frontier)
                    for n in list(frontier):
                        frontier.update(set(neighbors[n]))
                    frontier.difference_update(visited)
            return None
                            
        def remove_narrow_paths(paths, neighbors, points, size):
            neighbors = deepcopy(neighbors)
            paths = deepcopy(paths)
            
            # reverse index of starts
            # key is comp id, value is list of locations
            starts_by_comp = {}
            for comp, sym_points in enumerate(points):
                starts_by_comp[comp] = sym_points[:]            
            # remove paths between cells that are not large enough
            restart_path_check = True
            while restart_path_check:
                narrow_paths = []
                for path in sorted(paths.keys()):
                    if len(path) == 2:
                        if not any(self.get_path(starts_by_comp[path[0]][0], dest_loc, size, 3, paths[path])
                               for dest_loc in starts_by_comp[path[1]]):
                            neighbors[path[0]].remove(path[1])
                            neighbors[path[1]].remove(path[0])
                            if neighbor_dist(neighbors, path[0], path[1]) is not None:
                                narrow_paths.append(path)
                            else:
                                # find 3 cells that can be fused
                                for triple in list(paths.keys()):
                                    if len(set(triple)) == 3 and path[0] in triple and path[1] in triple:
                                        path3 = [n for n in triple if n not in path][0]
                                        paths = fuse_cells(paths, path3, path[0])
                                        paths = fuse_cells(paths, path3, path[1])
                                        break
                                neighbors = build_neighbors(paths.keys())
                                # we should restart the path checks since the openings between cells have changed
                                break
                else:
                    # did not break due to fusing cells, stop outer loop
                    restart_path_check = False
            
            return paths, neighbors, narrow_paths
                                
        def carve(paths, path, ilk=LAND, inclusive=False):
            # carve passages function to pass to maze and other functions
            if inclusive:
                path = [p for p in paths.keys() if len(set(path) - set(p)) == 0]
            elif type(path) != list:
                path = [tuple(sorted(path))]
            for p in path:
                if p in paths:
                    for row, col in paths[p]:
                        self.map[row][col] = ilk
            return path

        def mark_sections(sections):
            for i, (area_visited, _) in enumerate(sections):
                for loc in area_visited:
                    if self.map[loc[0]][loc[1]] == LAND:
                        self.map[loc[0]][loc[1]] = i
                          
        def ensure_connected(paths, carved_paths, carve):
            carved_paths = deepcopy(carved_paths)
            # I'm too lazy to search for disconnected nodes and fuse cells for proper 3x3 block traversal
            # blow up random walls until valid
            sections = self.section(1)
            neighbor = build_neighbors(paths.keys()) # includes removed neighbors
            if len(sections) > 1:
                self.log('connecting maze, sections: {0}'.format(len(sections)))
                # look for largest blocks of squares first, since this will make it easiest to traverse
                for path in (path for l, path in
                             sorted((-len(locs), path) for path, locs in paths.items()
                             if len(path) == 2 and
                             path not in carved_paths)):
                    carve(path, LAND)
                    new_sections = self.section(0)
                    if len(new_sections) < len(sections):
                        self.log('maze connected, sections: {0}'.format(len(new_sections)))
                        #print('# found %s: %s' % (path, self.water[path]))
                        block_sections = [s for s in self.section(1) if len(s[0]) > 9]
                        if len(block_sections) > len(new_sections):
                            self.log('opening connection, sections: {0}'.format(len(block_sections)))
                            # carved path not wide enough
                            for third in neighbor.keys():
                                # find start c that connects to both a and b
                                ac = tuple(sorted([third, path[0]]))
                                bc = tuple(sorted([third, path[1]]))
                                abc = tuple(sorted([third, path[0], path[1]]))
                                if (ac in paths.keys() and bc in paths.keys()):
                                    for p in (ac, bc, abc):
                                        if p not in carved_paths:
                                            carve(p, LAND)                                       
                                    #log('# found %s' % (tuple(sorted([third, path[0], path[1]])),))
                                    new_block_sections = [s for s in self.section(1) if len(s[0]) > 9]
                                    if len(new_block_sections) <= len(new_sections):
                                        # triple connection successful, fix data structures
                                        for p in (ac, bc, abc):
                                            carved_paths.append(p)
                                        break # goes to # connection successful line
                                    # next statement only run in triple not successful
                                    for p in (ac, bc, abc):
                                        if p not in carved_paths:
                                            carve(p, WATER)
                            else:
                                self.log('failed to connect wide path')
                        # connection successful, fix data structures
                        carved_paths.append(path)
                        if len(new_sections) == 1:
                            break
                        else:
                            sections = new_sections
                            self.log('connecting maze, sections: {0}'.format(len(sections)))
                            continue
                    # next statement only runs if connection not found
                    carve(path, WATER)
                else:
                    self.log('failed to connect')
            return carved_paths
                    
        def growing_tree(nodes, carve, visited=[]):
            visited = set(visited)
            carved_paths = []
            
            def prims(cells):
                return randrange(len(cells))
            def recursivebacktracker(cells):
                return -1
            def oldest(cells):
                return 0
            maze_type_report = {prims: 'prims',
                               recursivebacktracker: 'backtrack',
                               oldest: 'growing'}
            maze_type = [f for f, t in maze_type_report.items() if t == self.maze_type][0]
            
            unvisited = [node for node in nodes.keys() if node not in visited]
            next_cell = choice(unvisited)
            cells = [next_cell]
            visited.add(cells[0])
            while len(cells) > 0:
                index = maze_type(cells)
                cell = cells[index]
                unvisited = [node for node in nodes[cell] if not node in visited]
                if len(unvisited) > 0:
                    next_cell = choice(unvisited)
                    carve((cell, next_cell))
                    carved_paths.append(tuple(sorted([cell, next_cell])))
                    visited.add(next_cell)
                    cells.append(next_cell)
                else:
                    cells.pop(index)
            return carved_paths
        
        def set_openness(points, braids, carved_paths, openness, carve, open_braids=[]):
            def carve_braid(open_paths, braids):
                carved_neighbors = build_neighbors(open_paths)            
                braid = sorted(braids,
                               key=lambda path: neighbor_dist(carved_neighbors, *path),
                               reverse=True)[0]
                carve(braid, LAND)
                return braid

            # open initial set of braids
            self.log("set openness to {0} on {1} braids".format(self.openness, len(braids)))
            open_braid_count = int(len(braids) * openness)
            closed_braids = braids[:]
            for _ in range(open_braid_count):
                braid = carve_braid(carved_paths+open_braids, closed_braids)
                open_braids.append(braid)
                closed_braids.remove(braid)
                
            # adjust braids until hill distances are within proper ranges
            min_count = max_count = valid_first_count = 0
            valid_hills = []
            adjusted = False
            while valid_first_count == 0:
                min_count = max_count = valid_first_count = 0
                valid_hills = []
                for i, s_points in enumerate(points):
                    hill_dists = [dist for _, dist in self.get_distances(s_points[0], s_points[1:])]
                    if len(hill_dists) == 0:
                        if len(closed_braids) > 0:
                            braid = closed_braids.pop()
                            carve(braid, WATER)
                            continue
                        else:
                            raise Exception("MapException", "Map closed off")
                    elif min(hill_dists) < self.min_hill_dist:
                        min_count += 1
                    elif max(hill_dists) > self.max_hill_dist:
                        max_count += 1
                    else:
                        if min_count > 0 or stdev(hill_dists) < 2.0*max(hill_dists)/self.players:
                            valid_first_count += 1
                        else:
                            self.log(stdev(hill_dists))
                            max_count += 1
                        valid_hills.append((i, hill_dists, stdev(hill_dists)))
                if valid_first_count == 0:
                    adjusted = True
                    if min_count > max_count:
                        # reduce openness, close 1 braid
                        if len(open_braids) == 0:
                            raise Exception("Openness", "Map too open")
                        braid = open_braids.pop()
                        carve(braid, WATER)
                        closed_braids.append(braid)
                        self.log("decrease openness")
                    elif max_count > min_count:
                        # increase openness, open 1 braid
                        if len(closed_braids) == 0:
                            raise Exception("Openness", "Map too closed")
                        braid = carve_braid(carved_paths+open_braids, closed_braids)
                        open_braids.append(braid)
                        closed_braids.remove(braid)
                        self.log("increase openness")
                    else:
                        raise Exception("Openness", "Map too weird")
                    self.openness = 1.0 * len(open_braids) / len(braids)
            if adjusted:
                self.log("adjust openness to {0}".format(self.openness))
            return open_braids, valid_hills
                    
        def make_euclidean_distance(size):
            rows, cols = size
            dist_table = [ [ sqrt(min(y,cols-y)**2 + min(x,rows-x)**2)
                            for y in range(cols) ]
                          for x in range(rows) ]
            def distance(loc1, loc2, size):
                return dist_table[loc1[0]-loc2[0]][loc1[1]-loc2[1]]
            return distance
                
        def print_points(points, size, tile_size, fd=sys.stderr):
            TILE = [',','.','-',"'"]
            for r in range(size[0]):
                for c in range(size[1]):
                    for i, sym_points in enumerate(points):
                        if (r, c) in sym_points:
                            fd.write(MAP_RENDER[i % 30])
                            break
                    else:
                        fd.write(TILE[(r//tile_size[0] + c//tile_size[1])%len(TILE)])
                fd.write('\n')
            fd.write('\n')

        def stdev(values):
            avg = 1.0*sum(values)/len(values)
            return sqrt(sum([pow(d - avg, 2) for d in values])/len(values))

        def mark_points(points):
            # add point markers for debugging
            for comp, sym_p in enumerate(points):
                for _, (row, col) in enumerate(sym_p):
                    self.map[row][col] = comp % 30

        def cavern(nodes, all_nodes, carve, carve2, cavern_count=7):
            closed = []
            visited = []
            carved_paths = []
            caves = list(all_nodes.keys())
            while len(caves) > 0 and cavern_count > 0:
                # pick random cell
                cave = choice(caves)
                caves.remove(cave)
                if len(all_nodes[cave]) == 0:
                    continue
                #caves = []
                # find surrounding cells
                juxs = []
                for jux in all_nodes[cave]:
                    if jux not in closed:
                        carved_paths.extend(carve((cave, jux)))
                        closed.append(jux)
                        juxs.append(jux)
                if len(juxs) > 0:
                    # clean out cavern walls
                    carve((cave,))
                    for path in combinations(juxs, 2):
                        carved_paths.extend(carve(tuple(sorted(path))))
                        carve(tuple(sorted([cave, path[0], path[1]])))
                    # find paths out of cavern
                    doors = []
                    closed.append(cave)
                    for jux in juxs:
                        carve((jux,))
                        for jux2 in nodes[jux]:
                            if jux2 not in closed:
                                #closed.append(jux2)
                                if jux2 in caves:
                                    caves.remove(jux2)
                                doors.append(tuple(sorted((jux, jux2))))
                    # open doorways to cavern
                    shuffle(doors)
                    for _ in range(len(juxs)//4 + 1):
                        if len(doors) > 0:
                            door = doors.pop()
                            carved_paths.extend(carve2(door))
                    # add cavern cells to visited list for maze carving
                    if len(juxs) > 0:
                        visited.append(cave)
                        visited.extend(juxs)
                    cavern_count -= 1
            return carved_paths, visited
            
        def cell_maze():
            # actual maze code
            tile_size, grid = pick_size(self.grid, self.aspect_ratio)
            points = random_points(tile_size, self.cell_size, self.grandularity)
            sym_points, sym_size = make_symmetric(points, tile_size, self.players, grid, self.v_sym, self.v_step, self.h_sym, self.h_step)
        
            self.report("final parameters")
            self.report('area: {0}'.format(sym_size[0] * sym_size[1]))
            self.report('grid size: {0}'.format(grid))
            self.report('tile size: {0} {1}'.format(tile_size, 1.0 * tile_size[1] / tile_size[0]))
            self.report('map size: {0} {1}'.format((sym_size[0], sym_size[1]),
                                              1.0 * sym_size[1] / sym_size[0]))
            self.log("point count: {0}".format(len(points)))
            
            rows, cols = sym_size
            self.map = [[LAND]* cols for _ in range(rows)]
    
            paths = build_paths(sym_points, sym_size, make_euclidean_distance(sym_size))
            neighbors = build_neighbors(paths.keys())
            paths, neighbors, narrow_paths = remove_narrow_paths(paths, neighbors, sym_points, sym_size)
    
            carved_paths = []
#            carved_paths, visited = cavern(neighbors,
#                                           build_neighbors(paths.keys()),
#                                           lambda path: carve(paths, path, LAND),
#                                           lambda path: carve(paths, path, LAND))        
            carved_paths.extend(growing_tree(neighbors, lambda path: carve(paths, path)))            
            carved_paths = ensure_connected(paths, carved_paths, lambda path, ilk: carve(paths, path, ilk))        

            braids = [path for path in paths.keys() if len(path) == 2 and path not in narrow_paths + carved_paths]
            mirror_braids = sorted([path for path in paths.keys() if len(path) == 2 and path[0] == path[1]], key=lambda path: len(paths[path]))
            braid_paths, valid_hills = set_openness(sym_points, braids, carved_paths, self.openness, lambda path, ilk: carve(paths, path, ilk), mirror_braids)
            carved_paths.extend(braid_paths)
                
            valid_hills.sort(key=lambda valid_hill: (valid_hill[2], -max(valid_hill[1])))
            comp, hill_dists, _ = valid_hills.pop(0)
            first_hills = sym_points[comp]
            self.log("first hill stdev: {0}".format(stdev(hill_dists)))
            for player, (row, col) in enumerate(first_hills):
                self.map[row][col] = player
            map_sym = choice(self.get_map_symmetry())
            all_hills = first_hills[:]
            
            hill_count = None if self.hills is None else self.hills - 1
            while len(valid_hills) > 0:
                if (hill_count is None and random() < 0.5) or hill_count > 0:
                    comp = randrange(0, len(valid_hills))
                    comp, hill_dists, _ = valid_hills.pop(comp)
                    hills = sym_points[comp]
                    hill_dists = list(self.get_distances(hills[0], all_hills, sym_size))
                    if max([dist for _, dist in hill_dists]) > self.max_hill_dist:
                        # a potiential hill is too far away from other hills
                        continue
                    enemies = set([self.map[row][col] for (row, col), dist in
                                   hill_dists if dist < self.min_hill_dist])
                    #log(sym_points)
                    if len(enemies) > 1:
                        # a potential hill is within min distance to 2 enemies
                        continue
                    all_hills.extend(hills)
                    if hill_count is not None:
                        hill_count -= 1
                    for player, (row, col) in enumerate(hills):
                        if player == 0:
                            if len(enemies) == 1:
                                next_player = list(enemies)[0]
                            else:
                                next_player = randrange(0, self.players)
                            self.map[row][col] = next_player
                            # find offset to self
                            first_hill = first_hills[next_player]
                            hill_offset = (first_hill[0] - row, first_hill[1] - col)
                        else:
                            p_row, p_col = self.dest_offset((row, col), self.offset_aim(hill_offset, map_sym[player][1]), sym_size)
                            next_player = self.map[p_row][p_col]
                            self.map[row][col] = next_player
                else:
                    break
            self.fill_small_areas()
            self.make_wider()

        def add_border(size):
            rows, cols = size
            self.map[0] = [WATER] * cols
            self.map[-1] = [WATER] * cols
            for row in range(rows):
                self.map[row][0] = WATER
                self.map[row][-1] = WATER
                
        def corner_hills(size):
            self.map[3][3] = 0
            self.map[3][-4] = 1
            self.map[-4][-4] = 2
            self.map[-4][3] = 3
                
        def caverns():
            #self.cell_width = max(self.cell_width, 4)
            #self.cell_size += max(self.cell_size, 9.0)
            # actual maze code
            tile_size, grid = pick_size(self.grid, self.aspect_ratio)
            points = random_points(tile_size, self.cell_size)
            sym_points, sym_size = make_symmetric(points, tile_size, self.players, grid, self.v_sym, self.v_step, self.h_sym, self.h_step)
        
            self.report("final parameters")
            self.report('area: {0}'.format(sym_size[0] * sym_size[1]))
            self.report('grid size: {0}'.format(grid))
            self.report('tile size: {0} {1}'.format(tile_size, 1.0 * tile_size[1] / tile_size[0]))
            self.report('map size: {0} {1}'.format((sym_size[0], sym_size[1]),
                                              1.0 * sym_size[1] / sym_size[0]))
            self.log("point count: {0}".format(len(points)))
            
            rows, cols = sym_size
            self.map = [[LAND]* cols for _ in range(rows)]
    
            paths = build_paths(sym_points, sym_size, make_euclidean_distance(sym_size))
            for path in paths.keys():
                if len(path) == 1:
                    carve(paths, path, WATER)
                else:
                    carve(paths, path, LAND)
                    
#            if len(self.section(1)) > 1:
#                sys.exit()
            neighbors = build_neighbors(paths.keys())
            while True:
                n = choice(list(neighbors.keys()))
                nw = neighbors[n][:]
                if len(nw) < 4:
                    continue
                ns = randrange(1,len(nw)-1)
                nl = nw[:ns]
                nr = nw[ns:]
                for path in combinations(nl,2):
                    if path in paths:
                        carve(paths, path, WATER)
                for path in combinations(nr,2):
                    if path in paths:
                        carve(paths, path, WATER)
                carve(paths, (n,), LAND)
                break
                    
            mark_points(sym_points)
            self.toText()
            return
                    
            paths, neighbors, narrow_paths = remove_narrow_paths(paths, neighbors, sym_points, sym_size)
    
            carved_paths = growing_tree(neighbors, lambda path: carve(paths, path))
            carved_paths = ensure_connected(paths, carved_paths, lambda path, ilk: carve(paths, path, ilk))        

            braids = [path for path in paths.keys() if len(path) == 2 and path not in narrow_paths + carved_paths]
            mirror_braids = sorted([path for path in paths.keys() if len(path) == 2 and path[0] == path[1]], key=lambda path: len(paths[path]))
            braid_paths, valid_hills = set_openness(sym_points, braids, carved_paths, self.openness, lambda path, ilk: carve(paths, path, ilk), mirror_braids)
            carved_paths.extend(braid_paths)

                
            valid_hills.sort(key=lambda valid_hill: (valid_hill[2], -max(valid_hill[1])))
            comp, hill_dists, _ = valid_hills.pop(0)
            first_hills = sym_points[comp]
            self.log("first hill stdev: {0}".format(stdev(hill_dists)))
            for player, (row, col) in enumerate(first_hills):
                self.map[row][col] = player
            map_sym = choice(self.get_map_symmetry())
            all_hills = first_hills[:]
            
            hill_count = None if self.hills is None else self.hills - 1
            while len(valid_hills) > 0:
                if (hill_count is None and random() < 0.5) or hill_count > 0:
                    comp = randrange(0, len(valid_hills))
                    comp, hill_dists, _ = valid_hills.pop(comp)
                    hills = sym_points[comp]
                    hill_dists = list(self.get_distances(hills[0], all_hills, sym_size))
                    if max([dist for _, dist in hill_dists]) > self.max_hill_dist:
                        # a potiential hill is too far away from other hills
                        continue
                    enemies = set([self.map[row][col] for (row, col), dist in
                                   hill_dists if dist < self.min_hill_dist])
                    #log(sym_points)
                    if len(enemies) > 1:
                        # a potential hill is within min distance to 2 enemies
                        continue
                    all_hills.extend(hills)
                    if hill_count is not None:
                        hill_count -= 1
                    for player, (row, col) in enumerate(hills):
                        if player == 0:
                            if len(enemies) == 1:
                                next_player = list(enemies)[0]
                            else:
                                next_player = randrange(0, self.players)
                            self.map[row][col] = next_player
                            # find offset to self
                            first_hill = first_hills[next_player]
                            hill_offset = (first_hill[0] - row, first_hill[1] - col)
                        else:
                            p_row, p_col = self.dest_offset((row, col), self.offset_aim(hill_offset, map_sym[player][1]), sym_size)
                            next_player = self.map[p_row][p_col]
                            self.map[row][col] = next_player
                else:
                    break
            self.fill_small_areas()
            self.make_wider()
                    
        cell_maze()
        
def main():
    parser = OptionParser()
    parser.add_option("-s", "--seed", dest="seed", type="int",
                        help="random seed")
    parser.add_option("-p", "--players", dest="players", type="int",
                        help="number of players")
    parser.add_option("-o", "--openness", dest="openness", type="float",
                        help="openness of map (0.0 to 1.0)")
    parser.add_option("-a", "--area", dest="area", type="int",
                        help="area of map")
    parser.add_option("-w", "--cell_width", dest="cell_width", type="float",
                      help="cell width (or width of walls)")
    parser.add_option("-c", "--cell_size", dest="cell_size", type="float",
                      help="cell size")
    parser.add_option("-g", "--grid", dest="grid", type="int", nargs=2,
                      help="grid layout (product must equal players)")
    parser.add_option("--v_sym", dest="v_sym",
                      choices=["copy", "mirror", "rotate", "shear"],
                      help="vertical symmetry")
    parser.add_option("--v_step", dest="v_step", type="int",
                      help="vertical shearing step")
    parser.add_option("--h_sym", dest="h_sym",
                      choices=["copy", "mirror", "rotate", "shear"],
                      help="horizontal symmetry")
    parser.add_option("--h_step", dest="h_step", type="int",
                      help="horizontal shearing step")
    parser.add_option("-r", "--ratio", dest="aspect_ratio", type="float",
                      help="aspect ratio of final map")
    parser.add_option("-m", "--maze", dest="maze_type",
                      choices=["backtrack", "prims", "growing"],
                      help="maze type for carving passages")
    parser.add_option("--hills", dest="hills", type="int",
                      help="hills per player")
    parser.add_option("--grandularity", dest="grandularity", type="int",
                      help="align random points on a grid")
    
    opts, _ = parser.parse_args(sys.argv)

    options = {key: value for key, value in vars(opts).items() if value is not None}
    new_map = CellMazeMap(options)
    
    # check that all land area is accessible
    reason = new_map.generate()
    if not reason:
        reason = new_map.allowable(check_sym=True, check_dist=True)
    exit_code = 0
    if reason is not None:
        new_map.toText(sys.stderr)
        print('# ' + reason)
        exit_code = 1
        sys.exit(exit_code)
        
    new_map.toText()
    #new_map.toFile()

if __name__ == '__main__':
    main()

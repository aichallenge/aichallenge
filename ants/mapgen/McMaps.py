#!/usr/bin/python
import sys
from random import randrange, random, choice, seed
from math import sqrt
import Image, ImageDraw, ImageChops
from itertools import combinations
from collections import defaultdict

MY_ANT = 0
ANTS = 0
DEAD = -1
LAND = -2
FOOD = -3
BARRIER = -4
UNSEEN = -5

BARRIER_COLOR = (128, 128, 128)
LAND_COLOR = (139, 69, 19)
FOOD_COLOR = (255, 255, 255)

COLOR = {LAND: LAND_COLOR,
         BARRIER: BARRIER_COLOR,
         UNSEEN: LAND_COLOR}
for i in range(26):
    COLOR[i] = FOOD_COLOR

class Node:
    def all(self):
        yield self.location
        if self.left_child != None:
            for location in self.left_child.all():
                yield location
        if self.right_child != None:
            for location in self.right_child.all():
                yield location
 
def kdtree(point_list, depth=0):
    if not point_list:
        return
 
    # Select axis based on depth so that axis cycles through all valid values
    k = len(point_list[0]) # assumes all points have the same dimension
    axis = depth % k
    def key_func(point):
        return point[axis]
 
    # Sort point list and choose median as pivot element
    point_list.sort(key=key_func)
    median = len(point_list) // 2 # choose median
 
    # Create node and construct subtrees
    node = Node()
    node.location = [point_list[median], depth]
    node.left_child = kdtree(point_list[:median], depth + 1)
    node.right_child = kdtree(point_list[median + 1:], depth + 1)
    return node

def draw_line(image, point, neighbor, size):
    width, height = size
    center_point = (width//2, height//2)
    offset = (width//2 - point[0], height//2 - point[1])
    image = ImageChops.offset(image, offset[0], offset[1])
    draw = ImageDraw.Draw(image)
    to_point = ((neighbor[0] + offset[0]) % width, (neighbor[1] + offset[1]) % height)
    draw.line((center_point, to_point))
    return ImageChops.offset(image, -offset[0], -offset[1])

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def sort_key(self):
        return (self.x, self.y)

class Triangle:
    def __init__(self, points):
        self.p1 = points[0]
        self.p2 = points[1]
        if len(points) > 2:
            self.p3 = points[2]
        else:
            self.p3 = None
    def center(self):
        if self._center == None:
            x1, y1 = self.p1.x, self.p1.y
            x2, y2 = self.p2.x, self.p2.y
            if self.p3 == None:
                # return midpoint of segment
                self._center = (x1+x2)/2, (y1+y2)/2
            else:
                x3, y3 = self.p3.x, self.p3.y
                # check for coincident lines
                # check for infinite slope
                (x1, y1), (x2, y2), (x3, y3) = self.p1, self.p2, self.p3
                ma = (y2-y1)/(x2-x1)
                mb = (y3-y2)/(x3-x2)
                if ma != mb:                    
                    x = (ma*mb*(y1-y3) +
                         mb*(x1+x2) -
                         ma*(x2+x3))/(2*(mb-ma))
                    y = -1/ma*(x-(x1+x2)/2) + (y1+y2)/2
                    self._center = (x, y)
        return self._center

def divide_conquer():
    class Delaunay:
        pass
    
    def form(points):
        if len(points) > 3:
            mid = len(points)//2
            left = form(points[:mid])
            right = form(points[mid:])
            return merge(left, right)
        else:
            t = Triangle(points)
            return
    def merge(left, right):
        pass
    
    width = 100.0
    height = 100.0
    
    points = [Point(random()*width, random()*height) for i in range(10)]
    points.sort()    
    
    # draw image
    size = (int(width), int(height))
    image = Image.new('RGB', size, (128,128,128))
    draw = ImageDraw.Draw(image)
    for point in points:
        r = 1.0
        draw.ellipse((point[0]-r, point[1]-r, point[0]+r, point[1]+r), (255, 0, 0))
    image.save('delaunay.png')

def delaunay():
    class Triangle:
        def __init__(self, points):
            self._center = None
            self.points = points
        def center(self):
            if self._center == None:
                (x1, y1), (x2, y2), (x3, y3) = self.points
                ma = (y2-y1)/(x2-x1)
                mb = (y3-y2)/(x3-x2)
                if ma != mb:                    
                    x = (ma*mb*(y1-y3) +
                         mb*(x1+x2) -
                         ma*(x2+x3))/(2*(mb-ma))
                    y = -1/ma*(x-(x1+x2)/2) + (y1+y2)/2
                    self._center = (x, y)
                # check for coincident lines
                # check for 
            return self._center
    # setup
    width = 100.0
    height = 100.0
    # create point at 0,0, create 2 triangles in square
    points = [(0.0,0.0)]
    triangles = []
    triangles.append(Triangle([(0.0,0.0),(0.0, height), (width, 0.0)]))
    triangles.append(Triangle([(0.0, height), (width, 0.0), (width, height)]))
    
    # add point, remove inside triangles, create new ones
    point = (random()*width, random()*height)
    
    
    # draw triangles
    size = (int(width), int(height))
    image = Image.new('RGB', size, BARRIER_COLOR)
    draw = ImageDraw.Draw(image)
    draw.rectangle((0,0,width,height), outline=(32,32,32))
    for triangle in triangles:
        points = [(int(x), int(y)) for x, y in triangle.points]
        for i in range(len(triangle.points)):
            image = draw_line(image, points[i-1], points[i], size)
    image.save('delaunay.png')


def distance(x1, y1, x2, y2, width, height):
    x1 = x1 % width
    x2 = x2 % width
    y1 = y1 % height
    y2 = y2 % height
    d_x = min(abs(x1 - x2), width - abs(x1 - x2))
    d_y = min(abs(y1 - y2), height - abs(y1 - y2))
    return d_x**2 + d_y**2

def voronoi(players=4):
    width = randrange(64, 256)
    height = randrange(64, 256)
    point_count = randrange(players*3, players*6)
    min_dist = width * height / point_count
    print('%s, %s  %s %s' % (width, height, min_dist, sqrt(min_dist)))
    px, py = 0, 0
    points = []
    while min_dist > 100 and len(points) < point_count:
        while min_dist > 100:
            px, py = randrange(width), randrange(height)
            for nx, ny in points:
                if distance(px, py, nx, ny, width, height) < min_dist:
                    break
            else:
                break
            min_dist -= 1
        points.append((px, py))
    #for px, py in points:
    #    for nx, ny in points:
    #        print('(%s)-(%s) = %s' % ((px,py),(nx,ny),distance(px, py, nx, ny, width, height)))
    path = {}
    closest = {}
    for p_x, p_y in points:
        nearest = {}
        for n_x, n_y in points:
            if (p_x, p_y) != (n_x, n_y):
                dist = distance(p_x, p_y, n_x, n_y, width, height)
                nearest[dist] = (n_x, n_y)
        sorted = nearest.keys()
        sorted.sort()
        path[(p_x, p_y)] = [nearest[key] for key in sorted[:3]]
        closest[(p_x, p_y)] = sorted[0]
    image = Image.new('RGB', (width, height), BARRIER_COLOR)
    draw = ImageDraw.Draw(image)
    for point in points:
        image.putpixel(point, (0,0,0))
        size = int(sqrt(closest[point]))//2 - 2
        draw.ellipse((point[0]-size, point[1]-size, point[0]+size, point[1]+size),
                fill=LAND_COLOR, outline=LAND_COLOR)
    from_point = (width//2, height//2)
    for point, path_neighbors in path.items():
        offset = (width//2 - point[0], height//2 - point[1])
        image = ImageChops.offset(image, offset[0], offset[1])
        draw = ImageDraw.Draw(image)
        for neighbor in path_neighbors:
            to_point = ((neighbor[0] + offset[0]) % width, (neighbor[1] + offset[1]) % height)
            draw.line((from_point, to_point), width=randrange(3,6), fill=LAND_COLOR)
        image = ImageChops.offset(image, -offset[0], -offset[1])
    image = image.resize((width*4, height*4))
    image.save('voronoi.png')
    
def random_box():
    players = 4
    width = randrange(16, 64) * 2
    height = randrange(16, 64) * 2
    m = [[BARRIER for x in range(width)] for y in range(height)]
    def carve(row, col):
        if m[row][col] == BARRIER:
            m[row][col] = LAND
    for box in range(randrange(7,14)):
        l = randrange(width-5)
        t = randrange(height-5)
        r = randrange(l+2, min(l+width//4,width))
        b = randrange(t+2, min(t+height//4,height))
        for y in range(t, b+1):
            for x in range(l, r+1):
                carve(y, x)
        for y in range(height-b-1, height-t):
            for x in range(l, r+1):
                carve(y, x)
        for y in range(t, b+1):
            for x in range(width-r-1, width-l):
                carve(y, x)
        for y in range(height-b-1, height-t):
            for x in range(width-r-1, width-l):
                carve(y, x)
        if box == 0:
            m[t][l] = 1
            m[height-t-1][l] = 2
            m[t][width-l-1] = 3
            m[height-t-1][width-l-1] = 4
            for y in range(t+1, height-t-1):
                carve(y, l)
                carve(y, width-l-1)
            for x in range(l+1, width-l-1):
                carve(t, x)
                carve(height-t-1, x)
    return ant_map(m)

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

euclidean_cache = {}
def euclidean_distance(loc1, loc2, size):
    row1, col1 = loc1
    row2, col2 = loc2
    rows, cols = size
    d_row = min(abs(row1-row2),rows-abs(row1-row2))
    d_col = min(abs(col1-col2),cols-abs(col1-col2))
    key = (d_row, d_col)
    if key in euclidean_cache:
        return euclidean_cache[key]
    value = sqrt(d_row**2 + d_col**2)
    euclidean_cache[key] = value
    return value

def copy(value, size):
    return size+value
def mirror(value, size):
    return size*2-value-1
def flip(value, size):
    return size-value-1

def both_point(point, size, funcs):
    return (funcs[0](point[0], size[0]), funcs[1](point[1], size[1]))
def vert_point(point, size, funcs):
    return (funcs[0](point[0], size[0]), point[1])
def horz_point(point, size, funcs):
    return (point[0], funcs[0](point[1], size[1]))
# TODO: ensure square or change output size
def flip_point(point, size, funcs):
    return (funcs[0](point[1], size[1]), funcs[1](point[0], size[0]))

def vert_increase(size, count):
    return (size[0]*count, size[1])
def horz_increase(size, count):
    return (size[0], size[1]*count)

vert_copy = (vert_point, (copy,), vert_increase)
vert_mirror = (vert_point, (mirror,), vert_increase)
vert_rotate = (both_point, (mirror, flip), vert_increase)
horz_copy = (horz_point, (copy,), horz_increase)
horz_mirror = (horz_point, (mirror,), horz_increase)
horz_rotate = (both_point, (flip, mirror), horz_increase)    

def extend(funcs, points, size, count=2):
    if type(points) == list:
        points = {point: x for x, point in enumerate(points)}
    rows, cols = size
    new_points = {}
    for point, id in points.items():
        new_points[point] = id
        for c in range(1,count):
            new_points[funcs[0](point, funcs[2](size, c), funcs[1])] = id
    return new_points, funcs[2](size, count)
               
def make_symmetric(points, size, players):
    # TODO: shearing, like antimatroid
    #    3, 4 and 7 player can be made fair
    # TODO: rotational
    #    2, 4 and 8 can be made fairish
    
    # pick random grid size
    divs = [i for i in range(1,players+1) if players%i==0]
    row_sym = choice(divs)
    col_sym = players/row_sym
    grid = (row_sym, col_sym)
    
    newsize = (size[0]*row_sym, size[1]*col_sym)
    newpoints = []
    comps = []

    if row_sym % 2 == 0:
        points, size = extend(choice((vert_copy, vert_mirror, vert_rotate)), points, size)
        row_sym /= 2
    if row_sym > 1:
        points, size = extend(vert_copy, points, size, row_sym)

    if col_sym % 2 == 0:
        points, size = extend(choice((horz_copy, horz_mirror, horz_rotate)), points, size)
        col_sym /= 2
    if col_sym > 1:
        points, size = extend(horz_copy, points, size, col_sym)
    
    return points, size, grid

def random_points(count, size, spacing, distance):
    rows, cols = size
    points = []
    failures = 0
    for c in range(count):
        while True:
            point = (randrange(rows), randrange(cols))
            for other_point in points:
                if distance(point, other_point, size) < spacing:
                    failures += 1
                    if failures > 100000:
                        return points
                    break
            else:
                break
        points.append(point)
    return points

def random_points_unique(count, size, spacing, distance):
    rows, cols = size
    avail_rows = list(range(rows))
    avail_cols = list(range(cols))
    points = []
    failures = 0
    for c in range(count):
        while True:
            point = (choice(avail_rows), choice(avail_cols))
            for other_point in points:
                if distance(point, other_point, size) < spacing:
                    failures += 1
                    if failures > 100000:
                        return points
                    break
            else:
                break
        points.append(point)
        avail_rows.remove(point[0])
        if len(avail_rows) == 0:
            avail_rows = list(range(rows))
        avail_cols.remove(point[1])
        if len(avail_cols) == 0:
            avail_cols = list(range(cols))
    return points

def cells(size, points, min_gap=5, max_braids=1000, openness=0.25, distance=euclidean_distance):
    rows, cols = size
    size = (rows, cols)
    m = [[LAND for col in range(cols)] for row in range(rows)]
    
    # ensure points is a dict with id's
    if type(points) == dict:
        points = {point: x for x, point in enumerate(points)}
        
    # undirected node graph
    neighbor = defaultdict(list) 
    # list of barriers to remove when carving a passage between nodes
    barrier = defaultdict(list)
    
    for row in range(rows):
        for col in range(cols):
            # TODO: improve speed with nearest neighbor queries
            distances = {loc: distance((row,col), loc, size) for loc in points.keys()}
            cutoff = min(distances.values()) + 1
            closest = [point for point, d in distances.items() if d <= cutoff]
            comps = set([points[point] for point in closest])
            
            # find if there are unique complement sets that are closest
            # if not, this is probably a mirrored edge and the points should be
            # considered one cell
            #if closest[0] + 1 >= closest[1]:
            if len(closest) > 1:
                if comps_found:
                    m[row][col] = BARRIER
                    # find all starting points that contributed to the barrier,
                    # mark them as neighbors, add to barrier dict
                    if len(nearest) == 2:
                        neighbor[nearest[0]].append(nearest[1])
                        neighbor[nearest[1]].append(nearest[0])
                        # note: a path from one point to another could have 2 barrier sections if they touch
                        #       left and right or top and bottom due to wrapping
                        #       the path midpoint attempts to choose one and only one barrier section
                        m_row, m_col = mid_point(points[nearest[0]], points[nearest[1]], size)
                        if (row_distance(m_row, row, rows) <= rows//4 and
                                col_distance(m_col, col, cols) <= cols//4):
                            barrier[tuple(nearest)].append((row, col))
                    else:
                        pass
                        # barrier[tuple(nearest)].append((row, col))
                else:
                    # todo: similar logic to wrap around fix, but for
                    #       complementary points
                    pass
            else:
                # note: a cell could wrap around vertically or horizontally
                #       depending on the placement of other cells
                #       this draws a barrier halfway around on the other side
                nearest = distances.index(min(distances))
                if (row_distance(row, points[nearest][0], rows) >= rows//2 or
                    col_distance(col, points[nearest][1], cols) >= cols//2):
                    m[row][col] = BARRIER # this barrier can't be carved
                #m[row][col] = distances.index(closest[0])
    
    # add starting positions
    #for i, (row, col) in enumerate(points):
    #    m[row][col] = i
    
    # remove small gaps
    for path in barrier.keys():
        if len(path) == 2:
            if len(barrier[path]) < min_gap:
                neighbor[path[0]].remove(path[1])
                neighbor[path[1]].remove(path[0])
                
    # carve passages function to pass to maze function
    def carve(path):
        #print('%s-%s (%s,%s)-(%s,%s) %s,%s' % (chr(path[0]+97), chr(path[1]+97),
        #                                       points[path[0]][0], points[path[0]][1],
        #                                       points[path[1]][0], points[path[1]][1],
        #                                       m_row, m_col))
        paths = [path]
        if comps != None:
            paths = zip(comps[path[0]],comps[path[1]])
        for path in paths:
            path = tuple(sorted(path))
            for row, col in barrier[path]:
                m[row][col] = LAND
                
            
    carved = growing_tree(neighbor, carve, max_braids=max_braids, openness=openness)
    #for c1, cs in carved.items():
    #    for c2 in cs:
    #        print "%s-%s " % (chr(c1+97),chr(c2+97)),
    #print
    #for n in sorted(barrier.keys()):
    #    if len(n) == 2:
    #        print("%s : %s" % ('-'.join([chr(x+97) for x in n]),
    #                           ' '.join([','.join([str(s2) for s2 in s])
    #                                     for s in barrier[n]])))
    return m

def growing_tree(nodes, carve, max_braids=1000, openness=0.5):
    cells = [choice(nodes.keys())]
    visited = cells[:]
    carved = defaultdict(list) # modified node graph
    #carved[cells[0]].append(cells[0])
    new = True # track real dead ends, not backtracked forks
    while len(cells) > 0:
        # tune this for different generation methods
        # recursive backtracker
        #index = -1                     
        # Prim's algorithm
        index = randrange(len(cells)) 
        
        cell = cells[index]
        unvisited = [node for node in nodes[cell] if not node in visited]
        if len(unvisited) > 0:
            next = choice(unvisited)
            carve((cell, next))
            carved[next].append(cell)
            carved[cell].append(next)
            visited.append(next)
            cells.append(next)
            new = True
        else:
            if max_braids > 0 and (new or bool(random() < openness)):
                # tune this for different braiding methods
                # random
                #braid = choice([n for n in nodes[cell] if not n in carved[cell]])
                # longest loop
                braid = ([c for c in cells if c in nodes[cell]]+nodes[cell])[0]
                
                carve((cell, braid))
                carved[cell].append(braid)
                max_braids -= 1
            cells.pop(index)
            new = False
    return carved

def cell_maze():
    # these control how much barrier carving will happen
    max_braids = 100 # points where the maze can create a loop
    openness = 0.01  # chance that non dead ends can create a loop
    
    size = (100,100)
    point_count = 100
    spacing= 5
    points = random_points(point_count, size, spacing, euclidean_distance)
    
def ant_map(m):
    tmp = 'rows %s\ncols %s\n' % (len(m), len(m[0]))
    players = {}
    for row in m:
        tmp += 'm '
        for col in row:
            if col == LAND:
                tmp += '.'
            elif col == BARRIER:
                tmp += '%'
            elif col == FOOD:
                tmp += '*'
            elif col == UNSEEN:
                tmp += '?'
            else:
                players[col] = True
                tmp += chr(col + 97)
        tmp += '\n'
    tmp = ('players %s\n' % len(players)) + tmp
    return tmp

def file_to_map(filename):
    f = open(filename, 'r')
    m = []
    for line in f:
        if line.startswith('rows '):
            rows = int(line[5:])
        elif line.startswith('cols '):
            cols = int(line[5:])
        elif line.startswith('M '):
            data = line[2:-1]
            m.append([])
            for c in data:
                if c == '%':
                    m[-1].append(BARRIER)
                elif c == '.':
                    m[-1].append(LAND)
                elif c >= 'a' and c <= 'z':
                    m[-1].append(LAND)
                else:
                    print('found "%s"' % c)
    f.close()
    return m

def map_to_png(m, output_filename):
    rows = len(m)
    cols = len(m[0])
    image = Image.new('RGB', (cols*2, rows*2), FOOD_COLOR)
    for row, row_data in enumerate(m):
        for col, c in enumerate(row_data):
            #image.putpixel((col, row), COLOR[c])
            #image.putpixel((col+cols, row), COLOR[c])
            #image.putpixel((col, row+rows), COLOR[c])
            #image.putpixel((col+cols, row+rows), COLOR[c])

            image.putpixel((col, row), COLOR[c])
            image.putpixel((cols*2-col-1, row), COLOR[c])
            image.putpixel((col, rows*2-row-1), COLOR[c])
            image.putpixel((cols*2-col-1, rows*2-row-1), COLOR[c])
    image.save(output_filename)

def main():
    #map_to_png(sys.argv[1])
    #seed(0)
    size = (100, 100)
    points = random_points_unique(400, size, 6, euclidean_distance)
    m = cells(size, points, max_braids=choice((0,1000)), openness=random())
    #print(ant_map(m))
    #map_to_png(m, "test.png")

def make_text(points, size):
    tmp = ''
    rows, cols = size
    if cols > rows:
        for row in range(rows):
            for col in range(cols):
                if (row,col) in points:
                    tmp += chr(points[(row,col)]+97)
                else:
                    tmp += '.'
            tmp += '\n'
    else:
        for col in range(cols):
            for row in range(rows):
                if (row,col) in points:
                    tmp += chr(points[(row,col)]+97)
                else:
                    tmp += '.'
            tmp += '\n'
    return tmp

if __name__ == '__main__':
    p = [(0,0),(0,1)]
    s = (2,2)
    p, s, g = make_symmetric(p, s, randrange(2,12))
    t = make_text(p, s)
    print("size: %s\ngrid: %s\n\n%s" % (s, g, t))
    
    #import cProfile
    #cProfile.run('main()')
    
    
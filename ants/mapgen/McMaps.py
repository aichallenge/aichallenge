#!/usr/bin/python
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
WATER = -4
UNSEEN = -5

WALL_COLOR = (128, 128, 128)
LAND_COLOR = (139, 69, 19)
FOOD_COLOR = (255, 255, 255)

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
    image = Image.new('RGB', size, WALL_COLOR)
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
    image = Image.new('RGB', (width, height), WALL_COLOR)
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
    m = [[WATER for x in range(width)] for y in range(height)]
    def carve(row, col):
        if m[row][col] == WATER:
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
    row1, col1 = loc1
    row2, col2 = loc2
    rows, cols = size
    row1, row2 = sorted((row1,row2))
    m_row = (row1 + row2)//2
    if row2 - row1 > rows:
        m_row = (m_row + rows) % rows
    m_col = (col1 + col2)//2
    if col2 - col1 > cols:
        m_col = (m_col + cols) % cols
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

def euclidean_distance(loc1, loc2, size):
    row1, col1 = loc1
    row2, col2 = loc2
    rows, cols = size
    d_row = min(abs(row1-row2),rows-abs(row1-row2))
    d_col = min(abs(col1-col2),cols-abs(col1-col2))
    return sqrt(d_row**2 + d_col**2)

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
    count = min(count, max(rows, cols))
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
        avail_cols.remove(point[1])
    return points

def cells(size, starts, distance, max_braids=1000, openness=0.25):
    rows, cols = size
    size = (rows, cols)
    m = [[LAND for col in range(cols)] for row in range(rows)]

    # undirected node graph
    neighbor = defaultdict(list) 
    # list of water to remove when carving a passage between nodes
    water = defaultdict(list)
    
    for row in range(rows):
        for col in range(cols):
            distances = [distance((row,col),s_loc,size)
                         for s_loc in starts]
            #closest = sorted(distances)
            closest = [d for d in distances if d - 1 <= min(distances)]
            #if closest[0] + 1 >= closest[1]:
            if len(closest) > 1:
                m[row][col] = WATER
                # find all starting points that contributed to the water wall,
                # mark them as neighbors, add to water wall dict
                nearest = [i for i, x in enumerate(distances)
                           if x in closest]
                if len(nearest) == 2:
                    neighbor[nearest[0]].append(nearest[1])
                    neighbor[nearest[1]].append(nearest[0])
                water[tuple(nearest)].append((row, col))
            else:
                # note: a cell could wrap around vertically or horizontally
                #       depending on the placement of other cells
                #       this draws a wall halfway around on the other side
                nearest = distances.index(min(distances))
                if (row_distance(row, starts[nearest][0], rows) >= rows//2 or
                    col_distance(col, starts[nearest][1], cols) >= cols//2):
                    m[row][col] = WATER # this water can't be carved
                #m[row][col] = distances.index(closest[0])
    for i, (row, col) in enumerate(starts):
        m[row][col] = i
        
    # carve passages function to pass to maze function
    # note: a path from one point to another could have 2 walls if they touch
    #       left and right or top and bottom due to wrapping
    #       the path midpoint attempts to choose one and only one wall
    def carve(path):
        path = tuple(sorted(path))
        m_row, m_col = mid_point(starts[path[0]], starts[path[1]], size)
        for row, col in water[path]:
            if (row_distance(m_row, row, rows) > rows//4 or
                    col_distance(m_col, col, cols) > cols//4):
                continue
            m[row][col] = LAND
            
    carved = growing_tree(neighbor, carve, max_braids=max_braids, openness=openness)
    #for c1, cs in carved.items():
    #    for c2 in cs:
    #        print "%s-%s " % (chr(c1+97),chr(c2+97)),
    #for n, squares in water.items():
    #    print("%s : %s" % ('-'.join([chr(x+97) for x in n]),
    #                       ' '.join([','.join([str(s2) for s2 in s]) for s in squares])))
    return ant_map(m)

def growing_tree(nodes, carve, max_braids=1000, openness=0.5):
    cells = [choice(nodes.keys())]
    visited = cells[:]
    carved = defaultdict(list) # key is cell carved into, value is last cell where carve came from
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
    # these control how much wall carving will happen
    max_braids = 100 # points where the maze can create a loop
    openness = 0.01  # chance that non dead ends can create a loop
    
def ant_map(m):
    tmp = 'rows %s\ncols %s\n' % (len(m[0]), len(m))
    for row in m:
        tmp += 'M '
        for col in row:
            if col == LAND:
                tmp += '.'
            elif col == WATER:
                tmp += '%'
            elif col == FOOD:
                tmp += '*'
            else:
                tmp += chr(col + 97)
        tmp += '\n'
    return tmp
    
if __name__ == '__main__':
    size = (40, 80)
    starts = random_points(26, size, 6, euclidean_distance)
    print(cells(size, starts, euclidean_distance, choice((0,1000)), random()))

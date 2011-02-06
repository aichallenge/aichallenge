#!/usr/bin/env python
from copy import deepcopy
from random import randrange
from math import sqrt
import Image
import os

LAND = 0
FOOD = -1
WALL = -2
HILL = -3
BODY = -4
CONFLICT = -5

WALL_COLOR = (128, 128, 128)
LAND_COLOR = (139, 69, 19)
HILL_COLOR = (0, 0, 0)
FOOD_COLOR = (255, 255, 255)
BODY_COLOR = (64, 64, 64)
CONFLICT_COLOR = (255, 128, 128)

PLAYER_COLOR = [(204, 0, 0),    # red
                (255, 255, 0),  # yellow
                (51, 153, 0),   # green
                (51, 51, 153),  # blue
                (154, 51, 154), # purple
                (50, 154, 154), # teal
                (254, 154, 2),  # orange
                (154, 206, 51)] # sage

class AntMap:
    def __init__(self, filename):
        self.LAND = 0
        self.FOOD = -1
        self.WALL = -2
        self.HILL = -3
        self.do_orders = self.do_orders_3
        self.do_death = self.do_death_4
        self.do_birth = self.do_birth_3
        self.do_food = self.do_food_1
        self.do_score = self.do_score_1
        self.order_map = None
        self.ant_list = {}
        self.food_list = {}
        self.hill_list = {}
        if os.path.splitext(filename)[1].lower() == '.png':
            self.load_image(filename)
        elif os.path.splitext(filename)[1].lower() == '.txt':
            self.load_text(filename)

    def clone(self):
        new_map = AntMap('')
        new_map.width = self.width
        new_map.height = self.height
        new_map.land_area = self.land_area
        new_map.wall_area = self.wall_area
        new_map.num_players = self.num_players
        new_map.player_colors = self.player_colors[:]
        new_map.image_colors = self.image_colors[:]
        new_map.map = deepcopy(self.map)
        new_map.order_map = None
        new_map.ant_list = deepcopy(self.ant_list)
        new_map.food_list = self.food_list
        new_map.hill_list = self.hill_list
        return new_map

    def load_text(self, filename):
        num_players = 0
        map = None
        f = open(filename, 'r')
        last_row = 0
        for line in f:
            if line[0] == 'D':
                data = line.split()
                self.width = int(data[1])
                self.height = int(data[2])
                self.land_area = self.width * self.height
                self.wall_area = 0
                self.num_players = int(data[3])
                self.player_colors = PLAYER_COLOR[:self.num_players]
                self.image_colors = [LAND_COLOR] + self.player_colors + [CONFLICT_COLOR, 
                                        BODY_COLOR, HILL_COLOR, WALL_COLOR, FOOD_COLOR]
                self.map = [[0 for i in range(self.height)] for i in range(self.width)]
            elif line[0] == 'W':
                data = line.split()
                x1 = int(data[1])
                y1 = int(data[2])
                x2 = int(data[3])
                y2 = int(data[4])
                for x in range(x1, x2+1):
                    for y in range(y1, y2+1):
                        if self.map[x][y] != WALL:
                            self.map[x][y] = WALL
                            self.wall_area += 1
                            self.land_area -= 1
            elif line[0] == 'M':
                data = line.split()
                for x in range(len(data[1])):
                    if data[1][x] == 'X':
                        if self.map[x][last_row] != WALL:
                            self.map[x][last_row] = WALL
                            self.wall_area += 1
                            self.land_area -= 1
                    elif data[1][x] == '.':
                        if self.map[x][last_row] == WALL:
                            self.map[x][last_row] == LAND
                            self.wall_area -= 1
                            self.land_area += 1
                    elif data[1][x] == '*':
                        if self.map[x][last_row] != WALL:
                            self.map[x][last_row] = FOOD
                            self.food_list[(x,last_row)] = True
                    elif data[1][x] == '!':
                        if self.map[x][last_row] != WALL:
                            self.map[x][last_row] = HILL
                            self.hill_list[(x,last_row)] = True
                            self.wall_area += 1
                            self.land_area -= 1
                    elif ord(data[1][x]) >= 97 and ord(data[1][x]) <= 112:
                        if self.map[x][last_row] != WALL:
                            self.map[x][last_row] = ord(data[1][x]) - 96
                            self.ant_list[(x,last_row)] = ord(data[1][x]) - 96
                last_row += 1
            elif line[0] == 'F':
                data = line.split()
                x = int(data[1])
                y = int(data[2])
                if self.map[x][y] == LAND:
                    self.map[x][y] = FOOD
                    self.food_list[(x,y)] = True
            elif line[0] == 'A':
                data = line.split()
                x = int(data[1])
                y = int(data[2])
                owner = int(data[3])
                if self.map[x][y] == LAND:
                    self.map[x][y] = owner
                    self.ant_list[(x,y)] = owner
            elif line[0] == 'H':
                data = line.split()
                x = int(data[1])
                y = int(data[2])
                if self.map[x][y] == LAND:
                    self.map[x][y] = HILL
                    self.hill_list[(x,y)] = True
                    self.wall_area += 1
                    self.land_area -= 1

    def load_image(self, filename):
        image = Image.open(filename)
        self.width, self.height = image.size
        self.num_players = 0
        self.land_area = 0
        self.wall_area = 0
        self.player_colors = []
        self.map = [[LAND for i in range(self.height)] for i in range(self.width)]
        for x in range(self.width):
            for y in range(self.height):
                pixel = image.getpixel((x,y))
                if pixel == WALL_COLOR:
                    self.map[x][y] = WALL
                    self.wall_area += 1
                elif pixel == HILL_COLOR:
                    self.map[x][y] = HILL
                    self.hill_list[(x,y)] = True
                    self.wall_area += 1
                else:
                    self.land_area += 1
                    if pixel == LAND_COLOR:
                        self.map[x][y] = LAND
                    elif pixel == FOOD_COLOR:
                        self.map[x][y] = FOOD
                        self.food_list[(x,y)] = True
                    elif pixel in self.player_colors:
                        self.map[x][y] = self.player_colors.index(pixel)
                        self.ant_list[(x,y)] = self.map[x][y]
                    else:
                        self.num_players += 1
                        self.player_colors.append(pixel)
                        self.map[x][y] = self.num_players
                        self.ant_list[(x,y)] = self.map[x][y]
        self.image_colors = [LAND_COLOR] + self.player_colors + [CONFLICT_COLOR, 
                                BODY_COLOR, HILL_COLOR, WALL_COLOR, FOOD_COLOR]

    def render(self):
        image = Image.new('RGB', (self.width, self.height), LAND_COLOR)
        for x in range(self.width):
            for y in range(self.height):
                if self.map[x][y] != LAND:
                    image.putpixel((x, y), self.image_colors[self.map[x][y]])
        return image

    def ants(self):
        return [(x,y,o) for (x,y),o in self.ant_list.items()]

    def food(self):
        return list(self.food_list.keys())

    def hills(self):
        return list(self.hill_list.keys())

    def nearby_ants(self, x, y, exclude=0, min_dist=1, max_dist=2):
        mx = int(sqrt(max_dist))
        for dx in range(-mx,mx+1):
            for dy in range(-mx,mx+1):
                if dx**2 + dy**2 <= max_dist and dx**2 + dy**2 >= min_dist:
                    nx = (x + dx) % self.width
                    ny = (y + dy) % self.height
                    if self.map[nx][ny] > 0 and self.map[nx][ny] != exclude:
                        yield (nx, ny, self.map[nx][ny])

    def do_orders_3(self, player, orders):
        direction = {'N': (0, -1),
                     'S': (0, 1),
                     'E': (1, 0),
                     'W': (-1, 0)}
        src = {}
        dest = {}
        for order in orders:
            x1, y1, x2, y2 = order
            #x1, y1, d = order
            #x2 = (x1 + direction[d][0]) % self.width
            #y2 = (y1 + direction[d][1]) % self.height
            if src.has_key((x1,y1)): # order already given
                continue
            if self.map[x1][y1] != player: # must move *your* ant
                continue
            src[(x1,y1)] = True
            if self.map[x2][y2] in (FOOD, WALL, HILL, BODY): # blocking things
                if dest.has_key((x1,y1)):
                    dest[(x1,y1)].append((x1,y1))
                else:
                    dest[(x1,y1)] = [(x1,y1)]
            else:
                if dest.has_key((x2,y2)):
                    dest[(x2,y2)].append((x2,y2))
                else:
                    dest[(x2,y2)] = [(x2,y2)]
        # TODO: check for unordered ants and give them hold orders
        for x1, y1 in src.keys():
            self.map[x1][y1] = LAND
            try:
                del self.ant_list[(x1,y1)]
            except:
                print "delete ant error at %s %s" % (x1,y1)
                print order
                print self.map[x1][y1]
        # check for conflicts
        for (x2, y2), srcs in dest.items():
            if len(srcs) > 1 or self.map[x2][y2] > 0 or self.map[x2][y2] == CONFLICT:
                self.map[x2][y2] = CONFLICT
                if self.ant_list.has_key((x2,y2)):
                    del self.ant_list[(x2,y2)]
            else:
                self.map[x2][y2] = player
                self.ant_list[(x2,y2)] = player

    def resolve_orders(self):
        for x in range(self.width):
            for y in range(self.height):
                if self.map[x][y] == CONFLICT:
                    self.map[x][y] = LAND

    # the most forces near food source claim the new ant
    def do_birth_1(self):
        new_ants = {}
        for fx, fy in self.food():
            support = [0 for i in range(self.num_players+1)]
            for nx, ny, nowner in self.nearby_ants(fx, fy):
                support[nowner] += 1
            max_support = max(support)
            if max_support > 0:
                if support.count(max_support) == 1:
                    new_ants[(fx,fy)] = support.index(max_support)
        for (x, y), player in new_ants.items():
            self.map[x][y] = player
            self.ant_list[(x,y)] = player
            del self.food_list[(x,y)]

    # must have only 1 force near the ant to create a new ant, prefered method
    def do_birth_2(self):
        new_ants = {}
        for fx, fy in self.food():
            owner = 0
            for nx, ny, nowner in self.nearby_ants(fx, fy):
                if owner == 0:
                    owner = nowner
                elif owner != nowner:
                    break
            else:
                if owner != 0:
                    new_ants[(fx,fy)] = owner
        for (x, y), player in new_ants.items():
            self.map[x][y] = player
            self.ant_list[(x,y)] = player
            del self.food_list[(x,y)]

    # must have only 1 force near the ant to create a new ant, prefered method
    #  and food in contention is eliminated
    def do_birth_3(self):
        new_ants = {}
        for fx, fy in self.food():
            owner = 0
            for nx, ny, nowner in self.nearby_ants(fx, fy, 0, 1, 9):
                if owner == 0:
                    owner = nowner
                elif owner != nowner:
                    self.map[fx][fy] = LAND
                    del self.food_list[(fx,fy)]
                    break
            else:
                if owner != 0:
                    new_ants[(fx,fy)] = owner
        for (x, y), player in new_ants.items():
            self.map[x][y] = player
            self.ant_list[(x,y)] = player
            del self.food_list[(x,y)]

    # any less occupied ant near you kills you, preferred method
    def do_death_1(self):
        old_ant = []
        for ax, ay, aowner in self.ants():
            enemy_ants = list(self.nearby_ants(ax, ay, aowner))
            for ex, ey, eowner in enemy_ants:
                if len(list(self.nearby_ants(ex, ey, eowner))) <= len(enemy_ants):
                    old_ant.append((ax,ay))
                    break
        for x, y in old_ant:
            self.map[x][y] = LAND
            del self.ant_list[(x,y)]

    # more enemies must be near you than friendlies, this causes blocking lines of ants to be possible
    #  but moving directly on enemies will kill them, this may be desireable
    def do_death_2(self):
        old_ant = []
        for ax, ay, aowner in self.ants():
            support = [0 for i in range(self.num_players+1)]
            for nx, ny, nowner in self.nearby_ants(ax, ay):
                support[nowner] += 1
            if sum(support) - support[aowner] >= support[aowner]:
                old_ant.append((ax,ay))
        for x, y in old_ant:
            self.map[x][y] = LAND
            del self.ant_list[(x,y)]

    # ants within range kill you
    def do_death_3(self):
        old_ant = []
        for ax, ay, aowner in self.ants():
            for ex, ey, eowner in self.nearby_ants(ax, ay, aowner):
                old_ant.append((ax,ay))
                break
        for x, y in old_ant:
            self.map[x][y] = LAND
            del self.ant_list[(x,y)]

    # 1:1(:1:ect) kill ratio
    def do_death_4(self):
        ant_group = []
        def find_enemy(x, y, owner, min_dist, max_dist):
            for nx, ny, nowner in self.nearby_ants(x, y, owner, min_dist, max_dist):
                if not (nx, ny) in ant_group:
                    ant_group.append((nx,ny))
                    find_enemy(nx, ny, nowner, min_dist, max_dist)
        for distance in range(1,10):
            for ax, ay, aowner in self.ants():
                if self.map[ax][ay] != LAND:
                    ant_group = [(ax, ay)]
                    find_enemy(ax, ay, aowner, distance, distance)
                    if len(ant_group) > 1:
                        for ex, ey in ant_group:
                            self.map[ex][ey] = LAND
                            del self.ant_list[(ex,ey)]

    def do_food_1(self, amount=1):
        for f in range(amount):
            for t in range(10):
                x = randrange(self.width)
                y = randrange(self.height)
                if self.map[x][y] == LAND:
                    self.map[x][y] = FOOD
                    self.food_list[(x,y)] = True
                    break

    def do_score_1(self):
        score = [0 for i in range(self.num_players + 1)]
        for fx, fy in self.food():
            owner = 0
            for nx, ny, nowner in self.nearby_ants(fx, fy):
                if owner == 0:
                    owner = nowner
                elif owner != nowner:
                    break
            else:
                if owner != 0:
                    score[owner] += 1
        return score

    def remaining_players(self):
        exists = [0 for i in range(self.num_players+1)]
        for x, y, owner in self.ants():
            exists[owner] = 1
        return sum(exists)

    def game_over(self):
        return self.remaining_players() <= 1

if __name__ == '__main__':

    def save_image(map, filename):
        img = map.render()
        scale = 4
        new_size = (img.size[0] * scale, img.size[1] * scale)
        img = img.resize(new_size)
        img.save(filename + ".png")

    import sys
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option('-f', '--file',
                      dest='filename',
                      help='file name of map text file')
    options, args = parser.parse_args(sys.argv)
    map = AntMap(options.filename)
    save_image(map, options.filename[:-4])

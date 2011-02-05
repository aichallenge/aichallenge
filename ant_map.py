#!/usr/bin/env python
from copy import deepcopy
from random import randrange
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
        if os.path.splitext(filename)[1].lower() == '.png':
            self.load_image(filename)
        elif os.path.splitext(filename)[1].lower() == '.txt':
            self.load_text(filename)
        self.do_orders = self.do_orders_1
        self.do_death = self.do_death_2
        self.do_birth = self.do_birth_1
        self.do_food = self.do_food_1
        self.do_score = self.do_score_1
        self.order_map = None
        self.conflict = {}
        self.ant_list = {}
        self.food_list = {}

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
        new_map.conflict = {}
        new_map.ant_list = {}
        new_map.food_list = {}
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
                self.image_colors = [LAND_COLOR] + self.player_colors + [HILL_COLOR,
                                        WALL_COLOR, FOOD_COLOR]
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
                last_row += 1
            elif line[0] == 'F':
                data = line.split()
                x = int(data[1])
                y = int(data[2])
                if self.map[x][y] == LAND:
                    self.map[x][y] = FOOD
            elif line[0] == 'A':
                data = line.split()
                x = int(data[1])
                y = int(data[2])
                owner = int(data[3])
                if self.map[x][y] == LAND:
                    self.map[x][y] = owner
            elif line[0] == 'H':
                data = line.split()
                x = int(data[1])
                y = int(data[2])
                if self.map[x][y] == LAND:
                    self.map[x][y] = HILL
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
                    self.wall_area += 1
                else:
                    self.land_area += 1
                    if pixel == LAND_COLOR:
                        self.map[x][y] = LAND
                    elif pixel == FOOD_COLOR:
                        self.map[x][y] = FOOD
                    elif pixel in self.player_colors:
                        self.map[x][y] = self.player_colors.index(pixel)
                    else:
                        self.num_players += 1
                        self.player_colors.append(pixel)
                        self.map[x][y] = self.num_players
        self.image_colors = [LAND_COLOR] + self.player_colors + [HILL_COLOR,
                                WALL_COLOR, FOOD_COLOR]

    def render(self):
        image = Image.new('RGB', (self.width, self.height), LAND_COLOR)
        for x in range(self.width):
            for y in range(self.height):
                if self.map[x][y] != LAND:
                    image.putpixel((x, y), self.image_colors[self.map[x][y]])
        return image

    def ants(self):
        for x in range(self.width):
            for y in range(self.height):
                if self.map[x][y] > 0:
                    yield (x, y, self.map[x][y])

    def food(self):
        for x in range(self.width):
            for y in range(self.height):
                if self.map[x][y] == FOOD:
                    yield (x, y)

    def nearby_ants(self, x, y, exclude=0):
        for dx in (-1,0,1):
            for dy in (-1,0,1):
                nx = x + dx
                ny = y + dy
                if self.map[nx][ny] > 0 and self.map[nx][ny] != exclude:
                    yield (nx, ny, self.map[nx][ny])

    def no_ant_map(self):
        new_map = deepcopy(self.map)
        for x in range(self.width):
            for y in range(self.height):
                if new_map[x][y] > 0:
                    new_map[x][y] = 0
        return new_map

    def do_orders_1(self, player, orders):
        if self.order_map == None:
            self.order_map = self.no_ant_map()
        for order in orders:
            x1, y1, x2, y2 = order
            if self.map[x1][y1] != player: # must move your ant
                continue
            if self.map[x2][y2] in (FOOD, WALL, HILL, BODY): # blocking things
                continue
            if (self.order_map[x2][y2] == CONFLICT
                    or self.order_map[x2][y2] > 0):
                self.order_map[x2][y2] = CONFLICT
            else:
                self.order_map[x2][y2] = player
            self.map[x1][y1] = LAND
        for x in range(self.width):
            for y in range(self.height):
                if self.map[x][y] == player:
                    if self.order_map[x][y] > 0:
                        self.order_map[x][y] = CONFLICT
                    else:
                        self.order_map[x][y] = player
                    self.map[x][y] = LAND

    def do_orders_2(self, player, orders):
        if self.order_map == None:
            self.order_map = self.no_ant_map()
        for order in orders:
            x1, y1, x2, y2 = order
            if self.map[x1][y1] != player: # must move your ant
                continue
            if self.map[x2][y2] in (FOOD, WALL, HILL, BODY): # blocking things
                continue
            if self.order_map[x2][y2] == CONFLICT:
                if player in self.conflict[(x2,y2)]:
                    continue # already sent it ant, block second one
                else:
                    self.conflict[(x2,y2)].append(player)
            elif self.order_map[x2][y2] > 0:
                if self.order_map[x2][y2] == player:
                    continue # block self from moving
                else:
                    self.order_map[x2][y2] = CONFLICT
                    self.conflict[(x2,y2)] = [player, self.order_map[x2][y2]]
            else:
                self.order_map[x2][y2] = player
            self.map[x1][y1] = LAND
        for x in range(self.width):
            for y in range(self.height):
                if self.map[x][y] == player:
                    if self.order_map[x][y] > 0 and self.order_map[x][y] != player:
                        self.order_map[x][y] = CONFLICT
                    else:
                        self.order_map[x][y] = player # this may fuse ants, which is undesireable
                    self.map[x][y] = LAND
        
    def resolve_orders(self):
        if self.order_map != None:
            for x in range(self.width):
                for y in range(self.height):
                    if self.order_map[x][y] == CONFLICT:
                        self.order_map[x][y] = LAND
            self.map = self.order_map
            self.order_map = None
            self.conflict = {}

    def do_birth_1(self):
        new_map = deepcopy(self.map)
        for fx, fy in self.food():
            support = [0 for i in range(self.num_players+1)]
            for nx, ny, nowner in self.nearby_ants(fx, fy):
                support[nowner] += 1
            max_support = max(support)
            if max_support > 0:
                if support.count(max_support) == 1:
                    new_map[fx][fy] = support.index(max_support)
        self.map = new_map

    def do_death_1(self):
        new_map = deepcopy(self.map)
        for ax, ay, aowner in self.ants():
            enemy_ants = list(self.nearby_ants(ax, ay, aowner))
            for ex, ey, eowner in enemy_ants:
                if len(list(self.nearby_ants(ex, ey, eowner))) <= len(enemy_ants):
                    new_map[ax][ay] = LAND
                    break
        self.map = new_map

    def do_death_2(self):
        new_map = deepcopy(self.map)
        for ax, ay, aowner in self.ants():
            support = [0 for i in range(self.num_players+1)]
            for nx, ny, nowner in self.nearby_ants(ax, ay):
                support[nowner] += 1
            if sum(support) - support[aowner] >= support[aowner]:
                new_map[ax][ay] = LAND
        self.map = new_map

    def do_death_3(self):
        new_map = deepcopy(self.map)
        for ax, ay, aowner in self.ants():
            for ex, ey, eowner in self.nearby_ants(ax, ay, aowner):
                new_map[ax][ay] = LAND
                break

    def do_food_1(self, amount=1):
        for f in range(amount):
            for t in range(10):
                x = randrange(self.width)
                y = randrange(self.height)
                if self.map[x][y] == LAND:
                    self.map[x][y] = FOOD
                    break

    def do_score_1(self):
        return [0 for i in range(self.num_players+1)]

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

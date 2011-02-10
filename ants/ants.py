#!/usr/bin/env python
from copy import deepcopy
from random import randrange
from math import sqrt
import Image
import os

ANTS = 0
LAND = -1
FOOD = -2
WALL = -3
CONFLICT = -4
UNSEEN = -5

WALL_COLOR = (128, 128, 128)
LAND_COLOR = (139, 69, 19)
FOOD_COLOR = (255, 255, 255)
CONFLICT_COLOR = (255, 128, 128)
UNSEEN_COLOR = (0, 0, 0)

MAP_READ = '?!%*.abcdefghijklmnopqrstuvwxyz'
MAP_RENDER = 'abcdefghijklmnopqrstuvwxyz?!%*.'

PLAYER_COLOR = [(204, 0, 0),    # red
                (255, 255, 0),  # yellow
                (51, 153, 0),   # green
                (51, 51, 153),  # blue
                (154, 51, 154), # purple
                (50, 154, 154), # teal
                (254, 154, 2),  # orange
                (154, 206, 51)] # sage

DIRECTION = {'n': (-1, 0),
             's': (1, 0),
             'e': (0, 1),
             'w': (0, -1)}

# precalculated sqrt & radius coordinates for distance calcs
SQRT = [int(sqrt(r)) for r in range(101)]
RADIUS = []
for r in range(101):
    RADIUS.append([])
    mx = SQRT[r]
    for d_row in range(-mx, mx+1):
        for d_col in range(-mx, mx+1):
            if d_row**2 + d_col**2 == r:
                RADIUS[r].append((d_row, d_col))

class Ants:
    def __init__(self, filename):
        # methods to switch out possible game mechanics
        self.do_food = self.do_food_1
        self.do_score = self.do_score_1

        self.map = None     # the map
        self.ant_list = {}  # indexed list of ant locations for speed
        self.food_list = {} # indexed list of food locations for speed


        #self.center = [] # used to scroll the map so that a player's
                         #   starting ant is in the center

        # load map and get number of players from map
        #   will fill in center data
        if os.path.splitext(filename)[1].lower() == '.png':
            self.load_image(filename)
        elif os.path.splitext(filename)[1].lower() == '.txt':
            self.load_text(filename)

        # used to track dead players, ants may still exist, but order are not processed
        self.killed = [False for i in range(self.num_players)]

        # used to track wall and land already reveal to player
        self.revealed = [[[False for col in range(self.width)]
                          for row in range(self.height)]
                         for p in range(self.num_players)]

        # used to give a different ordering of players to each player
        #   to help hide total number of players
        self.switch = [[0 if j == i else None
                             for j in range(self.num_players)] + range(-5,0)
                         for i in range(self.num_players)]

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
                #self.center = [None for i in range(self.num_players)]
                self.player_colors = PLAYER_COLOR[:self.num_players]
                self.image_colors = self.player_colors + [UNSEEN_COLOR,
                             CONFLICT_COLOR, WALL_COLOR, FOOD_COLOR, LAND_COLOR]
                self.map = [[LAND for i in range(self.width)] for i in range(self.height)]
            elif line[0] == 'W':
                data = line.split()
                x1 = int(data[1])
                y1 = int(data[2])
                x2 = int(data[3])
                y2 = int(data[4])
                for row in range(x1, x2+1):
                    for y in range(y1, y2+1):
                        if self.map[row][y] != WALL:
                            self.map[row][y] = WALL
                            self.wall_area += 1
                            self.land_area -= 1
            elif line[0] == 'M':
                data = line[2:-1]
                if len(data) != self.width:
                    return False
                for col, c in enumerate(data):
                    value = MAP_READ.index(c) - 5
                    self.map[last_row][col] = value
                    if value >= 0:
                        self.land_area += 1
                        self.wall_area -= 1
                        self.ant_list[(last_row, col)] = value
                        #if self.center[value] == None:
                        #    self.center[value] = (last_row, col)
                    elif value in (LAND, FOOD, CONFLICT):
                        self.land_area += 1
                        self.wall_area -= 1
                last_row += 1
            elif line[0] == 'F':
                data = line.split()
                col = int(data[1])
                row = int(data[2])
                if self.map[row][col] == LAND:
                    self.map[row][col] = FOOD
                    self.food_list[(row,col)] = True
            elif line[0] == 'A':
                data = line.split()
                col = int(data[1])
                row = int(data[2])
                owner = int(data[3])
                if self.map[row][col] == LAND:
                    self.map[row][col] = owner
                    self.ant_list[(row,col)] = owner
                    #if self.center[owner] == None:
                    #    self.center[owner] = (row, col)
        return True

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
        self.image_colors = self.player_colors + [UNSEEN_COLOR,
                             CONFLICT_COLOR, WALL_COLOR, FOOD_COLOR, LAND_COLOR]

    def render(self):
        image = Image.new('RGB', (self.width, self.height), LAND_COLOR)
        img = image.load()
        for row in range(self.height):
            for col in range(self.width):
                if self.map[row][col] != LAND:
                    img[row, col] = self.image_colors[self.map[row][col]]
        return image

    def get_vision(self, player, radius=96):
        # return true for each spot that is visable
        vision = [[False for row in range(self.width)] for col in range(self.height)]
        for row, col in self.player_ants(player):
            for r in range(radius+1):
                for d_row, d_col in RADIUS[r]:
                    v_row = (row + d_row) % self.height
                    v_col = (col + d_col) % self.width
                    vision[v_row][v_col] = True
                    self.revealed[player][v_row][v_col] = True
                    # if player found a new player, setup the switch
                    value = self.map[v_row][v_col]
                    if (value >= ANTS and
                            self.switch[player][value] == None):
                        self.switch[player][value] = (self.num_players -
                            self.switch[player][:self.num_players].count(None))
        return vision

    def get_perspective(self, player, radius=96):
        v = self.get_vision(player, radius)
        #start_row = self.center[player][1] - self.height // 2
        #stop_row = start_row + self.height
        #start_col = self.center[player][0] - self.width // 2
        #stop_col = start_col + self.width
        return [[self.switch[player][self.map[row % self.height][col % self.width]]
                    if v[row % self.height][col % self.width] else UNSEEN
                #    for col in range(start_row, stop_row + 1)]
                #for row in range(start_col, stop_col + 1)]
                    for col in range(self.height)]
                for row in range(self.width)]

    def render_text(self, player=LAND):
        tmp = ''
        if player == LAND:
            m = self.map
        else:
            m = self.get_perspective(player)
        for row in m:
            tmp += ''.join([MAP_RENDER[col] for col in row]) + '\n'
        return tmp

    def ants(self):
        return [(row, col, ownr) for (row, col), ownr in self.ant_list.items()]

    def player_ants(self, player):
        return [(row, col) for (row, col), owner
                in self.ant_list.items() if player == owner]

    def food(self):
        return list(self.food_list.keys())

    # min and max are defined as sum of directions squared, so 9 is dist 3
    # default values 1-2 are the 8 spaces around a square
    # this avoids the sqrt function
    def nearby_ants(self, row, col, exclude=LAND, min_dist=1, max_dist=2):
        mx = SQRT[max_dist]
        for d_row in range(-mx,mx+1):
            for d_col in range(-mx,mx+1):
                d = d_row**2 + d_col**2
                if d >= min_dist and d <= max_dist:
                    n_row = (row + d_row) % self.width
                    n_col = (col + d_col) % self.height
                    owner = self.map[n_row][n_col]
                    if owner >= ANTS and owner != exclude:
                        yield (n_row, n_col, owner)

    def parse_orders(self, player, orders):
        # orders come in as x, y (col, row)
        # everywhere else they are row, col
        new_orders = []
        try:
            for line in orders.split('\n'):
                if line != '' and line[0] != '#':
                    data = line.split()
                    if not data[2] in DIRECTION.keys():
                        return None
                    order = [int(data[0]), int(data[1]), data[2]]
                    #o_col = (int(data[0]) - self.width//2 + self.center[player][0]) % self.width
                    #o_row = (int(data[1]) - self.height//2 + self.center[player][1]) % self.height
                    new_orders.append((int(data[0]), int(data[1]), data[2]))
            return new_orders
        except:
            import traceback
            traceback.print_exc()
            print('error in parsing orders')
            return None

    # process orders 1 player at a time
    def do_orders(self, player, orders):
        # orders have already been parsed, so they are in row, col format
        src = {}
        dest = {}
        # process orders ignoring bad or duplicates
        for order in orders:
            row1, col1, d = order
            row2 = (row1 + DIRECTION[d][0]) % self.height
            col2 = (col1 + DIRECTION[d][1]) % self.width
            if src.has_key((row1,col1)): # order already given
                continue
            if self.map[row1][col1] != player: # must move *your* ant
                continue
            src[(row1,col1)] = True
            if self.map[row2][col2] in (FOOD, WALL): # blocking things
                if dest.has_key((row1,col1)):
                    dest[(row1,col1)].append((row1,col1))
                else:
                    dest[(row1,col1)] = [(row1,col1)]
            else: # good order
                if dest.has_key((row2,col2)):
                    dest[(row2,col2)].append((row1,col1))
                else:
                    dest[(row2,col2)] = [(row1,col1)]
        # check for unordered ants and give them hold orders
        for pos in self.player_ants(player):
            if not src.has_key(pos):
                if dest.has_key(pos):
                    dest[pos].append(pos)
                else:
                    dest[pos] = [pos]
        for row1, col1 in src.keys():
            self.map[row1][col1] = LAND
            try:
                del self.ant_list[(row1,col1)]
            except:
                raise Exception("Delete ant error",
                                "Ant not found at (%s, %s), found %s" %
                                (col1, row1, self.map[row1][col1]))
        # check for conflicts
        for (row2, col2), srcs in dest.items():
            if (len(srcs) > 1 or (self.map[row2][col2] >= ANTS and
                    self.map[row2][col2] != player)
                    or self.map[row2][col2] == CONFLICT):
                self.map[row2][col2] = CONFLICT
                if self.ant_list.has_key((row2,col2)):
                    del self.ant_list[(row2,col2)]
            else:
                self.map[row2][col2] = player
                self.ant_list[(row2,col2)] = player

    def resolve_orders(self):
        for row in range(self.height):
            for col in range(self.width):
                if self.map[row][col] == CONFLICT:
                    self.map[row][col] = LAND

    # must have only 1 force near the food to create a new ant
    #  and food in contention is eliminated
    def do_birth(self):
        new_ants = {}
        for f_row, f_col in self.food():
            ownr = 0
            for n_row, n_col, n_ownr in self.nearby_ants(f_row, f_col, 0, 1, 9):
                if ownr == 0:
                    ownr = n_ownr
                elif ownr != n_ownr:
                    self.map[f_row][f_col] = LAND
                    del self.food_list[(f_row, f_col)]
                    break
            else:
                if ownr != 0:
                    new_ants[(f_row, f_col)] = ownr
        for (row, col), player in new_ants.items():
            self.map[row][col] = player
            self.ant_list[(row, col)] = player
            del self.food_list[(row, col)]

    # 1:1 kill ratio, almost, match closest groups and eliminate iteratively
    def do_death(self):
        self.kills = [0 for i in range(self.num_players)]
        ant_group = []
        def find_enemy(row, col, owner, min_d, max_d):
            for n_row, n_col, n_owner in self.nearby_ants(row, col, owner,
                                                          min_d, max_d):
                if not (n_row, n_col) in ant_group:
                    ant_group.append((n_row, n_col))
                    find_enemy(n_row, n_col, n_owner, min_d, max_d)
        for distance in range(1, 10):
            for a_row, a_col, a_owner in self.ants():
                if self.map[a_row][a_col] != LAND:
                    ant_group = [(a_row, a_col)]
                    find_enemy(a_row, a_col, a_owner, distance, distance)
                    if len(ant_group) > 1:
                        for e_row, e_col in ant_group:
                            self.map[e_row][e_col] = LAND
                            del self.ant_list[(e_row, e_col)]

    def do_food_1(self, amount=1):
        for f in range(amount):
            for t in range(10):
                row = randrange(self.height)
                col = randrange(self.width)
                if self.map[row][col] == LAND:
                    self.map[row][col] = FOOD
                    self.food_list[(row, col)] = True
                    break

    def do_score_1(self):
        return [0 for x in range(self.num_players)]

    def remaining_players(self):
        return sum(self.is_alive(p) for p in range(self.num_players))

    def game_over(self):
        return self.remaining_players() <= 1

    def kill_player(self, player):
        self.killed[player] = True

    # common functions for all games
    def start_game(self):
        self.do_food(self.land_area//100)

    def finish_game(self):
        pass

    def start_turn(self):
        pass

    def finish_turn(self):
        self.do_death()
        self.do_birth()
        self.do_food()

    # used for 'map hack' playback
    def get_state(self):
        return self.render_text()

    # used for turn 0, sending minimal info for bot to load
    def get_player_start(self, player):
        return 'D %s %s\n' % (self.width, self.height)

    # used for sending state to bots for each turn
    def get_player_state(self, player):
        return self.render_text(player)

    # used by engine to determine players still in game
    def is_alive(self, player):
        if self.killed[player]:
            return False
        else:
            for row, col, owner in self.ants():
                if owner == player:
                    return True
                    break
            else:
                return False

    # used to report error that kicked a player from game
    def get_error(self, player):
        return ''

    def do_moves(self, player, moves):
        orders = self.parse_orders(player, moves)
        if orders != None:
            self.do_orders(player, orders)
            return None
        else:
            self.kill_player(player)
            return 'Parse Error'

    def do_all_moves(self, bot_moves):
        return [self.do_moves(b, moves) for b, moves in enumerate(bot_moves)]

    # used for ranking
    def get_scores(self):
        return [0 for i in range(self.num_players)]

    # used for stats
    def get_stats(self):
        ant_count = [0 for i in range(self.num_players)]
        for row, col, owner in self.ants():
            ant_count[owner] += 1
        return {'ant_count': ant_count}

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

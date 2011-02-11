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

MAP_RENDER = 'abcdefghijklmnopqrstuvwxyz?!%*.'

PLAYER_COLOR = [(204, 0, 0),    # red
                (255, 255, 0),  # yellow
                (51, 153, 0),   # green
                (51, 51, 153),  # blue
                (154, 51, 154), # purple
                (50, 154, 154), # teal
                (254, 154, 2),  # orange
                (154, 206, 51)] # sage

MAP_COLOR = PLAYER_COLOR + [UNSEEN_COLOR, CONFLICT_COLOR, WALL_COLOR, FOOD_COLOR, LAND_COLOR]

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

FULL_RADIUS = []
for r in range(101):
    FULL_RADIUS.append([])
    mx = SQRT[r]
    for d_row in range(-mx, mx+1):
        for d_col in range(-mx, mx+1):
            if d_row**2 + d_col**2 <= r:
                FULL_RADIUS[r].append((d_row, d_col))
class Ants:
    def __init__(self, filename):
        # methods to switch out possible game mechanics
        self.do_food = self.do_food_1
        self.do_score = self.do_score_1

        self.width = None   # the map
        self.height = None
        self.map = None
        self.wall_area = 0
        self.land_area = 0

        self.ant_list = {}  # indexed list of ant locations for speed
        self.food_list = [] # indexed list of food locations for speed


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
        # used to track per turn for bot communication
        self.turn_reveal = [[] for i in range(self.num_players)]

        # used to give a different ordering of players to each player
        #   to help hide total number of players
        self.switch = [[0 if j == i else None
                             for j in range(self.num_players)] + range(-5,0)
                         for i in range(self.num_players)]

    def load_text(self, filename):
        players = []
        f = open(filename, 'r')
        self.map = []
        row = 0
        for line in f:
            data = line.strip()
            if data == '':
                continue # ignore blank lines
            if self.width == None:
                self.width = len(data)
            else:
                if len(data) != self.width:
                    return False
            if len(data) != self.width:
                return False
            self.map.append([])
            for col, c in enumerate(data):
                if c in 'abcdefghijklmnopqrstuvwxyz':
                    if not c in players:
                        players.append(c)
                        #if self.center[value] == None:
                        #    self.center[value] = (last_row, col)
                    value = players.index(c)
                    self.map[-1].append(value)
                    self.ant_list[(col, row)] = value
                    self.land_area += 1
                elif c == '*':
                    self.map[-1].append(FOOD)
                    self.food_list.append((col, row))
                    self.land_area += 1
                elif c == '%':
                    self.map[-1].append(WALL)
                    self.wall_area += 1
                elif c == '.':
                    self.map[-1].append(LAND)
                    self.land_area += 1
                else:
                    return False
            row += 1
        self.height = row
        self.num_players = len(players)
        return True

    def load_image(self, filename):
        image = Image.open(filename)
        self.width, self.height = image.size
        self.num_players = 0
        self.land_area = 0
        self.wall_area = 0
        self.player_colors = []
        self.map = [[LAND for i in range(self.width)] for i in range(self.height)]
        for x in range(self.width):
            for y in range(self.height):
                pixel = image.getpixel((x,y))
                if pixel == WALL_COLOR:
                    self.map[y][x] = WALL
                    self.wall_area += 1
                elif pixel == HILL_COLOR:
                    self.map[y][x] = HILL
                    self.hill_list[(x,y)] = True
                    self.wall_area += 1
                else:
                    self.land_area += 1
                    if pixel == LAND_COLOR:
                        self.map[y][x] = LAND
                    elif pixel == FOOD_COLOR:
                        self.map[y][x] = FOOD
                        self.food_list.append((x,y))
                    elif pixel in self.player_colors:
                        self.map[y][x] = self.player_colors.index(pixel)
                        self.ant_list[(x,y)] = self.map[y][x]
                    else:
                        self.num_players += 1
                        self.player_colors.append(pixel)
                        self.map[y][x] = self.num_players
                        self.ant_list[(x,y)] = self.map[y][x]

    def render(self, player=None):
        if player == None:
            m = self.map
        else:
            m = self.get_perspective(player)
        image = Image.new('RGB', (self.width, self.height), LAND_COLOR)
        img = image.load()
        for row in range(self.height):
            for col in range(self.width):
                if m[row][col] != LAND:
                    img[col, row] = MAP_COLOR[m[row][col]]
        return image

    def get_vision(self, player, radius=96):
        # return true for each spot that is visable
        vision = [[False for col in range(self.width)] for row in range(self.height)]
        for col, row in self.player_ants(player):
            for d_col, d_row in FULL_RADIUS[radius]:
                v_row = (row + d_row) % self.height
                v_col = (col + d_col) % self.width
                vision[v_row][v_col] = True
                if not self.revealed[player][v_row][v_col]:
                    self.turn_reveal[player].append((v_col, v_row))
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
                    for col in range(self.width)]
                for row in range(self.height)]

    # communication to bot is in x, y coords
    def render_changes(self, player):
        v = self.get_vision(player)
        # send new wall/land
        tmp = ''
        CHANGES = ['D','W','F','L']
        for col, row in self.turn_reveal[player]:
            value = self.map[row][col]
            if value in (LAND, WALL):
                tmp += '%s %s %s\n' % (CHANGES[self.map[row][col]], col, row)
        # send visible ants
        for (col, row), owner in self.ant_list.items():
            if v[row][col]:
                tmp += 'A %s %s %s\n' % (col, row, self.switch[player][owner])
        # send visible food
        for col, row in self.food_list:
            if v[row][col]:
                tmp += 'F %s %s\n' % (col, row)
        # send visible conflict
        for row in range(self.height):
            for col in range(self.width):
                if v[row][col] and self.map[row][col] == CONFLICT:
                    tmp += 'D %s %s\n' % (col, row)
        return tmp

    def render_text(self, player=None):
        tmp = ''
        if player == None:
            m = self.map
        else:
            m = self.get_perspective(player)
        for row in m:
            tmp += ''.join([MAP_RENDER[col] for col in row]) + '\n'
        return tmp

    def player_ants(self, player):
        return [(col, row) for (col, row), owner
                in self.ant_list.items() if player == owner]

    # min and max are defined as sum of directions squared, so 9 is dist 3
    # default values 1-2 are the 8 spaces around a square
    # this avoids the sqrt function
    def nearby_ants(self, col, row, exclude=None, min_dist=1, max_dist=2):
        mx = SQRT[max_dist]
        for d_row in range(-mx,mx+1):
            for d_col in range(-mx,mx+1):
                d = d_row**2 + d_col**2
                if d >= min_dist and d <= max_dist:
                    n_row = (row + d_row) % self.height
                    n_col = (col + d_col) % self.width
                    owner = self.map[n_row][n_col]
                    if owner >= ANTS and owner != exclude:
                        yield ((n_col, n_row), owner)

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
                    #order = [int(data[0]), int(data[1]), data[2]]
                    #o_col = (int(data[0]) - self.width//2 + self.center[player][0]) % self.width
                    #o_row = (int(data[1]) - self.height//2 + self.center[player][1]) % self.height
                    new_orders.append((int(data[1]), int(data[0]), data[2]))
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
            if src.has_key((col1,row1)): # order already given
                continue
            if self.map[row1][col1] != player: # must move *your* ant
                continue
            src[(col1,row1)] = True
            if self.map[row2][col2] in (FOOD, WALL): # blocking things
                if dest.has_key((col1,row1)):
                    dest[(col1,row1)].append((col1,row1))
                else:
                    dest[(col1,row1)] = [(col1,row1)]
            else: # good order
                if dest.has_key((col2,row2)):
                    dest[(col2,row2)].append((col1,row1))
                else:
                    dest[(col2,row2)] = [(col1,row1)]
        # check for unordered ants and give them hold orders
        for pos in self.player_ants(player):
            if not src.has_key(pos):
                if dest.has_key(pos):
                    dest[pos].append(pos)
                else:
                    dest[pos] = [pos]
        for col1, row1 in src.keys():
            self.map[row1][col1] = LAND
            try:
                del self.ant_list[(col1,row1)]
            except:
                raise Exception("Delete ant error",
                                "Ant not found at (%s, %s), found %s" %
                                (col1, row1, self.map[row1][col1]))
        # check for conflicts
        for (col2, row2), srcs in dest.items():
            if (len(srcs) > 1 or (self.map[row2][col2] >= ANTS and
                    self.map[row2][col2] != player)
                    or self.map[row2][col2] == CONFLICT):
                self.map[row2][col2] = CONFLICT
                if self.ant_list.has_key((col2,row2)):
                    del self.ant_list[(col2,row2)]
            else:
                self.map[row2][col2] = player
                self.ant_list[(col2,row2)] = player

    def resolve_orders(self):
        for row in range(self.height):
            for col in range(self.width):
                if self.map[row][col] == CONFLICT:
                    self.map[row][col] = LAND

    # must have only 1 force near the food to create a new ant
    #  and food in contention is eliminated
    def do_birth(self):
        new_ants = {}
        for f_col, f_row in self.food_list[:]:
            owner = None
            for (n_col, n_row), n_owner in self.nearby_ants(f_col, f_row, None, 1, 9):
                if owner == None:
                    owner = n_owner
                elif owner != n_owner:
                    self.map[f_row][f_col] = LAND
                    self.food_list.remove((f_col, f_row))
                    break
            else:
                if owner != None:
                    new_ants[(f_col, f_row)] = owner
                    self.food_list.remove((f_col, f_row))
                    self.map[f_row][f_col] = owner
                    self.ant_list[(f_col, f_row)] = owner

    # 1:1 kill ratio, almost, match closest groups and eliminate iteratively
    def do_death(self):
        self.kills = [0 for i in range(self.num_players)]
        ant_group = []
        def find_enemy(col, row, owner, min_d, max_d):
            for (n_col, n_row), n_owner in self.nearby_ants(col, row, owner,
                                                          min_d, max_d):
                if not (n_col, n_row) in ant_group:
                    ant_group.append((n_col, n_row))
                    find_enemy(n_col, n_row, n_owner, min_d, max_d)
        for distance in range(1, 10):
            for (a_col, a_row), a_owner in self.ant_list.items():
                if self.map[a_row][a_col] != LAND:
                    ant_group = [(a_col, a_row)]
                    find_enemy(a_col, a_row, a_owner, distance, distance)
                    if len(ant_group) > 1:
                        for e_row, e_col in ant_group:
                            self.map[e_row][e_col] = LAND
                            del self.ant_list[(e_col, e_row)]

    def do_food_1(self, amount=1):
        for f in range(amount):
            for t in range(10):
                row = randrange(self.height)
                col = randrange(self.width)
                if self.map[row][col] == LAND:
                    self.map[row][col] = FOOD
                    self.food_list.append((col, row))
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
        self.turn_reveal= [[] for i in range(self.num_players)]
        self.do_death()
        self.do_birth()
        self.do_food()

    # used for 'map hack' playback
    def get_state(self):
        return self.render_text()

    # used for turn 0, sending minimal info for bot to load
    def get_player_start(self, player):
        return 'P width %s\nP height %s\n' % (self.width, self.height)

    # used for sending state to bots for each turn
    def get_player_state(self, player):
        return self.render_changes(player)

    # used by engine to determine players still in game
    def is_alive(self, player):
        if self.killed[player]:
            return False
        else:
            for (col, row), owner in self.ant_list.items():
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

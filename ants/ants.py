#!/usr/bin/env python
from random import randrange, choice
from math import sqrt
import os
from collections import deque, defaultdict
from fractions import Fraction
import operator

ANTS = 0
LAND = -1
FOOD = -2
WATER = -3
CONFLICT = -4
UNSEEN = -5

MAP_RENDER = 'abcdefghijklmnopqrstuvwxyz?!%*.'

AIM = {'n': (-1, 0),
       'e': (0, 1),
       's': (1, 0),
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
    def __init__(self, options=None):
        # setup options
        # attack method
        filename = options['map']
        self.turns = options['turns']
        self.loadtime = options['loadtime']
        self.turntime = options['turntime']
        self.viewradius = 96
        self.attackradius = 5
        self.spawnradius = 2
        self.do_attack = self.do_attack_closest
        if 'attack' in options:
            if options['attack'] == 'occupied':
                self.do_attack = self.do_attack_occupied
            elif options['attack'] == 'closest':
                self.do_attack = self.do_attack_closest
        # bot communication method
        self.render = self.render_changes
        if 'communication' in options:
            if options['communication'] == 'changes':
                self.render = self.render_changes
            elif options['communication'] == 'map':
                self.render = self.render_map
        self.do_food = self.do_food_sections

        self.width = None   # the map
        self.height = None
        self.map = None
        self.water_area = 0
        self.land_area = 0

        self.ant_list = {}  # indexed list of ant locations for speed
        self.food_list = [] # indexed list of food locations for speed
        self.conflict_list = {}


        #self.center = [] # used to scroll the map so that a player's
        #                 #   starting ant is in the center

        # load map and get number of players from map
        #   will fill in center data
        if os.path.splitext(filename)[1].lower() == '.png':
            self.load_image(filename)
        elif os.path.splitext(filename)[1].lower() == '.txt':
            self.load_text(filename)
        else:
            raise Exception("map", "Invalid map file extension: %s" % os.path.splitext(filename)[1].lower())

        # used to remember where the ants started
        self.initial_ant_list = dict(self.ant_list)
        self.initial_access_map = self.access_map()

        # used to track dead players, ants may still exist, but order are not processed
        self.killed = [False for i in range(self.num_players)]

        # used to track water and land already reveal to player
        # ants and food will reset spots so a second land entry will be sent
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

        # used to track scores
        self.score = [Fraction(0,1) for i in range(self.num_players)]
        self.turn = 0

    def load_text(self, filename):
        players = []
        f = open(filename, 'r')
        self.map = []
        row = 0
        for line in f:
            line = line.strip().lower()
            if line == '':
                continue # ignore blank lines
            data = line.split(' ')
            if data[0] == 'cols':
                self.width = int(data[1])
            elif data[0] == 'rows':  
                self.height = int(data[1])
            elif data[0] == 'm':
                if len(data[1]) != self.width:
                    raise Exception('map',
                                    'Incorrect number of cols in row %s. Got %s, expected %s.' % (
                                    row, len(data[1]), self.width))
                self.map.append([])
                for col, c in enumerate(data[1]):
                    if c in 'abcdefghijklmnopqrstuvwxyz':
                        c = c
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
                        self.map[-1].append(WATER)
                        self.water_area += 1
                    elif c == '.':
                        self.map[-1].append(LAND)
                        self.land_area += 1
                    else:
                        raise Exception("map", "Invalid character in map: %s" % c)
                row += 1
        if self.height != row:
                raise Exception("map", "Incorrect number of rows.  Expected %s, got %s" % (self.height, row))                
        self.num_players = len(players)
        return True

    def distance(self, x1, y1, x2, y2):
        d_x = min(abs(x1 - x2), self.width - abs(x1 - x2))
        d_y = min(abs(y1 - y2), self.height - abs(y1 - y2))
        return d_x + d_y

    def get_vision(self, player, radius=96):
        vision = [[False for col in range(self.width)] for row in range(self.height)]
        squaresToCheck = deque()
        for col, row in self.player_ants(player):
            squaresToCheck.append(((col,row),(col,row)))
        while len(squaresToCheck) > 0:
            (a_col, a_row), (v_col, v_row) = squaresToCheck.popleft()
            for d_col, d_row in ((0,-1),(0,1),(-1,0),(1,0)):
                n_col = (v_col + d_col) % self.width
                n_row = (v_row + d_row) % self.height
                d_row = abs(a_row - n_row)
                d_row = min(d_row, self.height - d_row)
                d_col = abs(a_col - n_col)
                d_col = min(d_col, self.width - d_col)
                if not vision[n_row][n_col] and (d_row**2 + d_col**2) <= radius:
                    vision[n_row][n_col] = True
                    if not self.revealed[player][n_row][n_col]:
                        self.turn_reveal[player].append((n_col, n_row))
                        self.revealed[player][n_row][n_col] = True
                    value = self.map[n_row][n_col]
                    if (value >= ANTS and self.switch[player][value] == None):
                        self.switch[player][value] = (self.num_players -
                            self.switch[player][:self.num_players].count(None))
                    squaresToCheck.append(((a_col,a_row),(n_col,n_row)))
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
    def render_changes(self, player=None):
        if player != None:
            v = self.get_vision(player)
        # send new water
        tmp = ''
        if player != None:
            for col, row in self.turn_reveal[player]:
                if self.map[row][col] ==  WATER:
                    tmp += 'w %s %s\n' % (row, col)
        # send visible ants
        for (col, row), owner in self.ant_list.items():
            if player == None:
                tmp += 'a %s %s %s\n' % (row, col, owner)
            elif v[row][col]:
                tmp += 'a %s %s %s\n' % (row, col, self.switch[player][owner])
                self.revealed[player][row][col] = False
        # send visible food
        for col, row in self.food_list:
            if player == None or v[row][col]:
                tmp += 'f %s %s\n' % (row, col)
                if player != None:
                    self.revealed[player][row][col] = False
        # send visible conflict
        for row in range(self.height):
            for col in range(self.width):
                if ((player == None or v[row][col])) and self.map[row][col] == CONFLICT:
                    for owner in self.conflict_list[(row, col)]:
                        tmp += 'd %s %s %s\n' % (row, col, owner)
                        if player != None:
                            self.revealed[player][row][col] = False
        return tmp

    def render_map(self, player=None):
        tmp = ''
        if player == None:
            m = self.map
        else:
            m = self.get_perspective(player)
        for row in m:
            tmp += 'm ' + ''.join([MAP_RENDER[col] for col in row]) + '\n'
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
        valid = []
        invalid = []
        try:
            for line in orders:
                line = line.strip().lower()
                if line != '' and line[0] != '#':
                    data = line.split()
                    if data[0] == 'o':
                        if not data[3] in AIM.keys():
                            invalid.append(line + ' # invalid direction')
                        #order = [int(data[1]), int(data[2]), data[3]]
                        #o_col = (int(data[1]) - self.width//2 + self.center[player][0]) % self.width
                        #o_row = (int(data[2]) - self.height//2 + self.center[player][1]) % self.height
                        try:
                            new_orders.append((int(data[2]), int(data[1]), data[3]))
                            valid.append(line)
                        except:
                            invalid.append(line + ' # invalid row, col')
            return new_orders, valid, invalid
        except:
            import traceback
            traceback.print_exc()
            print('error in parsing orders')
            return ['fatal error in parsing orders']

    # process orders 1 player at a time
    def do_orders(self, player, orders):
        # orders have already been parsed, so they are in row, col format
        src = {}
        dest = {}
        # process orders ignoring bad or duplicates
        for order in orders:
            col1, row1, d = order
            row2 = (row1 + AIM[d][0]) % self.height
            col2 = (col1 + AIM[d][1]) % self.width
            if src.has_key((col1,row1)): # order already given
                continue
            if self.map[row1][col1] != player: # must move *your* ant
                continue
            src[(col1,row1)] = True
            if self.map[row2][col2] in (FOOD, WATER): # blocking things
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
                if (row2, col2) in self.conflict_list:
                    self.conflict_list[(row2, col2)].append(player)
                else:
                    self.conflict_list[(row2, col2)] = [player]                    
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
        self.conflict_list.clear()

    # must have only 1 force near the food to create a new ant
    #  and food in contention is eliminated
    def do_spawn(self):
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

    # ants kill enemies of less or equally occupied
    # TODO: update function to mark conflict for dead ant info
    # TODO: write function correctly, don't kill any ant until end
    def do_attack_occupied(self):
        score = [Fraction(0, 1) for i in range(self.num_players)]
        for (a_col, a_row), a_owner in self.ant_list.items():
            killers = []
            enemies = self.nearby_ants(a_col, a_row, a_owner, 1, 2)
            occupied = len(enemies)
            for (e_col, e_row), e_owner in enemies:
                e_occupied = len(self.nearby_ants(e_col, e_row, e_owner, 1, 2))
                if e_occupied <= occupied:
                    # kill ant
                    killers.append(e_owner)
            if len(killers) > 0:
                self.map[a_col][a_row] = LAND
                del self.ant_list[(a_col, a_row)]
                score_share = len(killers)
                for e_owner in killers:
                    score[e_owner] += Fraction(1, score_share)
        self.score = map(operator.add, self.score, score)

    # 1:1 kill ratio, almost, match closest groups and eliminate iteratively
    def do_attack_closest(self):
        score = [Fraction(0, 1) for i in range(self.num_players)]
        ant_group = []
        def find_enemy(col, row, owner, min_d, max_d):
            for (n_col, n_row), n_owner in self.nearby_ants(col, row, owner,
                                                          min_d, max_d):
                if not (n_col, n_row) in ant_group:
                    ant_group[(n_col, n_row)] = n_owner
                    find_enemy(n_col, n_row, n_owner, min_d, max_d)
        for distance in range(1, 10):
            for (a_col, a_row), a_owner in self.ant_list.items():
                if self.map[a_row][a_col] != LAND:
                    ant_group = {(a_col, a_row): a_owner}
                    find_enemy(a_col, a_row, a_owner, distance, distance)
                    if len(ant_group) > 1:
                        score_share = len(ant_group)
                        for (e_col, e_row), e_owner in ant_group.items():
                            score[e_owner] += Fraction(1, score_share)
                            self.map[e_row][e_col] = CONFLICT
                            if (e_row, e_col) in self.conflict_list:
                                self.conflict_list[(e_row, e_col)].append(e_owner)
                            else:
                                self.conflict_list[(e_row, e_col)] = [e_owner]                    
                            try:
                                del self.ant_list[(e_col, e_row)]
                            except:
                                raise Exception("Delete ant error",
                                                "Ant not found at (%s, %s), kill_group %s" %
                                                (e_col, e_row, ant_group))
        self.score = map(operator.add, self.score, score)

    def do_food_random(self, amount=1):
        """
            Place food randomly on the map
        """
        for f in range(amount):
            for t in range(10):
                row = randrange(self.height)
                col = randrange(self.width)
                if self.map[row][col] == LAND:
                    self.map[row][col] = FOOD
                    self.food_list.append((col, row))
                    break

    def offset_location(self, coord, direction):
        dr, dc = AIM[direction]
        return ((coord[0]+dr)%self.height, (coord[1]+dc)%self.width)

    def access_map(self):
        """
            Determine the list of locations that each player is closest to
        """
        distances = {}
        players = defaultdict(set)
        cell_queue = deque()

        # determine the starting cells and valid squares 
        # (where food can be placed)
        for i, row in enumerate(self.map):
            for j, cell in enumerate(row):
                coord = (i,j)
                if cell >= 0:
                    distances[coord] = 0
                    players[coord].add(cell)
                    cell_queue.append(coord)
                elif cell != WATER:
                    distances[coord] = None

        # use bfs to determine who can reach each cell first
        while cell_queue:
            c_loc = cell_queue.popleft()

            for d in AIM:
                n_loc = self.offset_location(c_loc, d)
                if n_loc not in distances: continue # wall

                if distances[n_loc] is None:
                    # first visit to this cell
                    distances[n_loc] = distances[c_loc] + 1
                    players[n_loc].update(players[c_loc])
                    cell_queue.append(n_loc)
                elif distances[n_loc] == distances[c_loc] + 1:
                    # we've seen this cell before, but the distance is
                    # the same - therefore combine the players that can
                    # reach this cell
                    players[n_loc].update(players[c_loc])

        # summarise the final results of the cells that are closest
        # to a single unique player
        access_map = defaultdict(list)
        for coord, player_set in players.items():
            if len(player_set) != 1: continue
            access_map[player_set.pop()].append(coord)

        return access_map

    def do_food_sections(self, amount=1):
        """
            Split the map into sections that each ant can access 
            first at the start of the game. Place food evenly into each space.
        """
        for f in range(amount//self.num_players):
            for p in range(self.num_players):
                squares = self.initial_access_map[p]
                for t in range(10):
                    row, col = choice(squares)
                    if self.map[row][col] == LAND:
                        self.map[row][col] = FOOD
                        self.food_list.append((col, row))
                        break

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
        self.turn += 1
        self.resolve_orders()

    def finish_turn(self):
        self.turn_reveal= [[] for i in range(self.num_players)]
        self.do_attack()
        self.do_spawn()
        self.do_food()

    # used for 'map hack' playback
    def get_state(self):
        return self.render_changes()

    # used for turn 0, sending minimal info for bot to load
    def get_player_start(self, player=None):
        
        tmp = ('turn 0\nloadtime %s\nturntime %s\nrows %s\ncols %s\nturns %s\n' +
                'viewradius2 %s\nattackradius2 %s\nspawnradius2 %s\n') % (
                self.loadtime, self.turntime, self.height, self.width,
                self.turns, self.viewradius, self.attackradius,
                self.spawnradius)
        if player == None:
            tmp += self.render_map()
        return tmp

    # used for sending state to bots for each turn
    def get_player_state(self, player):
        return self.render(player)

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
        orders, valid, invalid = self.parse_orders(player, moves)
        if len(invalid) == 0:
            self.do_orders(player, orders)
        else:
            self.kill_player(player)
        return valid, invalid

    def do_all_moves(self, bot_moves):
        return [self.do_moves(b, moves) for b, moves in enumerate(bot_moves)]

    # used for ranking
    def get_scores(self):
        return [int(score) for score in self.score]

    # used for stats
    def get_stats(self):
        ant_count = [0 for i in range(self.num_players)]
        for row, col, owner in self.ants():
            ant_count[owner] += 1
        return {'ant_count': ant_count}

if __name__ == '__main__':

    def save_image(map, filename):
        img = map.render_image()
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
    map = Ants(options.filename)
    save_image(map, options.filename[:-4])

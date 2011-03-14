#!/usr/bin/env python
from random import randrange, choice
from math import sqrt
import os
from collections import deque, defaultdict
from fractions import Fraction
import operator
import string

ANTS = 0
LAND = -1
FOOD = -2
WATER = -3
CONFLICT = -4
UNSEEN = -5

MAP_RENDER = string.ascii_lowercase + '?!%*.'

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
        map_text = options['map']
        self.turns = int(options['turns'])
        self.loadtime = int(options['loadtime'])
        self.turntime = int(options['turntime'])
        self.viewradius = int(options["viewradius2"])
        self.attackradius = int(options["attackradius2"])
        self.spawnradius = int(options["spawnradius2"])
        if "seed" in options:
            self.seed = options["seed"]
        else:
            self.seed = None
        #print("Starting game with view=%s, attack=%s and spawn=%s" % (self.viewradius, self.attackradius, self.spawnradius))

        self.do_attack = self.do_attack_closest
        if 'attack' in options:
            if options['attack'] == 'occupied':
                self.do_attack = self.do_attack_occupied
            elif options['attack'] == 'closest':
                self.do_attack = self.do_attack_closest
        self.do_food = self.do_food_sections
        if 'food' in options:
            if options['food'] == 'none':
                self.do_food = self.do_food_none
            elif options['food'] == 'sections':
                self.do_food = self.do_food_sections
        self.render = self.render_changes

        self.width = None   # the map
        self.height = None
        self.map = None
        self.water_area = 0
        self.land_area = 0

        self.current_ants = {} # ants that are currently alive
        self.killed_ants = []  # ants which were killed this turn
        self.all_ants = []     # all ants that have been created

        self.all_food = []     # all food created
        self.current_food = {} # food currently in game

        self.turn = 0

        #self.center = [] # used to scroll the map so that a player's
        #                 #   starting ant is in the center

        # load map and get number of players from map
        #   will fill in center data
        self.load_text(map_text)

        # used to remember where the ants started
        self.initial_ant_list = dict((ant.loc, ant.owner) for ant in self.current_ants.values())
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
        self.score = [Fraction(0,1)]*self.num_players
        self.score_history = [[s] for s in self.score]

    def load_text(self, map_text):
        players = []
        self.map = []
        row = 0
        for line in map_text.split('\n'):
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
                    if c in string.ascii_lowercase:
                        if not c in players:
                            players.append(c)
                            #if self.center[value] == None:
                            #    self.center[value] = (last_row, col)
                        value = players.index(c)
                        self.map[-1].append(value)
                        self.add_ant((row,col), value)
                        self.land_area += 1
                    elif c == '*':
                        self.map[-1].append(FOOD)
                        self.add_food((row, col))
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

    def get_vision(self, player):
        vision = [[False for col in range(self.width)] for row in range(self.height)]
        squaresToCheck = deque()
        for ant in self.player_ants(player):
            squaresToCheck.append((ant.loc, ant.loc))
        while len(squaresToCheck) > 0:
            (a_row, a_col), (v_row, v_col) = squaresToCheck.popleft()
            for d_row, d_col in ((0,-1),(0,1),(-1,0),(1,0)):
                n_col = (v_col + d_col) % self.width
                n_row = (v_row + d_row) % self.height
                d_row = abs(a_row - n_row)
                d_row = min(d_row, self.height - d_row)
                d_col = abs(a_col - n_col)
                d_col = min(d_col, self.width - d_col)
                if not vision[n_row][n_col] and (d_row**2 + d_col**2) <= self.viewradius:
                    vision[n_row][n_col] = True
                    if not self.revealed[player][n_row][n_col]:
                        self.turn_reveal[player].append((n_row, n_col))
                        self.revealed[player][n_row][n_col] = True
                    value = self.map[n_row][n_col]
                    if (value >= ANTS and self.switch[player][value] == None):
                        self.switch[player][value] = (self.num_players -
                            self.switch[player][:self.num_players].count(None))
                    squaresToCheck.append(((a_row,a_col),(n_row,n_col)))
        return vision

    def get_perspective(self, player):
        v = self.get_vision(player, self.viewradius)
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
            for row, col in self.turn_reveal[player]:
                if self.map[row][col] ==  WATER:
                    tmp += 'w %s %s\n' % (row, col)
        # send visible ants
        for ant in self.current_ants.values():
            row, col = ant.loc
            if player == None:
                tmp += 'a %s %s %s\n' % (row, col, ant.owner)
            elif v[row][col]:
                tmp += 'a %s %s %s\n' % (row, col, self.switch[player][ant.owner])
                self.revealed[player][row][col] = False
        # send visible food
        for row, col in self.current_food:
            if player == None or v[row][col]:
                tmp += 'f %s %s\n' % (row, col)
                if player != None:
                    self.revealed[player][row][col] = False
        # send visible dead ants 
        for ant in self.killed_ants:
            row, col = ant.loc
            if player == None or v[row][col]:
                tmp += 'd %s %s %s\n' % (row, col, ant.owner)
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

    # min and max are defined as sum of directions squared, so 9 is dist 3
    # default values 1-2 are the 8 spaces around a square
    # this avoids the sqrt function
    def nearby_ants(self, row, col, exclude=None, min_dist=1, max_dist=2):
        mx = SQRT[max_dist]
        for d_row in range(-mx,mx+1):
            for d_col in range(-mx,mx+1):
                d = d_row**2 + d_col**2
                if d >= min_dist and d <= max_dist:
                    n_row = (row + d_row) % self.height
                    n_col = (col + d_col) % self.width
                    owner = self.map[n_row][n_col]
                    if owner >= ANTS and owner != exclude:
                        yield ((n_row, n_col), owner)

    def parse_orders(self, player, orders):
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
                            new_orders.append((int(data[1]), int(data[2]), data[3]))
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
        # process orders ignoring bad or duplicates
        for order in orders:
            row2, col2 = self.destination(*order)
            row1, col1, d = order

            ant = self.current_ants[(row1, col1)]
            if ant.owner != player: # must move your *own* ant
                continue
            if ant.moved:           # ignore duplicate orders
                continue

            if self.map[row2][col2] not in (FOOD, WATER): # good orders
                ant.move((row2, col2), d)

    def resolve_orders(self):
        # hold any ants that haven't moved and determine new locations
        next_loc = defaultdict(list)
        for ant in self.current_ants.values():
            if not ant.moved:
                ant.move(ant.loc)
            next_loc[ant.loc].append(ant)

        # set old ant locations to land
        for ant in self.current_ants.values():
            row, col = ant.prev_loc
            self.map[row][col] = LAND

        # if ant is sole occupant of a new square then it survives
        self.current_ants = {}
        for loc, ants in next_loc.items():
            if len(ants) == 1:
                self.current_ants[loc] = ants[0]
            else:
                self.killed_ants.extend(ants)

        # set new ant locations
        for ant in self.current_ants.values():
            row, col = ant.loc
            self.map[row][col] = ant.owner

    # must have only 1 force near the food to create a new ant
    #  and food in contention is eliminated
    def do_spawn(self):
        # Determine new ant locations
        new_ant_locations = {}
        for f_loc in self.current_food.keys():
            f_row, f_col = f_loc
            owner = None
            for (n_row, n_col), n_owner in self.nearby_ants(f_row, f_col, None, 1, self.spawnradius):
                if owner == None:
                    owner = n_owner
                elif owner != n_owner:
                    self.remove_food(f_loc)
                    break
            else:
                if owner != None:
                    self.remove_food(f_loc)
                    new_ant_locations[f_loc] = owner

        # Create new ants
        for loc, owner in new_ant_locations.items():
            self.add_ant(loc, owner)

    def add_food(self, loc):
        if loc in self.current_food:
            raise Exception("Add food error",
                            "Food already found at %s" %(loc,))
        self.map[loc[0]][loc[1]] = FOOD
        food = Food(loc, self.turn)
        self.current_food[loc] = food
        self.all_food.append(food)

    def remove_food(self, loc):
        try:
            self.map[loc[0]][loc[1]] = LAND
            self.current_food[loc].end_turn = self.turn
            del self.current_food[loc]
        except KeyError:
            raise Exception("Remove food error",
                            "Food not found at %s" %(loc,))

    def add_ant(self, loc, owner):
        if loc in self.current_ants:
            raise Exception("Add ant error",
                            "Ant already found at %s" %(loc,))
        ant = Ant(loc, owner, self.turn)
        row, col = loc
        self.map[row][col] = owner
        self.all_ants.append(ant)
        self.current_ants[loc] = ant

    def kill_ant(self, loc):
        try:
            self.map[loc[0]][loc[1]] = LAND
            ant = self.current_ants[loc]
            self.killed_ants.append(ant)
            ant.killed = True
            del self.current_ants[loc]
        except KeyError:
            raise Exception("Kill ant error",
                            "Ant not found at %s" %(loc,))

    def player_ants(self, player):
        return [ant for ant in self.current_ants.values() if player == ant.owner]

    # ants kill enemies of less or equally occupied
    # TODO: update function to mark conflict for dead ant info
    # TODO: write function correctly, don't kill any ant until end
    def do_attack_occupied(self):
        score = [Fraction(0, 1) for i in range(self.num_players)]
        ants_to_kill = []
        for ant in self.current_ants.values():
            a_row, a_col = ant.loc
            a_owner = ant.owner
            killers = []
            enemies = list(self.nearby_ants(a_row, a_col, a_owner, 1, self.attackradius))
            occupied = len(enemies)
            for (e_row, e_col), e_owner in enemies:
                e_occupied = len(list(self.nearby_ants(e_row, e_col, e_owner, 1, 2)))
                if e_occupied <= occupied:
                    # kill ant
                    killers.append(e_owner)
            if len(killers) > 0:
                ants_to_kill.append(ant)
                score_share = len(killers)
                for e_owner in killers:
                    score[e_owner] += Fraction(1, score_share)
        for ant in ants_to_kill:
            if not ant.killed:
                self.kill_ant(ant.loc)
        self.score = map(operator.add, self.score, score)

    # 1:1 kill ratio, almost, match closest groups and eliminate iteratively
    def do_attack_closest(self):
        score = [Fraction(0, 1) for i in range(self.num_players)]
        ant_group = []
        def find_enemy(row, col, owner, min_d, max_d):
            for (n_row, n_col), n_owner in self.nearby_ants(row, col, owner,
                                                          min_d, max_d):
                if not (n_row, n_col) in ant_group:
                    ant_group[(n_row, n_col)] = n_owner
                    find_enemy(n_row, n_col, n_owner, min_d, max_d)
        for distance in range(1, 10):
            ants_to_kill = []
            for ant in self.current_ants.values():
                a_row, a_col = ant.loc
                a_owner = ant.owner
                ant_group = {(a_row, a_col): a_owner}
                find_enemy(a_row, a_col, a_owner, distance, distance)
                if len(ant_group) > 1:
                    score_share = len(ant_group)
                    for (e_row, e_col), e_owner in ant_group.items():
                        score[e_owner] += Fraction(1, score_share)
                        ants_to_kill.append(ant)
            for ant in ants_to_kill:
                if not ant.killed:
                    self.kill_ant(ant.loc)

        self.score = map(operator.add, self.score, score)

    def destination(self, row, col, direction):
        d_row, d_col = AIM[direction]
        return ((row + d_row) % self.height, (col + d_col) % self.width)

    def access_map(self):
        """
            Determine the list of locations that each player is closest to
        """
        distances = {}
        players = defaultdict(set)
        cell_queue = deque()

        # determine the starting cells and valid squares 
        # (where food can be placed)
        for row, cell_row in enumerate(self.map):
            for col, cell in enumerate(cell_row):
                loc = (row, col)
                if cell >= 0:
                    distances[loc] = 0
                    players[loc].add(cell)
                    cell_queue.append(loc)
                elif cell != WATER:
                    distances[loc] = None

        # use bfs to determine who can reach each cell first
        while cell_queue:
            c_loc = cell_queue.popleft()
            for d in AIM:
                n_loc = self.destination(c_loc[0], c_loc[1], d)
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

    def find_closest_land(self, coord):
        """
            Find the closest square to coord which is a land square using BFS
            Return None if no square is found
        """

        if self.map[coord[1]][coord[0]] == LAND:
            return coord

        visited = set()
        cell_queue = deque([coord])

        while cell_queue:
            c_row, c_col = cell_queue.popleft()

            for d in AIM:
                n_loc = self.destination(c_row, c_col, d)
                if n_loc in visited: continue

                if self.map[n_loc[1]][n_loc[0]] == LAND:
                    return n_loc

                visited.add(n_loc)
                cell_queue.append(n_loc)

        return None

    def do_food_none(self, amount=0):
        pass

    def do_food_random(self, amount=1):
        """
            Place food randomly on the map
        """
        for f in range(amount*self.num_players):
            for t in range(10):
                row = randrange(self.height)
                col = randrange(self.width)
                if self.map[row][col] == LAND:
                    self.add_food((row, col))
                    break

    def do_food_offset(self, amount=1):
        """
            Pick a col/row offset each turn. Calculate this offset for each 
            bots starting location and place food there. If the spot is not
            land, find the closest land to that spot and place the food there.
        """
        for f in range(amount):
            dr = -self.height//4 + randrange(self.height//2)
            dc = -self.width//4  + randrange(self.width//2)
            for row, col in self.initial_ant_list:
                col = (col+dc)%self.width
                row = (row+dr)%self.height
                coord = self.find_closest_land((row, col))
                if coord:
                    self.add_food(coord)

    def do_food_sections(self, amount=1):
        """
            Split the map into sections that each ant can access 
            first at the start of the game. Place food evenly into each space.
        """
        for f in range(amount):
            for p in range(self.num_players):
                squares = self.initial_access_map[p]
                for t in range(10):
                    row, col = choice(squares)
                    if self.map[row][col] == LAND:
                        self.add_food((row, col))
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
        self.killed_ants = []
        for ant in self.current_ants.values():
            ant.moved = False

    def finish_turn(self):
        self.turn_reveal= [[] for i in range(self.num_players)]
        self.resolve_orders()
        self.do_attack()
        self.do_spawn()
        self.do_food()
        for i, s in enumerate(self.score):
            self.score_history[i].append(s)

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
        if self.seed is not None:
            tmp += 'seed %d\n' %(self.seed,)
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
            return bool(self.player_ants(player))

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
        for loc, ant in self.current_ants.items():
            ant_count[ant.owner] += 1
        return {'ant_count': ant_count}

    def __str__(self):
        result = []
        # required params
        result.append(['v', 'ants', '1'])
        result.append(['players', self.num_players])

        # optional params
        result.append(['loadtime', self.loadtime])
        result.append(['turntime', self.turntime])
        result.append(['rows', self.height])
        result.append(['cols', self.width])
        result.append(['turns', self.turns])
        result.append(['viewradius2', self.viewradius])
        result.append(['attackradius2', self.attackradius])
        result.append(['spawnradius2', self.spawnradius])
        if self.seed is not None:
            result.append(['seed', self.seed])

        # map
        result.append([self.render_map()])

        # food
        for f in self.all_food:
            food_data = ['f', f.loc[0], f.loc[1], f.start_turn]
            if f.end_turn:
                food_data.append(f.end_turn)
            result.append(food_data)

        # ants
        for a in self.all_ants:
            result.append([
                'a', a.owner, a.initial_loc[0], a.initial_loc[1], 
                a.spawn_turn, ''.join(a.orders)
            ])

        # scores
        for s in self.score_history:
            result.append(['s'] + map(int, s))

        result.append([]) # final new line

        return '\n'.join(' '.join(map(str,s)) for s in result)

class Ant:
    def __init__(self, loc, owner, spawn_turn=None):
        self.loc = loc
        self.initial_loc = loc
        self.owner = owner
        self.killed = False

        self.prev_loc = None
        self.initial_loc = loc
        self.spawn_turn = spawn_turn
        self.orders = []

        self.moved = False

    def move(self, new_loc, direction='-'):
        # ignore duplicate moves
        if self.moved:
            raise Exception("Move ant error",
                            "This ant was already moved from %s to %s"
                            %(self.prev_loc, self.loc))

        self.prev_loc = self.loc
        self.loc = new_loc
        self.moved = True
        self.orders.append(direction)

class Food:
    def __init__(self, loc, start_turn):
        self.loc = loc
        self.start_turn = start_turn
        self.end_turn = None


#!/usr/bin/env python
from random import randrange, choice, shuffle
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

PLAYER_CHARS = string.ascii_lowercase
MAP_RENDER = PLAYER_CHARS + '?!%*.'

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
        map_text = options['map']
        self.turns = int(options['turns'])
        self.loadtime = int(options['loadtime'])
        self.turntime = int(options['turntime'])
        self.viewradius = int(options["viewradius2"])
        self.attackradius = int(options["attackradius2"])
        self.spawnradius = int(options["spawnradius2"])
        self.seed = options.get('seed')

        self.do_attack = {
            'occupied': self.do_attack_occupied,
            'closest':  self.do_attack_closest,
            'support':  self.do_attack_support,
            'damage':   self.do_attack_damage
        }.get(options.get('attack'), self.do_attack_closest)

        self.do_food = {
            'none':      self.do_food_none,
            'random':    self.do_food_random,
            'sections':  self.do_food_sections,
            'symmetric': self.do_food_symmetric
        }.get(options.get('food'), self.do_food_sections)

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

        # load map and get number of players from map
        #   will fill in center data
        self.load_map(map_text)

        # used to remember where the ants started
        self.initial_ant_list = sorted(self.current_ants.values(), key=operator.attrgetter('owner'))
        self.initial_access_map = self.access_map()

        # used to track dead players, ants may still exist, but order are not processed
        self.killed = [False for i in range(self.num_players)]

        # used to give a different ordering of players to each player
        self.switch = [[None]*self.num_players + range(-5,0) for i in range(self.num_players)]
        # used to track water and land already reveal to player
        # ants and food will reset spots so a second land entry will be sent
        self.revealed = [[[False for col in range(self.width)]
                          for row in range(self.height)]
                         for p in range(self.num_players)]
        # used to track what a player can see
        self.vision = [self.get_vision(i) for i in range(self.num_players)]
        # used to track new water squares that the player can see
        self.revealed_water = [[] for i in range(self.num_players)]

        # initialise data that remembers what players have seen
        self.update_revealed()

        # used to track scores
        self.score = [Fraction(0,1)]*self.num_players
        self.score_history = [[s] for s in self.score]

    def distance(self, x, y):
        """ Returns distance between x and y squared """
        d_row = abs(x[0] - y[0])
        d_row = min(d_row, self.height - d_row)
        d_col = abs(x[1] - y[1])
        d_col = min(d_col, self.width - d_col)
        return d_row**2 + d_col**2

    def load_map(self, map_text):
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
                    if c in PLAYER_CHARS:
                        if not c in players:
                            players.append(c)
                            #if self.center[value] == None:
                            #    self.center[value] = (last_row, col)
                        value = players.index(c)
                        self.map[-1].append(value)
                        self.add_ant((row, col), value)
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

    def get_vision(self, player):
        """ Determine which squares are visible to the given player """

        vision = [[False for col in range(self.width)] for row in range(self.height)]
        squares_to_check = deque()
        for ant in self.player_ants(player):
            squares_to_check.append((ant.loc, ant.loc))
        while squares_to_check:
            a_loc, v_loc = squares_to_check.popleft()
            for d in AIM.values():
                n_loc = self.destination(v_loc, d)
                n_row, n_col = n_loc
                if not vision[n_row][n_col] and self.distance(a_loc, n_loc) <= self.viewradius:
                    vision[n_row][n_col] = True
                    squares_to_check.append((a_loc, n_loc))
        return vision

    def update_revealed(self):
        """ Make updates to state based on what each player can see

            Update self.revealed to reflect the updated vision
            Update self.switch for any new enemies
            Update self.revealed_water
        """
        self.revealed_water = []
        for player in range(self.num_players):
            water = []
            revealed = self.revealed[player]
            switch = self.switch[player]
            for row, squares in enumerate(self.vision[player]):
                for col, visible in enumerate(squares):
                    if not visible:
                        continue

                    value = self.map[row][col]

                    # if this player encounters a new enemy then
                    #   assign the enemy the next index
                    if value >= ANTS and switch[value] == None:
                        switch[value] = self.num_players - switch.count(None)

                    # mark square as revealed and determine if we see any
                    #   new water
                    if not revealed[row][col]:
                        revealed[row][col] = True
                        if value == WATER:
                            water.append((row,col))

            self.revealed_water.append(water)

    def get_perspective(self, player):
        v = self.vision[player]
        result = []
        for row, squares in enumerate(self.map):
            result.append([
                self.switch[player][square] if v[row][col] else UNSEEN
                for col, square in enumerate(squares)
            ])
        return result

    def render_changes(self, player):
        updates = self.get_state_updates()
        v = self.vision[player]
        visible_updates = []

        # first add unseen water
        for row, col in self.revealed_water[player]:
            visible_updates.append(['w', row, col])

        # next list all transient objects
        for update in updates:
            type, row, col = update[0:3]

            # only include visible updates
            if v[row][col]:
                visible_updates.append(update)

                # switch player perspective of player numbers
                if type in ['a','d']:
                    update[-1] = self.switch[player][update[-1]]

        visible_updates.append([]) # newline
        return '\n'.join(' '.join(map(str,s)) for s in visible_updates)

    def get_state_updates(self):
        updates = []

        # current ants
        for ant in self.current_ants.values():
            updates.append(['a', ant.loc[0], ant.loc[1], ant.owner])
        # visible food
        for row, col in self.current_food:
            updates.append(['f', row, col])
        # dead ants 
        for ant in self.killed_ants:
            updates.append(['d', ant.loc[0], ant.loc[1], ant.owner])

        return updates

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
    def nearby_ants(self, loc, exclude=None, min_dist=1, max_dist=2):
        row, col = loc
        mx = SQRT[max_dist]
        for d_row in range(-mx,mx+1):
            for d_col in range(-mx,mx+1):
                d = d_row**2 + d_col**2
                if d >= min_dist and d <= max_dist:
                    n_row = (row + d_row) % self.height
                    n_col = (col + d_col) % self.width
                    ant = self.current_ants.get((n_row, n_col), None)
                    if ant and ant.owner != exclude:
                        yield ant

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
            row1, col1, d = order
            row2, col2 = self.destination((row1, col1), AIM[d])

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
        new_ant_locations = []
        for f_loc in self.current_food.keys():
            owner = None
            for ant in self.nearby_ants(f_loc, None, 1, self.spawnradius):
                if owner == None:
                    owner = ant.owner
                elif owner != ant.owner:
                    self.remove_food(f_loc)
                    break
            else:
                if owner != None:
                    food = self.remove_food(f_loc)
                    new_ant_locations.append((food, owner))

        # Create new ants
        for food, owner in new_ant_locations:
            self.add_ant(food, owner)

    def add_food(self, loc):
        if loc in self.current_food:
            raise Exception("Add food error",
                            "Food already found at %s" %(loc,))
        self.map[loc[0]][loc[1]] = FOOD
        food = Food(loc, self.turn)
        self.current_food[loc] = food
        self.all_food.append(food)
        return food

    def remove_food(self, loc):
        try:
            self.map[loc[0]][loc[1]] = LAND
            food = self.current_food[loc]
            food.end_turn = self.turn
            del self.current_food[loc]
            return food
        except KeyError:
            raise Exception("Remove food error",
                            "Food not found at %s" %(loc,))

    def add_ant(self, food, owner):
        # if we weren't given a Food object then create a dummy food
        if not isinstance(food, Food):
            loc = food
            self.add_food(loc)
            food = self.remove_food(loc)

        loc = food.loc
        if loc in self.current_ants:
            raise Exception("Add ant error",
                            "Ant already found at %s" %(loc,))
        ant = Ant(loc, owner, self.turn)
        row, col = loc
        self.map[row][col] = owner
        self.all_ants.append(ant)
        self.current_ants[loc] = ant
        food.ant = ant
        return ant

    def kill_ant(self, loc):
        try:
            self.map[loc[0]][loc[1]] = LAND
            ant = self.current_ants[loc]
            self.killed_ants.append(ant)
            ant.killed = True
            ant.die_turn = self.turn
            del self.current_ants[loc]
            return ant
        except KeyError:
            raise Exception("Kill ant error",
                            "Ant not found at %s" %(loc,))

    def player_ants(self, player):
        return [ant for ant in self.current_ants.values() if player == ant.owner]

    def do_attack_damage(self):
        """ Kill ants which take more than 1 damage in a turn

            Each ant deals 1/#enemies damage to each enemy.
            Any ant with over 1 damage dies.
            Damage does not accumulate over turns 
              (ie, ants heal at the end of the battle).
        """

        damage = defaultdict(Fraction)
        nearby_enemies = {}

        # each ant damages nearby enemies
        for ant in self.current_ants.values():
            enemies = list(self.nearby_ants(ant.loc, ant.owner, 1, self.attackradius))
            if enemies:
                nearby_enemies[ant] = enemies
                damage_per_enemy = Fraction(1, len(enemies))
                for enemy in enemies:
                    damage[enemy] += damage_per_enemy

        # kill ants with damage > 1
        for ant in damage:
            if damage[ant] >= 1:
                self.kill_ant(ant.loc)
                score = Fraction(1, len(nearby_enemies[ant]))
                for enemy in nearby_enemies[ant]:
                    self.score[enemy.owner] += score

    def do_attack_support(self):
        """ Kill ants which have more enemies nearby than friendly ants 

            An ant dies if the number of enemy ants within the attackradius
            is greater than the number of friendly ants within the attackradius.
            The current ant is not counted in the friendly ant count.

            1 point is distributed evenly among the enemies of the dead ant.
        """

        # map ants (to be killed) to the enemies that kill it
        ants_to_kill = {}
        for ant in self.current_ants.values():
            enemies = []
            friends = []
            # sort nearby ants into friend and enemy lists
            for nearby_ant in self.nearby_ants(ant.loc, None, 1, self.attackradius):
                if nearby_ant.owner == ant.owner:
                    friends.append(nearby_ant)
                else:
                    enemies.append(nearby_ant)
            # add ant to kill list if it doesn't have enough support
            if len(friends) < len(enemies):
                ants_to_kill[ant] = enemies

        # actually do the killing and score distribution
        for ant, enemies in ants_to_kill.items():
            self.kill_ant(ant.loc)
            score_share = len(enemies)
            for enemy in enemies:
                self.score[enemy.owner] += Fraction(1, score_share)

    def do_attack_occupied(self):
        """ Kill ants which are the most surrounded by enemies

            For a given ant define: Power = 1/NumOpponents
            An ant's Opponents are enemy ants which are within the attackradius.
            Ant alive if its Power is greater than Power of any of his Opponents.
            If an ant dies 1 point is shared equally between its Opponents.
        """

        # maps ants to nearby enemies
        nearby_enemies = {}
        for ant in self.current_ants.values():
            nearby_enemies[ant] = list(self.nearby_ants(ant.loc, ant.owner, 1, self.attackradius))

        # determine which ants to kill
        ants_to_kill = []
        for ant in self.current_ants.values():
            # determine this ants weakness (1/power)
            weakness = len(nearby_enemies[ant])
            # an ant with no enemies nearby can't be attacked
            if weakness == 0:
                continue
            # determine the most powerful nearby enemy
            min_enemy_weakness = min(len(nearby_enemies[enemy]) for enemy in nearby_enemies[ant])
            # ant dies if it is weak as or weaker than an enemy weakness
            if min_enemy_weakness <= weakness:
                ants_to_kill.append(ant)

        # kill ants and distribute score
        for ant in ants_to_kill:
            self.kill_ant(ant.loc)
            score_share = len(nearby_enemies[ant])
            for enemy in nearby_enemies[ant]:
                self.score[enemy.owner] += Fraction(1, score_share)

    def do_attack_closest(self):
        """ Iteratively kill neighbouring groups of ants """

        # maps ants to nearby enemies by distance
        ants_by_distance = {}
        for ant in self.current_ants.values():
            # pre-compute distance to each enemy in range
            dist_map = defaultdict(list)
            for enemy in self.nearby_ants(ant.loc, ant.owner, 1, self.attackradius):
                dist_map[self.distance(ant.loc, enemy.loc)].append(enemy)
            ants_by_distance[ant] = dist_map

        # create helper method to find ant groups
        ant_group = set()
        def find_enemy(ant, distance):
            """ Recursively finds a group of ants to eliminate each other """
            # we only need to check ants at the given distance, because closer
            #   ants would have been eliminated already
            for enemy in ants_by_distance[ant][distance]:
                if not enemy.killed and enemy not in ant_group:
                    ant_group.add(enemy)
                    find_enemy(enemy, distance)

        # setup done - start the killing
        for distance in range(1, self.attackradius):
            for ant in self.current_ants.values():
                if not ants_by_distance[ant] or ant.killed:
                    continue

                ant_group = set([ant])
                find_enemy(ant, distance)

                # kill all ants in groups with more than 1 ant
                #  this way of killing is order-independent because the
                #  the ant group is the same regardless of which ant
                #  you start looking at
                if len(ant_group) > 1:
                    score_share = len(ant_group)
                    for ant in ant_group:
                        self.score[ant.owner] += Fraction(1, score_share)
                        self.kill_ant(ant.loc)

    def destination(self, loc, d):
        return ((loc[0] + d[0]) % self.height, (loc[1] + d[1]) % self.width)

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
            for d in AIM.values():
                n_loc = self.destination(c_loc, d)
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

        if self.map[coord[0]][coord[1]] == LAND:
            return coord

        visited = set()
        cell_queue = deque([coord])

        while cell_queue:
            c_loc = cell_queue.popleft()

            for d in AIM.values():
                n_loc = self.destination(c_loc, d)
                if n_loc in visited: continue

                if self.map[n_loc[0]][n_loc[1]] == LAND:
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
            for ant in self.initial_ant_list: # assumes one ant per player
                row = (ant.loc[0]+dr)%self.height
                col = (ant.loc[1]+dc)%self.width
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

    def do_food_symmetric(self, amount=1):
        """
            Split the map into sections that each ant can access 
            first at the start of the game. Place food evenly into each space.
        """
        if not hasattr(self, 'food_sets'):
            self.food_sets = deque(self.get_symmetric_food_sets())
            # add a sentinal so we know when to shuffle
            self.food_sets.append(None)

            # counter for food locations
            self.pending_food = defaultdict(int)

        # increment food counter for food spawning locations
        for f in range(amount):
            s = self.food_sets.pop()
            # if we finished one rotation, shuffle for the next
            if s == None:
                shuffle(self.food_sets)
                self.food_sets.appendleft(None)
                s = self.food_sets.pop()
            self.food_sets.appendleft(s)

            for loc in s:
                self.pending_food[loc] += 1

        # place food in scheduled locations if they are free
        for loc in self.pending_food.keys():
            if self.map[loc[0]][loc[1]] == LAND:
                self.add_food(loc)
                self.pending_food[loc] -= 1

                # remove from queue if the count reaches 0
                if not self.pending_food[loc]:
                    del self.pending_food[loc]

    def get_symmetric_food_sets(self):
        ant1, ant2 = self.initial_ant_list[0:2] # assumed one ant per player
        row_t = abs(ant1.loc[0] - ant2.loc[0])
        col_t = abs(ant1.loc[1] - ant2.loc[1])
        food_sets = []
        visited = [[False for col in range(self.width)]
                          for row in range(self.height)]

        for row, squares in enumerate(visited):
            for col, square in enumerate(squares):
                # if this square has been visited then we don't need to process
                if square:
                    continue

                # possible food locations
                locations = [
                    self.destination((row, col), (n*row_t, n*col_t))
                    for n in range(self.num_players)
                ]

                # set locations to visited
                for loc in locations:
                    # we should not have visited these locations yet
                    # this also catches duplicates in the current list
                    if visited[loc[0]][loc[1]]:
                        raise Exception("Invalid map",
                                        "This map does not support symmetric food placement")
                    visited[loc[0]][loc[1]] = True

                # we only care about sets where none of the locations hit water
                if all(self.map[loc[0]][loc[1]] != WATER for loc in locations):
                    food_sets.append(locations)

        return food_sets

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
        self.revealed_water = [[] for i in range(self.num_players)]

    def finish_turn(self):
        self.resolve_orders()
        self.do_attack()
        self.do_spawn()
        self.do_food()

        for i, s in enumerate(self.score):
            self.score_history[i].append(s)

        # now that all the ants have moved we can update the vision
        self.vision = [self.get_vision(i) for i in range(self.num_players)]
        self.update_revealed()

    # used for 'map hack' streaming playback
    def get_state(self):
        updates = self.get_state_updates()
        updates.append([]) # newline

        return '\n'.join(' '.join(map(str,s)) for s in updates)

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
        return self.render_changes(player)

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

    def get_replay(self):
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

        # food and ants combined
        for food in self.all_food:
            ant_data = ['a', food.loc[0], food.loc[1], food.start_turn]
            if food.end_turn == None:
                # food survives to end of game
                ant_data.append(self.turn + 1)
            elif food.ant == None:
                # food disappears
                ant_data.append(food.end_turn)
            else:
                # food got converted to an ant
                ant = food.ant
                ant_data.append(ant.spawn_turn)
                if not ant.killed:
                    ant_data.append(self.turn + 1)
                else:
                    ant_data.append(ant.die_turn)
                ant_data.append(ant.owner)
                ant_data.append(''.join(ant.orders))

            result.append(ant_data)

        # scores
        for s in self.score_history:
            result.append(['s'] + map(int, s))

        result.append([]) # final new line

        return '\n'.join(' '.join(map(str,s)) for s in result)

class Ant:
    def __init__(self, loc, owner, spawn_turn=None):
        self.loc = loc
        self.owner = owner

        self.prev_loc = None
        self.initial_loc = loc
        self.spawn_turn = spawn_turn
        self.die_turn = None
        self.orders = []
        self.killed = False

        self.moved = False

    def __str__(self):
        return '(%s, %s, %s, %s, %s)' % (self.initial_loc, self.owner, self.spawn_turn, self.die_turn, ''.join(self.orders))

    def move(self, new_loc, direction='-'):
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
        self.ant = None

    def __str__(self):
        return '(%s, %s, %s)' % (self.loc, self.start_turn, self.end_turn)


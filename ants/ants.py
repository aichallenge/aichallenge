#!/usr/bin/env python
from random import randrange, choice, shuffle
from math import sqrt
import os
from collections import deque, defaultdict
from fractions import Fraction
import operator
import string
from game import Game

ANTS = 0
LAND = -1
FOOD = -2
WATER = -3
CONFLICT = -4
UNSEEN = -5

PLAYER_CHARS = string.ascii_lowercase
MAP_RENDER = PLAYER_CHARS + '?!%*.'

# possible directions an ant can move
AIM = {'n': (-1, 0),
       'e': (0, 1),
       's': (1, 0),
       'w': (0, -1)}

# precalculated sqrt
SQRT = [int(sqrt(r)) for r in range(101)]

class Ants(Game):
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
            'power':   self.do_attack_power,
            'closest': self.do_attack_closest,
            'support': self.do_attack_support,
            'damage':  self.do_attack_damage
        }.get(options.get('attack'), self.do_attack_damage)

        self.do_food = {
            'none':      self.do_food_none,
            'random':    self.do_food_random,
            'sections':  self.do_food_sections,
            'symmetric': self.do_food_symmetric
        }.get(options.get('food'), self.do_food_sections)

        self.width = None   # the map
        self.height = None
        self.map = None
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

        # cache used by neighbourhood_offsets() to determine nearby squares
        self.offsets_cache = {}

        # used to track dead players, ants may still exist, but order are not processed
        self.killed = [False for i in range(self.num_players)]

        # used to give a different ordering of players to each player
        #   initialised to ensure that each player thinks they are player 0
        self.switch = [[None]*self.num_players + range(-5,0) for i in range(self.num_players)]
        for i in range(self.num_players):
            self.switch[i][i] = 0
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
        """ Parse the map_text and initialise the map data """
        players = []
        self.map = []
        row = 0
        water_area = 0

        for line in map_text.split('\n'):
            line = line.strip().lower()

            # ignore blank lines and comments
            if not line or line[0] == '#':
                continue

            key, value = line.split(' ')
            if key == 'cols':
                self.width = int(value)
            elif key == 'rows':
                self.height = int(value)
            elif key == 'm':
                if len(value) != self.width:
                    raise Exception("map",
                                    "Incorrect number of cols in row %s. "
                                    "Got %s, expected %s."
                                    %(row, len(value), self.width))
                self.map.append([LAND]*self.width)
                for col, c in enumerate(value):
                    if c in PLAYER_CHARS:
                        # assign player ids in the order that we see them
                        #  (so player 'a' won't necessarily be 0, and so on)
                        if c not in players:
                            players.append(c)
                        self.add_ant((row, col), players.index(c))
                    elif c == MAP_RENDER[FOOD]:
                        self.add_food((row, col))
                    elif c == MAP_RENDER[WATER]:
                        self.map[row][col] = WATER
                        water_area += 1
                    elif c != MAP_RENDER[LAND]:
                        raise Exception("map",
                                        "Invalid character in map: %s" % c)
                row += 1

        if self.height != row:
            raise Exception("map",
                            "Incorrect number of rows.  Expected %s, got %s"
                            % (self.height, row))

        self.land_area = self.width*self.height - water_area
        self.num_players = len(players)

    def neighbourhood_offsets(self, max_dist):
        """ Return a list of squares within a given distance of loc

            Loc is not included in the list
            For all squares returned: 0 < distance(loc,square) <= max_dist

            Offsets are calculated so that:
              -height <= row+offset_row < height (and similarly for col)
              negative indicies on self.map wrap thanks to python
        """

        if max_dist not in self.offsets_cache:
            offsets = []
            mx = int(sqrt(max_dist))
            for d_row in range(-mx,mx+1):
                for d_col in range(-mx,mx+1):
                    d = d_row**2 + d_col**2
                    if 0 < d <= max_dist:
                        offsets.append((
                            d_row%self.height-self.height,
                            d_col%self.width-self.width
                        ))
            self.offsets_cache[max_dist] = offsets
        return self.offsets_cache[max_dist]

    def get_vision(self, player):
        """ Determine which squares are visible to the given player """

        offsets = self.neighbourhood_offsets(self.viewradius)
        vision = [[False]*self.width for row in range(self.height)]
        for ant in self.player_ants(player):
            a_row, a_col = ant.loc
            vision[a_row][a_col] = True
            for v_row, v_col in offsets:
                # offsets are such that there is never an IndexError
                vision[a_row+v_row][a_col+v_col] = True
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

    def get_perspective(self, player=None):
        """ Get the map from the perspective of the given player

            If player is None, the map is return unaltered.
            Squares that are outside of the player's vision are
               marked as UNSEEN.
            Enemy identifiers are changed to reflect the order in
               which the player first saw them.
        """
        if player is None:
            return self.map

        v = self.vision[player]
        result = []
        for row, squares in enumerate(self.map):
            result.append([
                self.switch[player][square] if v[row][col] else UNSEEN
                for col, square in enumerate(squares)
            ])
        return result

    def render_changes(self, player):
        """ Create a string which communicates the updates to the state

            Water which is seen for the first time is included.
            All visible transient objects (ants, food) are included.
        """
        updates = self.get_state_changes()
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

    def get_state_changes(self):
        """ Return a list of all transient objects on the map.

            Food, living ants, ants killed this turn
            Changes are sorted so that the same state will result in the same output
        """
        changes = []

        # current ants
        changes.extend(sorted(
            ['a', ant.loc[0], ant.loc[1], ant.owner]
            for ant in self.current_ants.values()
        ))
        # current food
        changes.extend(sorted(
            ['f', row, col]
            for row, col in self.current_food
        ))
        # ants killed this turn
        changes.extend(sorted(
            ['d', ant.loc[0], ant.loc[1], ant.owner]
            for ant in self.killed_ants
        ))

        return changes

    def render_map(self, player=None):
        """ Render the map from the perspective of the given player.

            If player is None, then no squares are hidden and player ids
              are not reordered.
        """
        result = []
        for row in self.get_perspective(player):
            result.append('m ' + ''.join([MAP_RENDER[col] for col in row]))
        result.append('') # newline
        return '\n'.join(result)

    def nearby_ants(self, loc, max_dist, exclude=None):
        """ Returns ants where 0 < dist to loc <= sqrt(max_dist)

            If exclude is not None, ants with owner == exclude
              will be ignored.
        """
        ants = []
        row, col = loc
        for d_row, d_col in self.neighbourhood_offsets(max_dist):
            if ANTS <= self.map[row+d_row][col+d_col] != exclude:
                n_loc = self.destination(loc, (d_row, d_col))
                ants.append(self.current_ants[n_loc])
        return ants

    def parse_orders(self, player, lines):
        """ Parse orders from the given player

            Orders must be of the form: o row col direction
            row, col must be integers
            direction must be in (n,s,e,w)
        """
        orders = []
        valid = []
        invalid = []

        for line in lines:
            line = line.strip().lower()
            # ignore blank lines and comments
            if not line or line[0] == '#':
                continue

            data = line.split()

            # validate data format
            if data[0] != 'o':
                invalid.append((line, 'unknown action'))
                continue
            if len(data) != 4:
                invalid.append((line, 'incorrectly formatted order'))
                continue

            row, col, direction = data[1:]
            loc = None

            # validate the data types
            try:
                loc = int(row), int(col)
            except ValueError:
                invalid.append((line,'invalid row or col'))
                continue
            if direction not in AIM:
                invalid.append((line,'invalid direction'))
                continue

            # this order can be parsed
            orders.append((loc, direction))
            valid.append(line)

        return orders, valid, invalid

    def validate_orders(self, player, orders, lines, invalid):
        """ Validate orders from a given player

            Location (row, col) must be ant belonging to the player
            direction must not be blocked
            Can't multiple orders to one ant
        """
        valid = []
        valid_orders = []
        seen_locations = set()
        for line, (loc, direction) in zip(lines, orders):
            # validate orders
            if loc in seen_locations:
                invalid.append((line,'duplicate order'))
                continue
            if self.map[loc[0]][loc[1]] != player:
                invalid.append((line,'not player ant'))
                continue
            dest = self.destination(loc, AIM[direction])
            if self.map[dest[0]][dest[1]] in (FOOD, WATER):
                invalid.append((line,'moved blocked'))
                continue

            # this order is valid!
            valid_orders.append((loc, direction))
            valid.append(line)
            seen_locations.add(loc)

        return valid_orders, valid, invalid

    def do_orders(self):
        """ Execute player orders and handle conflicts

            All ants are moved to their new positions.
            Any ants which occupy the same square are killed.
        """
        # set old ant locations to land
        for ant in self.current_ants.values():
            row, col = ant.loc
            self.map[row][col] = LAND

        # determine the direction that each ant moves
        #  (holding any ants that don't have orders)
        move_direction = {}
        for player, orders in enumerate(self.orders):
            for loc, direction in orders:
                move_direction[self.current_ants[loc]] = direction
        for ant in self.current_ants.values():
            if ant not in move_direction:
                move_direction[ant] = '-'

        # move all the ants
        next_loc = defaultdict(list)
        for ant, direction in move_direction.items():
            ant.loc = self.destination(ant.loc, AIM.get(direction, (0,0)))
            ant.orders.append(direction)
            next_loc[ant.loc].append(ant)

        # if ant is sole occupant of a new square then it survives
        self.current_ants = {}
        for loc, ants in next_loc.items():
            if len(ants) == 1:
                self.current_ants[loc] = ants[0]
            else:
                for ant in ants:
                    self.kill_ant(ant, True)

        # set new ant locations
        for ant in self.current_ants.values():
            row, col = ant.loc
            self.map[row][col] = ant.owner

    def do_spawn(self):
        """ Spawn new ants from food

            If there are no ants within spawnradius of a food then
              the food remains.
            If all the ants within spawnradius of a food are owned by the
              same player then the food gets converted to an ant owned by
              that player.
            If ants of more than one owner are within spawnradius of a food
              then that food disappears.
        """
        # Determine new ant locations
        new_ant_locations = []
        for f_loc in self.current_food.keys():
            # find the owners of all the ants near the food
            nearby_players = set(
                ant.owner for ant in self.nearby_ants(f_loc, self.spawnradius)
            )

            if len(nearby_players) == 1:
                # spawn food because there is only one player near the food
                food = self.remove_food(f_loc)
                new_ant_locations.append((food, nearby_players.pop()))
            elif nearby_players:
                # remove food because it is contested
                self.remove_food(f_loc)

        # Create new ants
        for food, owner in new_ant_locations:
            self.add_ant(food, owner)

    def add_food(self, loc):
        """ Add food to a location

            An error is raised if the location is not free
        """
        if self.map[loc[0]][loc[1]] != LAND:
            raise Exception("Add food error",
                            "Food already found at %s" %(loc,))
        self.map[loc[0]][loc[1]] = FOOD
        food = Food(loc, self.turn)
        self.current_food[loc] = food
        self.all_food.append(food)
        return food

    def remove_food(self, loc):
        """ Remove food from a location

            An error is raised if no food exists there.
        """
        try:
            self.map[loc[0]][loc[1]] = LAND
            self.current_food[loc].end_turn = self.turn
            return self.current_food.pop(loc)
        except KeyError:
            raise Exception("Remove food error",
                            "Food not found at %s" %(loc,))

    def add_ant(self, food, owner):
        """ Spawn an ant from a food square

            If a location is given instead of a food objects, then a dummy
              food is placed at the location to spawn from. This is required
              for the replay format where all ants must come from food.
        """
        # if we weren't given a Food object then create a dummy food
        if not isinstance(food, Food):
            loc = food
            self.add_food(loc)
            food = self.remove_food(loc)

        loc = food.loc
        ant = Ant(loc, owner, self.turn)
        row, col = loc
        self.map[row][col] = owner
        self.all_ants.append(ant)
        self.current_ants[loc] = ant
        food.ant = ant
        return ant

    def kill_ant(self, ant, ignore_error=False):
        """ Kill the ant at the given location

            Raises an error if no ant is found at the location
              (if ignore error is set to False)
        """
        try:
            loc = ant.loc
            self.map[loc[0]][loc[1]] = LAND
            self.killed_ants.append(ant)
            ant.killed = True
            ant.die_turn = self.turn
            return self.current_ants.pop(loc)
        except KeyError:
            if not ignore_error:
                raise Exception("Kill ant error",
                                "Ant not found at %s" %(loc,))

    def player_ants(self, player):
        """ Return the current ants belonging to the given player """
        return [ant for ant in self.current_ants.values() if player == ant.owner]

    def do_attack_damage(self):
        """ Kill ants which take more than 1 damage in a turn

            Each ant deals 1/#nearby_enemy damage to each nearby enemy.
              (nearby enemies are those within the attackradius)
            Any ant with at least 1 damage dies.
            Damage does not accumulate over turns 
              (ie, ants heal at the end of the battle).
        """

        damage = defaultdict(Fraction)
        nearby_enemies = {}

        # each ant damages nearby enemies
        for ant in self.current_ants.values():
            enemies = self.nearby_ants(ant.loc, self.attackradius, ant.owner)
            if enemies:
                nearby_enemies[ant] = enemies
                damage_per_enemy = Fraction(1, len(enemies))
                for enemy in enemies:
                    damage[enemy] += damage_per_enemy

        # kill ants with at least 1 damage
        for ant in damage:
            if damage[ant] >= 1:
                self.kill_ant(ant)
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
            for nearby_ant in self.nearby_ants(ant.loc, self.attackradius, ant.owner):
                if nearby_ant.owner == ant.owner:
                    friends.append(nearby_ant)
                else:
                    enemies.append(nearby_ant)
            # add ant to kill list if it doesn't have enough support
            if len(friends) < len(enemies):
                ants_to_kill[ant] = enemies

        # actually do the killing and score distribution
        for ant, enemies in ants_to_kill.items():
            self.kill_ant(ant)
            score_share = len(enemies)
            for enemy in enemies:
                self.score[enemy.owner] += Fraction(1, score_share)

    def do_attack_power(self):
        """ Kill ants which are the most surrounded by enemies

            For a given ant define: Power = 1/NumOpponents
            An ant's Opponents are enemy ants which are within the attackradius.
            Ant alive if its Power is greater than Power of any of his Opponents.
            If an ant dies 1 point is shared equally between its Opponents.
        """

        # maps ants to nearby enemies
        nearby_enemies = {}
        for ant in self.current_ants.values():
            nearby_enemies[ant] = self.nearby_ants(ant.loc, self.attackradius, ant.owner)

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
            self.kill_ant(ant)
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
            for enemy in self.nearby_ants(ant.loc, self.attackradius, ant.owner):
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
                        self.kill_ant(ant)

    def destination(self, loc, d):
        """ Returns the location produced by offsetting loc by d """
        return ((loc[0] + d[0]) % self.height, (loc[1] + d[1]) % self.width)

    def access_map(self):
        """ Determine the list of locations that each player is closest to """
        distances = {}
        players = defaultdict(set)
        square_queue = deque()

        # determine the starting squares and valid squares
        # (where food can be placed)
        for row, squares in enumerate(self.map):
            for col, square in enumerate(squares):
                loc = (row, col)
                if square >= 0:
                    distances[loc] = 0
                    players[loc].add(square)
                    square_queue.append(loc)
                elif square != WATER:
                    distances[loc] = None

        # use bfs to determine who can reach each square first
        while square_queue:
            c_loc = square_queue.popleft()
            for d in AIM.values():
                n_loc = self.destination(c_loc, d)
                if n_loc not in distances: continue # wall

                if distances[n_loc] is None:
                    # first visit to this square
                    distances[n_loc] = distances[c_loc] + 1
                    players[n_loc].update(players[c_loc])
                    square_queue.append(n_loc)
                elif distances[n_loc] == distances[c_loc] + 1:
                    # we've seen this square before, but the distance is
                    # the same - therefore combine the players that can
                    # reach this square
                    players[n_loc].update(players[c_loc])

        # summarise the final results of the squares that are closest
        # to a single unique player
        access_map = defaultdict(list)
        for coord, player_set in players.items():
            if len(player_set) != 1: continue
            access_map[player_set.pop()].append(coord)

        return access_map

    def find_closest_land(self, coord):
        """ Find the closest square to coord which is a land square using BFS

            Return None if no square is found
        """

        if self.map[coord[0]][coord[1]] == LAND:
            return coord

        visited = set()
        square_queue = deque([coord])

        while square_queue:
            c_loc = square_queue.popleft()

            for d in AIM.values():
                n_loc = self.destination(c_loc, d)
                if n_loc in visited: continue

                if self.map[n_loc[0]][n_loc[1]] == LAND:
                    return n_loc

                visited.add(n_loc)
                square_queue.append(n_loc)

        return None

    def do_food_none(self, amount=0):
        """ Place no food """
        pass

    def do_food_random(self, amount=1):
        """ Place food randomly on the map """
        for f in range(amount*self.num_players):
            for t in range(10):
                row = randrange(self.height)
                col = randrange(self.width)
                if self.map[row][col] == LAND:
                    self.add_food((row, col))
                    break

    def do_food_offset(self, amount=1):
        """ Place food at the same offset from each player's start position

            Pick a col/row offset each turn.
            Calculate this offset for each bots starting location and place
              food there.
            If the spot is not land, find the closest land to that spot and
              place the food there.
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
        """ Place food randomly in each player's start section

            Split the map into sections that each ant can access first at
              the start of the game.
            Place food evenly into each space.
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
        """ Place food in the same relation player start positions.

            Food that can't be placed is put into a queue and is places
              as soon as the location becomes available.
            Positions are randomly orders and cycled to evenly
              distribute food.
        """
        # if this is the first time calling this function then
        #   create the food sets
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
        """ Split map into sets of squares

            Each set contains self.num_players points where each point
              is at a consistant offset from each player's starting
              position.
            Assumes map is symmetric.
        """
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
        """ Return the number of players still alive """
        return sum(self.is_alive(p) for p in range(self.num_players))

    # Common functions for all games

    def game_over(self):
        """ Determine if the game is over

            Used by the engine to determine when to finish the game.
            A game is over when there are no players remaining, or a single
              winner remaining.
        """
        return self.remaining_players() <= 1

    def kill_player(self, player):
        """ Used by engine to signal that a player is out of the game """
        self.killed[player] = True

    def start_game(self):
        """ Called by engine at the start of the game """
        self.do_food(self.land_area//100)

    def finish_game(self):
        """ Called by engine at the end of the game """
        pass

    def start_turn(self):
        """ Called by engine at the start of the turn """
        self.turn += 1
        self.killed_ants = []
        self.revealed_water = [[] for i in range(self.num_players)]
        self.orders = [[] for i in range(self.num_players)]

    def finish_turn(self):
        """ Called by engine at the end of the turn """
        self.do_orders()
        self.do_attack()
        self.do_spawn()
        self.do_food()

        for i, s in enumerate(self.score):
            self.score_history[i].append(s)

        # now that all the ants have moved we can update the vision
        self.vision = [self.get_vision(i) for i in range(self.num_players)]
        self.update_revealed()

    def get_state(self):
        """ Get all state changes

            Used by engine for streaming playback
        """
        updates = self.get_state_changes()
        updates.append([]) # newline

        return '\n'.join(' '.join(map(str,s)) for s in updates)

    def get_player_start(self, player=None):
        """ Get game parameters visible to players

            Used by engine to send bots startup info on turn 0
        """
        result = []
        result.append(['turn', 0])
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
        if player == None:
            result.append([self.render_map()])
        else:
            result.append([]) # newline
        return '\n'.join(' '.join(map(str,s)) for s in result)

    def get_player_state(self, player):
        """ Get state changes visible to player

            Used by engine to send state to bots
        """
        return self.render_changes(player)

    def is_alive(self, player):
        """ Determine if player is still alive

            Used by engine to determine players still in the game
        """
        if self.killed[player]:
            return False
        else:
            return bool(self.player_ants(player))

    def get_error(self, player):
        """ Returns the reason a player was killed

            Used by engine to report the error that kicked a player
              from the game
        """
        return ''

    def do_moves(self, player, moves):
        """ Called by engine to give latest player orders """
        orders, valid, invalid = self.parse_orders(player, moves)
        orders, valid, invalid = self.validate_orders(player, orders, valid, invalid)
        self.orders[player] = orders
        return valid, ['%s # %s' %error for error in invalid]

    def get_scores(self):
        """ Gets the scores of all players

            Used by engine fo ranking
        """
        return [int(score) for score in self.score]

    def get_stats(self):
        """ Get current ant counts

            Used by engine to report stats
        """
        ant_count = [0 for i in range(self.num_players)]
        for loc, ant in self.current_ants.items():
            ant_count[ant.owner] += 1
        return {'ant_count': ant_count}

    def get_replay(self):
        """ Return a summary of the entire game

            Used by the engine to create a replay file which may be used
              to replay the game.
        """
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

        self.initial_loc = loc
        self.spawn_turn = spawn_turn
        self.die_turn = None
        self.orders = []
        self.killed = False

    def __str__(self):
        return '(%s, %s, %s, %s, %s)' % (self.initial_loc, self.owner, self.spawn_turn, self.die_turn, ''.join(self.orders))

class Food:
    def __init__(self, loc, start_turn):
        self.loc = loc
        self.start_turn = start_turn
        self.end_turn = None
        self.ant = None

    def __str__(self):
        return '(%s, %s, %s)' % (self.loc, self.start_turn, self.end_turn)


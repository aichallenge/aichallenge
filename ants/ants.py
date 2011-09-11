#!/usr/bin/env python
from random import randrange, choice, shuffle, randint, seed
from math import sqrt
from collections import deque, defaultdict

from fractions import Fraction
import operator
import string
from game import Game
from sys import maxint
from copy import deepcopy

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
        self.engine_seed = options.get('engine_seed', randint(-maxint-1, maxint))
        self.player_seed = options.get('player_seed', randint(-maxint-1, maxint))
        seed(self.engine_seed)
        self.food_rate = options.get('food_rate', (2,8)) # total food
        if type(self.food_rate) in (list, tuple):
            self.food_rate = randrange(*self.food_rate)
        self.food_turn = options.get('food_turn', (12,30)) # per turn
        if type(self.food_turn) in (list, tuple):
            self.food_turn = randrange(*self.food_turn)
        self.food_start = options.get('food_start', (75,175)) # per land area
        if type(self.food_start) in (list, tuple):
            self.food_start = randrange(*self.food_start)
        self.food_visible = options.get('food_visible', (1,3)) # in starting loc
        if type(self.food_visible) in (list, tuple):
            self.food_visible = randrange(*self.food_visible)
        self.food_extra = Fraction(0,1)

        self.cutoff_percent = options.get('cutoff_percent', 0.90)
        self.cutoff_turn = options.get('cutoff_turn', 100)

        self.do_attack = {
            'focus':   self.do_attack_focus,
            'closest': self.do_attack_closest,
            'support': self.do_attack_support,
            'damage':  self.do_attack_damage
        }.get(options.get('attack'), self.do_attack_focus)
        self.kill_points = options.get('kill_points', 2)

        self.do_food = {
            'none':      self.do_food_none,
            'random':    self.do_food_random,
            'sections':  self.do_food_sections,
            'symmetric': self.do_food_symmetric
        }.get(options.get('food'), self.do_food_sections)

        map_data = self.parse_map(map_text)

        self.turn = 0
        self.num_players = map_data['num_players']

        self.current_ants = {} # ants that are currently alive
        self.killed_ants = []  # ants which were killed this turn
        self.all_ants = []     # all ants that have been created

        self.all_food = []     # all food created
        self.current_food = {} # food currently in game
        self.pending_food = defaultdict(int)

        # initalise scores
        self.score = [Fraction(0,1)]*self.num_players
        self.score_history = [[s] for s in self.score]
        self.bonus = [0 for s in self.score]

        # used to cutoff games early
        self.cutoff_bot = LAND # Can be ant owner, FOOD or LAND
        self.cutoff_turns = 0
        # used to calculate the turn when the winner took the lead
        self.winning_bot = None
        self.winning_turn = 0
        # used to calculate when the player rank last changed
        self.ranking_bots = None
        self.ranking_turn = 0

        # initialise size
        self.height, self.width = map_data['size']
        self.land_area = self.height*self.width - len(map_data['water'])

        # initialise map
        self.map = [[LAND]*self.width for _ in range(self.height)]

        # initialise water
        for row, col in map_data['water']:
            self.map[row][col] = WATER

        # initalise ants
        for owner, locs in map_data['ants'].items():
            for loc in locs:
                self.add_ant(loc, owner)

        # initalise food
        for loc in map_data['food']:
            self.add_food(loc)

        # track which food has been seen by each player
        self.seen_food = [set() for _ in range(self.num_players)]

        # used to remember where the ants started
        self.initial_ant_list = sorted(self.current_ants.values(), key=operator.attrgetter('owner'))
        self.initial_access_map = self.access_map()

        # cache used by neighbourhood_offsets() to determine nearby squares
        self.offsets_cache = {}

        # used to track dead players, ants may still exist, but order are not processed
        self.killed = [False for _ in range(self.num_players)]

        # used to give a different ordering of players to each player
        #   initialised to ensure that each player thinks they are player 0
        self.switch = [[None]*self.num_players + range(-5,0) for i in range(self.num_players)]
        for i in range(self.num_players):
            self.switch[i][i] = 0
        # used to track water and land already reveal to player
        # ants and food will reset spots so a second land entry will be sent
        self.revealed = [[[False for col in range(self.width)]
                          for row in range(self.height)]
                         for _ in range(self.num_players)]
        # used to track what a player can see
        self.init_vision()

        # the engine may kill players before the game starts and this is needed to prevent errors
        self.orders = [[] for i in range(self.num_players)]

    def distance(self, x, y):
        """ Returns distance between x and y squared """
        d_row = abs(x[0] - y[0])
        d_row = min(d_row, self.height - d_row)
        d_col = abs(x[1] - y[1])
        d_col = min(d_col, self.width - d_col)
        return d_row**2 + d_col**2

    def parse_map(self, map_text):
        """ Parse the map_text into a more friendly data structure """
        players = []
        width = height = None
        water = []
        food = []
        ants = defaultdict(list)
        row = 0

        for line in map_text.split('\n'):
            line = line.strip().lower()

            # ignore blank lines and comments
            if not line or line[0] == '#':
                continue

            key, value = line.split(' ', 1)
            if key == 'cols':
                width = int(value)
            elif key == 'rows':
                height = int(value)
            elif key == 'm':
                if len(value) != width:
                    raise Exception("map",
                                    "Incorrect number of cols in row %s. "
                                    "Got %s, expected %s."
                                    %(row, len(value), width))
                for col, c in enumerate(value):
                    if c in PLAYER_CHARS:
                        # assign player ids in the order that we see them
                        #  (so player 'a' won't necessarily be 0, and so on)
                        if c not in players:
                            players.append(c)
                        ants[players.index(c)].append((row,col))
                    elif c == MAP_RENDER[FOOD]:
                        food.append((row,col))
                    elif c == MAP_RENDER[WATER]:
                        water.append((row,col))
                    elif c != MAP_RENDER[LAND]:
                        raise Exception("map",
                                        "Invalid character in map: %s" % c)
                row += 1

        if height != row:
            raise Exception("map",
                            "Incorrect number of rows.  Expected %s, got %s"
                            % (height, row))

        return {
            'size':        (height, width),
            'num_players': len(players),
            'ants':        ants,
            'food':        food,
            'water':        water
        }

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

    def init_vision(self):
        """ Initialise the vision data """
        # calculate and cache vision offsets
        cache = {}
        # all offsets that an ant can see
        locs = set(self.neighbourhood_offsets(self.viewradius))
        locs.add((0,0))
        cache['new'] = list(locs)
        cache['-'] = [list(locs)]

        for d in AIM:
            # determine the previous view
            p_r, p_c = -AIM[d][0], -AIM[d][1]
            p_locs = set(
                (((p_r+r)%self.height-self.height),
                 ((p_c+c)%self.width-self.width))
                for r,c in locs
            )
            cache[d] = [list(p_locs), list(locs-p_locs), list(p_locs-locs)]
        self.vision_offsets_cache = cache

        # create vision arrays
        self.vision = []
        for _ in range(self.num_players):
            self.vision.append([[0]*self.width for __ in range(self.height)])

        # initialise the data based on the initial ants
        self.update_vision()
        self.update_revealed()

    def update_vision(self):
        """ Incrementally updates the vision data """
        for ant in self.current_ants.values():
            if not ant.orders:
                # new ant
                self.update_vision_ant(ant, self.vision_offsets_cache['new'], 1)
            else:
                order = ant.orders[-1]
                if order in AIM:
                    # ant moved
                    self.update_vision_ant(ant, self.vision_offsets_cache[order][1], 1)
                    self.update_vision_ant(ant, self.vision_offsets_cache[order][-1], -1)
                # else: ant stayed where it was
        for ant in self.killed_ants:
            order = ant.orders[-1]
            self.update_vision_ant(ant, self.vision_offsets_cache[order][0], -1)

    def update_vision_ant(self, ant, offsets, delta):
        """ Update the vision data for a single ant

            Increments all the given offsets by delta for the vision
              data for ant.owner
        """
        a_row, a_col = ant.loc
        vision = self.vision[ant.owner]
        for v_row, v_col in offsets:
            # offsets are such that there is never an IndexError
            vision[a_row+v_row][a_col+v_col] += delta

    def update_revealed(self):
        """ Make updates to state based on what each player can see

            Update self.revealed to reflect the updated vision
            Update self.switch for any new enemies
            Update self.revealed_water
            Update self.seen_food
            Update self.removed_food
        """
        self.revealed_water = []
        self.removed_food = []
        for player in range(self.num_players):
            water = []
            revealed = self.revealed[player]
            switch = self.switch[player]

            # update the removed food which was revealed this turn
            food = []
            for seen in list(self.seen_food[player]):
                # here we care about the food that the player HAS seen
                #  which has since been removed
                row, col = seen.loc
                if self.vision[player][row][col] and seen.end_turn:
                    self.seen_food[player].remove(seen)
                    food.append(seen.loc)
            self.removed_food.append(food)

            for row, squares in enumerate(self.vision[player]):
                for col, visible in enumerate(squares):
                    if not visible:
                        continue

                    value = self.map[row][col]

                    # add any food that is visible to seen_food
                    if value == FOOD:
                        self.seen_food[player].add(self.current_food[(row,col)])

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

            # update the water which was revealed this turn
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

            # only include updates to squares which are visible
            # and the current players dead ants
            if v[row][col] or (type == 'd' and update[-1] == player):
                visible_updates.append(update)

                # switch player perspective of player numbers
                if type in ['a','d']:
                    # an ant can appear in a bots vision and die the same turn
                    # in this case the ant has not been assigned a number yet
                    #   assign the enemy the next index
                    if self.switch[player][update[-1]] == None:
                        self.switch[player][update[-1]] = self.num_players - self.switch[player].count(None)
                    update[-1] = self.switch[player][update[-1]]

        # also tell the player about any food that has been removed
        #   (only for food they have already seen)
        #for row, col in sorted(self.removed_food[player]):
        #    visible_updates.append(['r',row,col])

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

    def get_map_output(self, player=None):
        """ Render the map from the perspective of the given player.

            If player is None, then no squares are hidden and player ids
              are not reordered.
        """
        result = []
        for row in self.get_perspective(player):
            result.append(''.join([MAP_RENDER[col] for col in row]))
        return result

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
        ignored = []
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

        return orders, valid, ignored, invalid

    def validate_orders(self, player, orders, lines, ignored, invalid):
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
            try:
                if self.map[loc[0]][loc[1]] != player:
                    invalid.append((line,'not player ant'))
                    continue
            except IndexError:
                invalid.append((line,'out of bounds'))
                continue
            dest = self.destination(loc, AIM[direction])
            if self.map[dest[0]][dest[1]] in (FOOD, WATER):
                ignored.append((line,'move blocked'))
                continue

            # this order is valid!
            valid_orders.append((loc, direction))
            valid.append(line)
            seen_locations.add(loc)

        return valid_orders, valid, ignored, invalid

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
        for orders in self.orders:
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
        colliding_ants = []
        for loc, ants in next_loc.items():
            if len(ants) == 1:
                self.current_ants[loc] = ants[0]
            else:
                for ant in ants:
                    self.kill_ant(ant, True)
                    colliding_ants.append(ant)

        # set new ant locations
        for ant in self.current_ants.values():
            row, col = ant.loc
            self.map[row][col] = ant.owner

        # distribute score for ants which died from collisions
        for ant in colliding_ants:
            # find living nearby enemies
            enemies = self.nearby_ants(ant.loc, self.attackradius, ant.owner)
            # in addition to the living nearby enemies, dead nearby enemies
            #   should get points too!
            for other_ant in colliding_ants:
                # only interested in enemies within range
                if other_ant.owner != ant.owner and self.distance(ant.loc, other_ant.loc) <= self.attackradius:
                    enemies.append(other_ant)
            score_share = len(enemies)
            for enemy in enemies:
                self.score[enemy.owner] += Fraction(1,score_share)

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
        # each ant gives the owner 1 point
        self.score[owner] += 1

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
                strenth = 10 # dot dot dot
                if ant.orders[-1] == '-':
                    strenth = 10
                else:
                    strenth = 10
                damage_per_enemy = Fraction(strenth, len(enemies)*10)
                for enemy in enemies:
                    damage[enemy] += damage_per_enemy

        # kill ants with at least 1 damage
        for ant in damage:
            if damage[ant] >= 1:
                self.kill_ant(ant)
                score = Fraction(self.kill_points, len(nearby_enemies[ant]))
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
                self.score[enemy.owner] += Fraction(self.kill_points, score_share)

    def do_attack_focus(self):
        """ Kill ants which are the most surrounded by enemies

            For a given ant define: Focus = 1/NumOpponents
            An ant's Opponents are enemy ants which are within the attackradius.
            Ant alive if its Focus is greater than Focus of any of his Opponents.
            If an ant dies 1 point is shared equally between its Opponents.
        """

        # maps ants to nearby enemies
        nearby_enemies = {}
        for ant in self.current_ants.values():
            nearby_enemies[ant] = self.nearby_ants(ant.loc, self.attackradius, ant.owner)

        # determine which ants to kill
        ants_to_kill = []
        for ant in self.current_ants.values():
            # determine this ants weakness (1/focus)
            weakness = len(nearby_enemies[ant])
            # an ant with no enemies nearby can't be attacked
            if weakness == 0:
                continue
            # determine the most focused nearby enemy
            min_enemy_weakness = min(len(nearby_enemies[enemy]) for enemy in nearby_enemies[ant])
            # ant dies if it is weak as or weaker than an enemy weakness
            if min_enemy_weakness <= weakness:
                ants_to_kill.append(ant)

        # kill ants and distribute score
        for ant in ants_to_kill:
            self.kill_ant(ant)
            score_share = len(nearby_enemies[ant])
            for enemy in nearby_enemies[ant]:
                self.score[enemy.owner] += Fraction(self.kill_points, score_share)

    def do_attack_closest(self):
        """ Iteratively kill neighboring groups of ants """

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
                        self.score[ant.owner] += Fraction(self.kill_points, score_share)
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
        return amount

    def do_food_random(self, amount=1):
        """ Place food randomly on the map """
        for _ in range(amount):
            while True:
                row = randrange(self.height)
                col = randrange(self.width)
                if self.map[row][col] == LAND:
                    self.pending_food[(row, col)] += 1
                    break
        self.place_food()
        return 0

    def do_food_offset(self, amount=1):
        """ Place food at the same offset from each player's start position

            Pick a col/row offset each turn.
            Calculate this offset for each bots starting location and place
              food there.
            If the spot is not land, find the closest land to that spot and
              place the food there.
        """
        left_over = amount % len(self.initial_ant_list)
        for _ in range(amount//len(self.initial_ant_list)):
            dr = -self.height//4 + randrange(self.height//2)
            dc = -self.width//4  + randrange(self.width//2)
            for ant in self.initial_ant_list: # assumes one ant per player
                row = (ant.loc[0]+dr)%self.height
                col = (ant.loc[1]+dc)%self.width
                coord = self.find_closest_land((row, col))
                if coord:
                    self.pending_food[coord] += 1
        self.place_food()
        return left_over

    def do_food_sections(self, amount=1):
        """ Place food randomly in each player's start section

            Split the map into sections that each ant can access first at
              the start of the game.
            Place food evenly into each space.
        """
        left_over = amount % self.num_players
        for _ in range(amount//self.num_players):
            for p in range(self.num_players):
                squares = self.initial_access_map[p]
                row, col = choice(squares)
                if self.map[row][col] == LAND:
                    self.pending_food[(row, col)] += 1
        self.place_food()
        return left_over

    def do_food_visible(self, amount=1):
        """ Place food in vison of starting spots """
        # if this is the first time calling this function then
        #   create the food sets
        if not hasattr(self, 'food_sets_visible'):
            self.food_sets_visible = deque(self.get_symmetric_food_sets(True))
            # add a sentinal so we know when to shuffle
            self.food_sets_visible.append(None)

        # place food while next food set is <= left over amount
        while True:
            s = self.food_sets_visible.pop()
            # if we finished one rotation, shuffle for the next
            if s == None:
                shuffle(self.food_sets_visible)
                self.food_sets_visible.appendleft(None)
                s = self.food_sets_visible.pop()

            if len(s) > amount:
                # reached food limit, save set, place food and return left over
                self.food_sets_visible.append(s)
                self.place_food()
                return amount
            else:
                amount -= len(s)
                self.food_sets_visible.appendleft(s)
                for loc in s:
                    self.pending_food[loc] += 1


    def do_food_symmetric(self, amount=1):
        """ Place food in the same relation player start positions.

            Food that can't be placed is put into a queue and is placed
              as soon as the location becomes available.
            Positions are randomly ordered and cycled to evenly
              distribute food.
        """
        # if this is the first time calling this function then
        #   create the food sets
        if not hasattr(self, 'food_sets'):
            self.food_sets = deque(self.get_symmetric_food_sets())
            # add a sentinal so we know when to shuffle
            self.food_sets.append(None)

        # place food while next food set is <= left over amount
        while True:
            s = self.food_sets.pop()
            # if we finished one rotation, shuffle for the next
            if s == None:
                shuffle(self.food_sets)
                self.food_sets.appendleft(None)
                s = self.food_sets.pop()

            if len(s) > amount:
                self.food_sets.append(s)
                self.place_food()
                return amount
            else:
                amount -= len(s)
                self.food_sets.appendleft(s)
                for loc in s:
                    self.pending_food[loc] += 1

    def place_food(self):
        """ Place food in scheduled locations if they are free
        """
        for loc in self.pending_food.keys():
            if self.map[loc[0]][loc[1]] == LAND:
                self.add_food(loc)
                self.pending_food[loc] -= 1

                # remove from queue if the count reaches 0
                if not self.pending_food[loc]:
                    del self.pending_food[loc]

    def offset_aim(self, offset, aim):
        """ Return proper offset given an orientation
        """
        # eight possible orientations
        row, col = offset
        if aim == 0:
            return offset
        elif aim == 1:
            return -row, col
        elif aim == 2:
            return row, -col
        elif aim == 3:
            return -row, -col
        elif aim == 4:
            return col, row
        elif aim == 5:
            return -col, row
        elif aim == 6:
            return col, -row
        elif aim == 7:
            return -col, -row

    def map_similar(self, loc1, loc2, aim, player):
        """ find if map is similar given loc1 aim of 0 and loc2 ant of player
            return a map of translated enemy locations
        """
        enemy_map = {}
        for row in range(self.height):
            for col in range(self.width):
                row0, col0 = self.destination(loc1, (row, col))
                row1, col1 = self.destination(loc2, self.offset_aim((row, col), aim))
                # compare locations
                ilk0 = self.map[row0][col0]
                ilk1 = self.map[row1][col1]
                if ilk0 == 0 and ilk1 != player:
                    # friendly ant not in same location
                    return None
                elif ilk0 > 0 and (ilk1 < 0 or ilk1 == player):
                    # enemy ant not in same location
                    return None
                elif ilk0 < 0 and ilk1 != ilk0:
                    # land or water not in same location
                    return None
                if ilk0 >= 0 and enemy_map != None:
                    enemy_map[ilk0] = ilk1
        return enemy_map

    def get_map_symmetry(self):
        """ Get orientation for each starting ant
        """

        # get list of player 0 ants
        ants = [ant for ant in self.initial_ant_list if ant.owner == 0]
        # list of
        #     list of tuples containing
        #         location, aim, and enemy map dict
        orientations = [[(ants[0].loc, 0,
            dict([(i, i, ) for i in range(self.num_players)]))]]
        for player in range(1, self.num_players):
            player_ants = [ant for ant in self.initial_ant_list if ant.owner == player]
            if len(player_ants) != len(ants):
                raise Exception("Invalid map",
                                "This map is not symmetric.  Player 0 has {0} ants while player {1} has {2} ants."
                                .format(len(ants), player, len(player_ants)))
            new_orientations = []
            for player_ant in player_ants:
                for aim in range(8):
                # check if map looks similar given the orientation
                    enemy_map = self.map_similar(ants[0].loc, player_ant.loc, aim, player)
                    if enemy_map != None:
                        # produce combinations of orientation sets
                        for ant_aims in orientations:
                            new_ant_aims = deepcopy(ant_aims)
                            new_ant_aims.append((player_ant.loc, aim, enemy_map))
                            new_orientations.append(new_ant_aims)
            orientations = new_orientations
            if len(orientations) == 0:
                raise Exception("Invalid map",
                                "This map is not symmetric. Player {0} does not have an orientation that matches player 0"
                                .format(player))
        # ensure types of ant aims in orientations are symmetric
        # place food set and double check symmetry
        valid_orientations = []
        for ant_aims in orientations:
            fix = []
            for loc, aim, enemy_map in ant_aims:
                row, col = self.destination(loc, self.offset_aim((1,2), aim))
                fix.append(((row, col), self.map[row][col]))
                self.map[row][col] = FOOD
            for loc, aim, enemy_map in ant_aims:
                if self.map_similar(ant_aims[0][0], loc, aim, enemy_map[0]) == None:
                    break
            else:
                valid_orientations.append(ant_aims)
            for (row, col), ilk in reversed(fix):
                self.map[row][col] = ilk
        if len(valid_orientations) == 0:
            raise Exception("Invalid map",
                            "There are no valid orientation sets")
        return valid_orientations

    def get_symmetric_food_sets(self, starting=False):
        """ Split map into sets of squares

            Each set contains self.num_players points where each point
              is at a consistent offset from each player's starting
              position.
            A square may be the same distance to more than one player
                which will cause the set to be smaller than the number of players
            Assumes map is symmetric.
        """
        if not hasattr(self, 'map_symmetry'):
            # randomly choose one symmetry
            # get_map_symmetry will raise an exception for non-symmetric maps
            self.map_symmetry = choice(self.get_map_symmetry())


        food_sets = []
        # start with only land squares
        visited = [[False for col in range(self.width)]
                          for row in range(self.height)]

        # aim for ant 0 will always be 0
        ant0 = self.map_symmetry[0][0]

        for row, squares in enumerate(visited):
            for col, square in enumerate(squares):
                # if this square has been visited then we don't need to process
                if square:
                    continue

                if starting:
                    # skip locations outside of initial ants' view radius
                    for ant in self.initial_ant_list:
                        if self.distance(ant.loc, (row, col)) <= self.viewradius:
                            break
                    else:
                        # square not close enough to any starting ant
                        continue

                # offset to ant 0
                o_row, o_col = row - ant0[0], col - ant0[1]
                # set of unique food locations based on offsets from each starting ant
                locations = list(set([
                    self.destination(loc, self.offset_aim((o_row, o_col), aim))
                    for loc, aim, _ in self.map_symmetry
                ]))
                # duplicates can happen if 2 ants are the same distance from 1 square
                # the food set will be smaller and food spawning takes this into account

                # check for spawn location next to each other
                # create food dead zone along symmetric lines
                too_close = False
                loc1 = locations[0]
                for loc2 in locations[1:]:
                    if self.distance(loc1, loc2) == 1:
                        # spawn locations too close
                        too_close = True
                        break
                if too_close:
                    continue

                # prevent starting food from being equidistant to ants
                if not starting or len(locations) == self.num_players:
                    # set locations to visited
                    for loc in locations:
                        visited[loc[0]][loc[1]] = True
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
        if self.remaining_players() <= 1:
            return True
        if self.cutoff_turns >= self.cutoff_turn:
            return True
        return False

    def kill_player(self, player):
        """ Used by engine to signal that a player is out of the game """
        self.killed[player] = True

    def start_game(self):
        """ Called by engine at the start of the game """
        if self.do_food != self.do_food_none:
            self.game_started = True
            starting_food = ((self.land_area // self.food_start)
                             - self.food_visible * self.num_players)
            self.do_food_visible(self.food_visible * self.num_players)
            self.do_food(starting_food)

    def finish_game(self):
        """ Called by engine at the end of the game """
        players = [p for p in range(self.num_players) if self.is_alive(p)]

        # if there is exactly one player remaining they get food bonus
        # if the game ends early there is no bonus
        #    the bonus only exists to ensure a lone survivor the best chance at winning
        #    simply removing the bonus is the best disincentive to not ending the game
        #    games ending do to inactivity would not have the player rank changed by splitting the bonus
        if len(players) == 1:
            player = players[0]
            # the food bonus represents the maximum points a bot can get with perfect play
            # remaining food and food to be spawned would be 1 point
            #   either from collecting the food and spawning an ant
            #   or killing an enemy ant that the food spawned into
            # plus 1 point for killing all enemy ants (losses don't matter for points)
            food_bonus = (
                # food that will spawn
                (self.turns - self.turn) *
                Fraction(self.food_rate * self.num_players, self.food_turn)
                + self.food_extra
                # food that hasn't been collected
                + len(self.current_food)
                # enemy ants (player ants already received point when spawned)
                + len([ant for ant in self.current_ants.values() if ant.owner != player])
            ) * self.kill_points
            self.score[player] += food_bonus
            # separate bonus from score history
            self.bonus[player] = food_bonus

    def start_turn(self):
        """ Called by engine at the start of the turn """
        self.turn += 1
        self.killed_ants = []
        self.revealed_water = [[] for _ in range(self.num_players)]
        self.removed_food = [[] for _ in range(self.num_players)]
        self.orders = [[] for _ in range(self.num_players)]

    def finish_turn(self):
        """ Called by engine at the end of the turn """

        # determine players alive at the start of the turn
        #  (only these players will be able to score this turn)
        was_alive = set(i for i in range(self.num_players) if self.is_alive(i))

        self.do_orders()
        self.do_attack()
        self.do_spawn()
        self.food_extra += Fraction(self.food_rate * self.num_players, self.food_turn)
        food_now = int(self.food_extra)
        left_over = self.do_food(food_now)
        self.food_extra -= (food_now - left_over)

        for i, s in enumerate(self.score):
            if i in was_alive:
                # update score for those were alive at the START of the turn
                self.score_history[i].append(s)
            else:
                # otherwise undo any changes to their score made
                #   during this turn
                self.score[i] = self.score_history[i][-1]

        # now that all the ants have moved we can update the vision
        self.update_vision()
        self.update_revealed()

        # calculate population counts for stopping games early
        # FOOD can end the game as well, since no one is gathering it
        pop_count = defaultdict(int)
        for ant in self.current_ants.values():
            pop_count[ant.owner] += 1
        pop_count[FOOD] = len(self.current_food)
        pop_total = sum(pop_count.values())
        for owner, count in pop_count.items():
            if (count >= pop_total * self.cutoff_percent
                    and self.score[owner] == max(self.score)):
                if self.cutoff_bot == owner:
                    self.cutoff_turns += 1
                else:
                    self.cutoff_bot = owner
                    self.cutoff_turns = 1
                break
        else:
            self.cutoff_bot = LAND
            self.cutoff_turns = 0

        scores = [int(score) for score in self.score]
        ranking_bots = [sorted(set(scores), reverse=True).index(x) for x in scores]
        if self.ranking_bots != ranking_bots:
            self.ranking_turn = self.turn
        self.ranking_bots = ranking_bots

        winning_bot = [p for p in range(len(scores)) if scores[p] == max(scores)]
        if self.winning_bot != winning_bot:
            self.winning_turn = self.turn
        self.winning_bot = winning_bot

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
        result.append(['player_seed', self.player_seed])
        # information hidden from players
        if player == None:
            result.append(['food_rate', self.food_rate])
            result.append(['food_turn', self.food_turn])
            result.append(['food_start', self.food_start])
            for line in self.get_map_output():
                result.append(['m',line])
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
        orders, valid, ignored, invalid = self.parse_orders(player, moves)
        orders, valid, ignored, invalid = self.validate_orders(player, orders, valid, ignored, invalid)
        self.orders[player] = orders
        return valid, ['%s # %s' % ignore for ignore in ignored], ['%s # %s' % error for error in invalid]

    def get_scores(self, player=None):
        """ Gets the scores of all players

            Used by engine for ranking
        """
        if player == None:
            return [int(score) for score in self.score]
        else:
            return self.order_for_player(player, map(int, self.score))

    def order_for_player(self, player, data):
        """ Orders a list of items for a players perspective of player #

            Used by engine for ending bot states
        """
        s = self.switch[player]
        return [None if not i in s else data[s.index(i)]
                for i in range(max(len(data),self.num_players))]

    def get_stats(self):
        """ Get current ant counts

            Used by engine to report stats
        """
        ant_count = [0 for _ in range(self.num_players+1)]
        for ant in self.current_ants.values():
            ant_count[ant.owner] += 1
        stats = {}
        stats['ant_count'] = ant_count
        stats['food'] = len(self.current_food)
        stats['cutoff'] = 'Food' if self.cutoff_bot == FOOD else '-' if self.cutoff_bot == LAND else self.cutoff_bot
        stats['c_turns'] = self.cutoff_turns
        stats['winning'] = self.winning_bot
        stats['w_turn'] = self.winning_turn
        stats['ranking_bots'] = self.ranking_bots
        stats['r_turn'] = self.ranking_turn
        stats['score'] = map(int, self.score)
        return stats

    def get_replay(self):
        """ Return a summary of the entire game

            Used by the engine to create a replay file which may be used
              to replay the game.
        """
        replay = {}
        # required params
        replay['revision'] = 2
        replay['players'] = self.num_players

        # optional params
        replay['loadtime'] = self.loadtime
        replay['turntime'] = self.turntime
        replay['turns'] = self.turns
        replay['viewradius2'] = self.viewradius
        replay['attackradius2'] = self.attackradius
        replay['spawnradius2'] = self.spawnradius
        replay['engine_seed'] = self.engine_seed
        replay['player_seed'] = self.player_seed
        replay['food_rate'] = self.food_rate
        replay['food_turn'] = self.food_turn
        replay['food_start'] = self.food_start

        # map
        replay['map'] = {}
        replay['map']['rows'] = self.height
        replay['map']['cols'] = self.width
        replay['map']['data'] = self.get_map_output()

        # food and ants combined
        replay['ants'] = []
        for food in self.all_food:
            ant_data = [ food.loc[0], food.loc[1], food.start_turn]
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

            replay['ants'].append(ant_data)

        # scores
        # score_history contains Fraction objects, so round down with int function
        replay['scores'] = [map(int, s) for s in self.score_history]
        replay['bonus'] = map(int, self.bonus)
        replay['winning_turn'] = self.winning_turn
        replay['ranking_turn'] = self.ranking_turn
        replay['cutoff'] = bool(self.remaining_players() > 1 and self.turns > self.turn)

        return replay

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

def test_symmetry():
    import sys
    import visualizer.visualize_locally
    if len(sys.argv) < 2:
        map_file_name = 'maps/test_maps/sym_test_2.map'
    else:
        map_file_name = sys.argv[1]
    with open(map_file_name, 'r') as f:
        options = {'map': f.read(),
                   'turns': 1,
                   'loadtime': 1000,
                   'turntime': 1000,
                   'viewradius2': 77,
                   'attackradius2': 5,
                   'spawnradius2': 1 }
    a = Ants(options)
    ors = a.get_map_symmetry()
    for o_count, ant_aims in enumerate(ors):
        print '=== orientation {0} '.format(o_count) + '=' * 30
        fix = []
        lines = ['' for _ in range(a.height)]
        for loc, aim, enemy_map in ant_aims:
            row, col = a.destination(loc, a.offset_aim((1,2), aim))
            fix.append(((row, col), a.map[row][col]))
            a.map[row][col] = FOOD
        for loc, aim, enemy_map in ant_aims:
            print aim, enemy_map, loc
            for row in range(a.height):
                lines[row] += ' '
                for col in range(a.width):
                    row1, col1 = a.destination(loc, a.offset_aim((row, col), aim))
                    lines[row] += MAP_RENDER[a.map[row1][col1]]
        #for line in lines:
        #    print line
        #print
        # write test file
        if len(sys.argv) > 2:
            test_map_name = map_file_name + ''.join([str(aim) for _, aim, __ in ant_aims]) + '.replay'
            with open(test_map_name, 'w') as f:
                f.write("players {0}\n".format(a.num_players))
                f.write("rows {0}\n".format(a.height))
                f.write("cols {0}\n".format(a.width))
                for row in range(a.height):
                    f.write("m ")
                    for col in range(a.width):
                        f.write(MAP_RENDER[a.map[row][col]])
                    f.write("\n")
            visualizer.visualize_locally.launch(test_map_name)
            for (row, col), ilk in reversed(fix):
                a.map[row][col] = ilk
if __name__ == '__main__':
    test_symmetry()

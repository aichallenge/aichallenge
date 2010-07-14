from sys import stdin, stdout

class StringUtils:
    @staticmethod
    def input():
        string = stdin.readline()
        return string

    @staticmethod
    def output(string):
        stdout.write(string + "\n")
        stdout.flush()

class Fleet:
    """
    Object representing an active fleet of ships.
    """
    _instances = ['owner', 'num_ships', 'source_planet', 'destination_planet',
                  'total_trip_length', 'turns_remaining']

    def __init__(self, **kwds):
        for name in self._instances:
            setattr(self, name, kwds[name])

    def remove_ships(self, amount):
        """
        Removes given amount ships from the fleet object.
        """
        self.num_ships -= (amount <= self.num_ships) and amount or self.num_ships

    def time_step(self):
        """
        Decrements the number of turns by 1, not below 0.
        """
        self.turns_remaining -= 1 % (self.turns_remaining + 1)

    # TODO comparison operator?

class Planet:
    """
    Object representing a planet.
    """
    _instances = ['owner', 'num_ships', 'growth_rate', 'x', 'y']

    def __init__(self, **kwds):
        for name in self._instances:
            setattr(self, name, kwds[name])

    def add_ships(self, amount):
        """
        Adds given amount of ships to the planet.
        """
        self.num_ships += amount

    def remove_ships(self, amount):
        """
        Removes given amount of ships from the planet.
        """
        self.num_ships -= (amount <= self.num_ships) and amount or self.num_ships

class PlanetWars:
    def __init__(self, game_state):
        self.planets = []
        self.fleets = []

        # Parse the game state string
        for line in game_state.split("\n"):
            line = line.split("#")[0]
            components = line.split(" ")

            # Handle planets
            if components[0] == "P" and len(components) == 6:
                p = Planet(x = float(components[1]),
                           y = float(components[2]),
                           owner = float(components[3]),
                           num_ships = float(components[4]),
                           growth_rate = float(components[5]))
                self.planets.append(p)

            #Handle fleets
            elif components[0] == "F" and len(components) == 7:
                f = Fleet(owner = components[1],
                          num_ships = components[2],
                          source_planet = components[3],
                          destination_planet = components[4],
                          trip_trip_length = components[5],
                          turns_remaining = components[6])
                self.fleets.append(f)

    def distance(self, source_planet, destination_planet):
        dx = planets[source_planet].x - planets[destination_planet].x
        dy = planets[source_planet].y - planets[destination_planet].y
        return int(math.ceil(math.sqrt(dx ** 2 + dy ** 2)))

    def issue_order(self, source_planet, destination_planet, num_ships):
        StringUtils.output("%s %s %s" %
                          (source_planet, destination_planet, num_ships))

    def end_turn(self):
        StringUtils.output("go")

    def is_alive(self, player_id):
        alive = False
        for planet in planets:
            if planet.owner == player_id:
                alive = True

        for fleet in fleets:
            if fleet.owner == player_id:
                alive = True

        return alive

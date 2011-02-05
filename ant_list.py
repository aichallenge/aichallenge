class AntList:
    def __init__(self):
        self.ants = []

    def add(self, ant):
        self.ants.append(ant)

    def nearby_ants(self, x, y, r, filter=[0]):
        r *= r
        nearby = []
        for ant in self.ants:
            if pow(ant.x - x, 2) + pow(ant.y - y, 2) <= r:
                if (not (ant.owner in filter)):
                    nearby.append(ant)
        return nearby

    def __iter__(self):
        def iter():
            for ant in self.ants:
               yield ant
        return iter()

    def __len__(self):
        return len(self.ants)

    def __getitem__(self, key):
        return self.ants[key]

    def get_by_location(self, x, y):
        for ant in self.ants:
            if ant.x == x and ant.y == y:
                return ant
        return None

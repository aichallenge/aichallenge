

class AntList:
  def __init__(self):
    self.ants = []

  def add(self, ant):
    self.ants.append(ant)

  def nearby_ants(self, x, y, r):
    nearby = []
    for ant in self.ants:
      if abs(ant.x - x) + abs(ant.y - y) <= r:
        nearby.append(ant)
    return nearby

  def __iter__(self):
    self.current_ant = 0
    return self

  def __len__(self):
    return len(self.ants)

  def __getitem__(self, key):
    return self.ants[key]

  def next(self):
    if self.current_ant >= len(self.ants):
      raise StopIteration
    else:
      self.current_ant += 1
      return self.ants[self.current_ant - 1]

  def get_by_location(self, x, y):
    for ant in self.ants:
      if ant.x == x and ant.y == y:
        return ant
    return None

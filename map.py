import math

from ant import *
from ant_list import *
import Image
import random

class Map:
  def __init__(self, filename):
    self.passable = []
    self.ants = AntList()
    image = Image.open(filename)
    (self.width, self.height) = image.size
    self.num_players = 0
    self.land_area = 0
    self.water_area = 0
    for x in range(self.width):
      self.passable.append([])
      for y in range(self.height):
        pixel = image.getpixel((x, y))
        if pixel[0] > 0:
          self.passable[x].append(True)
          self.num_players += 1
          self.ants.add(Ant(x, y, self.num_players))
          self.land_area += 1
        elif pixel[1] > 0:
          self.passable[x].append(True)
          self.land_area += 1
        elif pixel[2] > 0:
          self.passable[x].append(False)
          self.water_area += 1

  def randomly_place_food(self, amount=1):
    for i in range(amount):
      for j in range(10):
        x = random.randrange(self.width)
        y = random.randrange(self.height)
        if self.passable[x][y] and self.ants.get_by_location(x, y) is None:
          self.ants.add(Ant(x, y, 0))
          break

  def render(self):
    water_color = (0, 0, 255)
    land_color = (0, 255, 0)
    player_colors = [
      (139, 69, 19),
      (255, 0, 0),
      (255, 255, 255),
      (0, 0, 0),
      (255, 255, 0),
      (255, 0, 255),
      (255, 127, 0)
    ]
    image = Image.new("RGB", ((self.width, self.height)))
    for x in range(self.width):
      for y in range(self.height):
        ant = self.ants.get_by_location(x, y)
        if ant is not None:
          image.putpixel((x, y), player_colors[ant.owner])
        elif self.passable[x][y]:
          image.putpixel((x, y), land_color)
        else:
          image.putpixel((x, y), water_color)
    return image

  def remaining_players(self):
    return set([a.owner for a in self.ants])

  def game_over(self):
    return len(self.remaining_players()) <= 1

  def do_order(self, order):
    x1, y1, x2, y2 = order
    ant = self.ants.get_by_location(x1, y1)
    if ant is None:
      return
    defender = self.ants.get_by_location(x2, y2)
    if defender is not None:
      return
    ant.x, ant.y = x2, y2

  def do_turn(self):
    for i in range(len(self.ants)):
      for j in range(i + 1, len(self.ants)):
        if i >= len(self.ants) or j >= len(self.ants):
          continue
        a = self.ants[i]
        b = self.ants[j]
        dist = math.sqrt((a.x - b.x) ** 2 + (a.y - b.y) ** 2)
        if dist <= 3.000001:
          if a.owner == 0 and b.owner == 0:
            pass
          elif a.owner == b.owner:
            pass
          elif a.owner == 0:
            a.owner = b.owner
          elif b.owner == 0:
            b.owner = a.owner
          else:
            self.ants.ants.pop(j)
            self.ants.ants.pop(i)
    self.randomly_place_food(1)

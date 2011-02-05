import math

from ant import *
from ant_list import *
import Image
import random
from copy import deepcopy

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
                if (self.passable[x][y] 
                    and self.ants.get_by_location(x, y) is None):
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
            (255, 127, 0),
            (0, 192, 192),
            (128, 128, 128)
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
        return set([a.owner for a in self.ants if a.owner != 0])

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

    def do_birth(self):
        ants = deepcopy(self.ants)
        for a in range(len(self.ants)):
            ant = self.ants[a]
            if ant.owner == 0:
                near_ants = self.ants.nearby_ants(ant.x, ant.y, 1.6)
                support = [0,0,0,0,0,0,0,0,0]
                for near_ant in near_ants:
                    support[near_ant.owner] += 1
                max_support = max(support)
                if max_support > 0:
                    if support.count(max_support) == 1:
                        ants.ants[a].owner = support.index(max_support)
        self.ants = ants

    def do_death(self):
        ants = deepcopy(self.ants)
        for a in range(len(self.ants)):
            ant = self.ants[a]
            if ant.owner != 0:
                near_ants = self.ants.nearby_ants(ant.x, ant.y, 1.6)
                support = 0
                for near_ant in near_ants:
                    if near_ant.owner == ant.owner:
                        support += 1
                    elif near_ant.owner != 0:
                        support -= 1
                if support <= 0:
                    ants.ants[a].owner = -1
        for i in range(len(ants)-1,-1,-1):
            if ants.ants[i].owner == -1:
                ants.ants.pop(i)
        self.ants = ants

    def do_death_2(self):
        ants = deepcopy(self.ants)
        for a in range(len(self.ants)):
            ant = self.ants[a]
            if ant.owner != 0:
                enemy_ants = self.ants.nearby_ants(ant.x, ant.y, 1.6, [0,ant.owner])
                for enemy_ant in enemy_ants:
                    distracting_ants = self.ants.nearby_ants(enemy_ant.x, 
                                                             enemy_ant.y, 
                                                             1.6, 
                                                             [0, enemy_ant.owner])
                    if len(distracting_ants) <= len(enemy_ants):
                        ants.ants[a].owner = -1
                        break
        for i in range(len(ants)-1,-1,-1):
            if ants.ants[i].owner == -1:
                ants.ants.pop(i)
        self.ants = ants

    def do_turn(self):
        self.do_death()
        self.do_birth()
        self.randomly_place_food()

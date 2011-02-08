import math
import random
from ant import Ant

from player import Player

class HunterBot(Player):
    def do_turn(self, m):
        orders = []
        my_ants = [Ant(*a) for a in m.ants() if a[2] == 1]
        for ant in my_ants:
            if ant.owner != 1:
                continue
            x, y = ant.x, ant.y
            candidates = [
                (x, y, x + 1, y),
                (x, y, x - 1, y),
                (x, y, x, y + 1),
                (x, y, x, y - 1)
                ]
            closest_enemy = None
            closest_distance = 999999.0
            other_ants = [Ant(*a) for a in m.ants()] + [Ant(a[0],a[1],0) for a in m.food()]
            for other_ant in other_ants:
                if other_ant.owner == 1:
                    continue
                dx = ant.x - other_ant.x
                dy = ant.y - other_ant.y
                dist = math.sqrt(dx ** 2 + dy ** 2)
                if dist < closest_distance:
                    closest_distance = dist
                    closest_enemy = other_ant
            if closest_enemy == None:
                continue
            best_move = None
            closest_distance = 999999.0
            for c in candidates:
                if c[2] < 0 or c[3] < 0:
                    continue
                if c[2] >= m.width or c[3] >= m.height:
                    continue
                if m.map[c[2]][c[3]] in (m.WALL, m.HILL, m.FOOD):
                    continue
                occupied = False
                for order in orders:
                    if c[2] == order[2] and c[3] == order[3]:
                        occupied = True
                if occupied:
                    continue
                dx = c[2] - closest_enemy.x
                dy = c[3] - closest_enemy.y
                dist = math.sqrt(dx ** 2 + dy ** 2)
                if dist < closest_distance:
                    closest_distance = dist
                    best_move = c
            if best_move is not None:
                orders.append(best_move)
        # TODO: crappy fix, should rewrite bot
        direction_orders = []
        for order in orders:
            if order[2] > order[0]:
                direction_orders.append((order[1],order[0],'E'))
            elif order[2] < order[0]:
                direction_orders.append((order[1],order[0],'W'))
            elif order[3] > order[1]:
                direction_orders.append((order[1],order[0],'N'))
            elif order[3] < order[1]:
                direction_orders.append((order[1],order[0],'S'))
        return direction_orders

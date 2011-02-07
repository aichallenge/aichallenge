import math
import random

from player import Player

class HunterBot(Player):
    def do_turn(self, m):
        orders = []
        my_ants = [a for a in m.ants if a.owner == 1]
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
            for other_ant in m.ants:
                if other_ant.owner == 1:
                    continue
                dx = ant.x - other_ant.x
                dy = ant.y - other_ant.y
                dist = math.sqrt(dx ** 2 + dy ** 2)
                if dist < closest_distance:
                    closest_distance = dist
                    closest_enemy = other_ant
            best_move = None
            closest_distance = 999999.0
            for c in candidates:
                if c[2] < 0 or c[3] < 0:
                    continue
                if c[2] >= m.width or c[3] >= m.height:
                    continue
                if not m.passable[c[2]][c[3]]:
                    continue
                dx = c[2] - closest_enemy.x
                dy = c[3] - closest_enemy.y
                dist = math.sqrt(dx ** 2 + dy ** 2)
                if dist < closest_distance:
                    closest_distance = dist
                    best_move = c
            if best_move is not None:
                orders.append(best_move)
        return orders

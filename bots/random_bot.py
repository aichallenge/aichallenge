import random

from player import Player

class RandomBot(Player):
    def do_turn(self, m):
        orders = []
        for ant in m.ants:
            if ant.owner != 1:
                continue
            x, y = ant.x, ant.y
            candidates = [
                (x, y, x + 1, y),
                (x, y, x - 1, y),
                (x, y, x, y + 1),
                (x, y, x, y - 1)
                ]
            possible = []
            for c in candidates:
                if c[2] < 0 or c[3] < 0:
                    continue
                if c[2] >= m.width or c[3] >= m.height:
                    continue
                if not m.passable[c[2]][c[3]]:
                    continue
                possible.append(c)
            if len(possible) > 0:
                orders.append(random.choice(possible))
        return orders

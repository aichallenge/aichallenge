#!/usr/bin/env python
from trueskill import trueskill

class Player(object):
    def __init__(self, name, skill, rank):
        self.name = name
        self.old_skill = skill
        self.skill = skill
        self.rank = rank
    def __str__(self):
        return ('id=%5d rank=%1d\n\t   mu=%8.5f->%8.5f,\n\tsigma=%8.5f->%8.5f' %
                (self.name, self.rank, self.old_skill[0], self.skill[0], self.old_skill[1], self.skill[1]))

def test_trueskill():
    # get list of players and their mu/sigma values from the database
    players = [Player(0, (41.0538, 1.6888), 1),
               Player(1, (31.6869, 1.70811), 2),
               Player(2, (28.0252, 1.74717), 2),
               Player(3, (27.0053, 1.83862), 2)]
    
    trueskill.AdjustPlayers(players)

    print('\nAfter:')
    for player in players:
        print(player)

if __name__ == '__main__':
    test_trueskill()
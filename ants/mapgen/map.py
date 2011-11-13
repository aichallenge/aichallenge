#!/usr/bin/env python

import collections

from terrain import *

class Map(Terrain):
    def __init__(self, **kwargs):
        Terrain.__init__(self, **kwargs)
        
        #A list of players, each player being a set of hills
        self.players=[set() for player in xrange(kwargs["num_players"])]

    def render(self):
        string ="rows %s\n" % self.size.y
        string+="cols %s\n" % self.size.x
        string+="players %s\n" % len(self.players)
        
        for y in xrange(self.size.y):
            string+="m "
            for x in xrange(self.size.x):
                character=self[Point(x,y)]
                
                #Check if there's a hill
                for player,hills in enumerate(self.players):
                    if Point(x,y) in hills:
                        character=str(player)
                
                string+=character
            string+="\n"
        return string[:-1]

if __name__=="__main__":
    print Map(size=Point(10,10),num_players=3)
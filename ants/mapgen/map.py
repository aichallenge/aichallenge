#!/usr/bin/env python

import collections

from terrain import *

class Map(Terrain):
    def __init__(self, **kwargs):
        Terrain.__init__(self, **kwargs)
        
        #A list of players, each player being a set of hills
        self.players=[set() for player in xrange(kwargs["num_players"])]
    
    def add_hill(self,player,location):
        """Adds a hill to the map, and clears the immediate area"""
        location=location.normalize(self.size)
        
        #check if there's already a hill there
        if location in self.hills():
            raise Exception("Already a hill at %s" % (location,))
        
        #Actually add the hill
        self.players[player].add(location)
        
        #clear an area
        clearsize=Point(3,3)
        offset=clearsize*0.5
        for point in clearsize.upto():
            point=(point-offset)+location
            self[point]=LAND
    
    def hills(self):
        """Returns a set of all hills(locations) on the map"""
        return reduce(lambda x,y: x|y, self.players)

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
    map=Map(size=Point(10,10),num_players=3,defaultterrain=WATER)
    map.addbase(0,Point(1,1))
    print map
    print "Hills", map.hills()
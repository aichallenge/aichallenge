#!/usr/bin/env python

from symmetricmap import *

class Cavemap(SymmetricMap):
    def __init__(self, size, num_players, symmetry_type, translation_factor=Point(1,3)):
        SymmetricMap.__init__(self, size, num_players, symmetry_type, translation_factor)
    
    def add_water_randomly(self,percent=0.49):
        for point in self.size.upto():
            if random.random() < percent:
                self[point]=WATER
    
    def smooth(self, times=10):
        """Apply a cellular automaton to smoothen the walls"""
        for time in xrange(times):
            oldmap=self.copy()
            
            for point in self.size.upto():
                neighbour_water=[d for d in diag_directions.values() if oldmap[point+d]==WATER]
                
                if len(neighbour_water)<3:
                    self[point]=LAND
                if len(neighbour_water)>4:
                    self[point]=WATER

if __name__=="__main__":
    import random
    #random.seed(2)
    
    map=Cavemap(Point(40,40),4,"translational")
    map.add_water_randomly(0.5/4)
    map.smooth()
    
    for player,location in enumerate(map.symmetry_vector(Point(1,1))):
        map.players[player].add(location)
    
    print map
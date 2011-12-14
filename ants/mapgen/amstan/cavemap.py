#!/usr/bin/env python

import random
from symmetricmap import *

class Cavemap(SymmetricMap):
    def __init__(self, **kwargs):
        kwargs["defaultterrain"]=WATER
        SymmetricMap.__init__(self, **kwargs)
    
    def add_water_randomly(self,percent=0.49):
        for point in self.size.upto():
            self[point]=LAND
            if random.random() < percent:
                self[point]=WATER
    
    def random_walk(self,start,cover=0.5):
        total_squares=self.size.x*self.size.y
        squares_water=0
        symmetric_locations=len(self.symmetry_vector(Point(0,0)))
        
        location=start
        end_reached=False
        
        while squares_water<total_squares*cover:
            if self[location]==WATER:
                self[location]=LAND
                squares_water+=symmetric_locations
            
            location+=random.choice(directions.values())
    
    def smooth(self, times=1):
        """Apply a cellular automaton to smoothen the walls"""
        for time in xrange(times):
            oldmap=self.copy()
            
            for point in self.size.upto():
                neighbour_water=[d for d in diag_directions.values() if oldmap[point+d]==WATER]
                
                if len(neighbour_water)<4:
                    self[point]=LAND
                if len(neighbour_water)>4:
                    self[point]=WATER
    
    def generate(self,**kwargs):
        self.random_walk(list(self.hills())[0])
        
        try:
            self.smooth(kwargs["smooth"])
        except KeyError:
            self.smooth(4)

if __name__=="__main__":
    #random.seed(6)
    
    size=Point(60,60)
    playerone=size.random_upto()*(0.5/3)+size*(0.5/3)
    
    map=Cavemap(size=size,num_players=4,symmetry="translational")
    
    map.add_hill(playerone)
    map.generate()
    
    print map
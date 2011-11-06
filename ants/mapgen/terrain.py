#!/usr/bin/env python

import random
from util import *

directions = {'N': Point(-1,0), 'S': Point(1,0), 'E': Point(0,1), 'W': Point(0,-1)}
diag_directions = {'NW': Point(-1,-1), 'SW': Point(1,-1), 'NE': Point(-1,1), 'SW': Point(1,-1)}
diag_directions.update(directions)

WATER = "%"
LAND = "."

class Terrain(object):
    """Terrain class contains only map size and terrain data.
    It does not concern itself with players and symmetries"""
    
    def __init__(self, size):
        self.size=size
        self.terrain=[[LAND for x in xrange(size.x)] for y in xrange(size.y)]
        
    def __getitem__(self,point):
        """Gets a point in the terrain"""
        point=point.normalize(self.size)
        return self.terrain[point.y][point.x]
    
    def __setitem__(self,point,value):
        """Sets a point in the terrain"""
        point=point.normalize(self.size)
        self.terrain[point.y][point.x]=value
    
    def add_water_randomly(self,percent=0.49):
        for point in self.size.upto():
            if random.random() < percent:
                self[point]=WATER
    
    def smooth(self):
        """Apply a cellular automaton to smoothen the walls"""
        oldmap=self.copy()
        
        for point in self.size.upto():
            neighbour_water=[d for d in diag_directions.values() if oldmap[point+d]==WATER]
            
            if len(neighbour_water)<3:
                self[point]=LAND
            if len(neighbour_water)>4:
                self[point]=WATER
        
    def copy(self):
        """Makes a copy of this map"""
        newterrain=Terrain(self.size)
        for point in self.size.upto():
            newterrain[point]=self[point]
        return newterrain
    
    def render(self):
        string ="rows %s\n" % self.size.y
        string+="cols %s\n" % self.size.x
        
        for y in xrange(self.size.y):
            string+="m "
            for x in xrange(self.size.x):
                string+=self[Point(x,y)]
            string+="\n"
        
        return string[:-1]
    
    def __str__(self):
        return self.render()

if __name__=="__main__":
    terrain=Terrain(Point(10,10))
    terrain[Point(-1,-1)]=WATER
    print terrain
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
    
    def __init__(self, **kwargs):
        self.size=kwargs["size"]
        
        try:
            defaultterrain=kwargs["defaultterrain"]
        except KeyError:
            defaultterrain=LAND
        self.terrain=[[defaultterrain for x in xrange(self.size.x)] for y in xrange(self.size.y)]
        
    def __getitem__(self,point):
        """Gets a point in the terrain"""
        point=point.normalize(self.size)
        return self.terrain[point.y][point.x]
    
    def __setitem__(self,point,value):
        """Sets a point in the terrain"""
        point=point.normalize(self.size)
        self.terrain[point.y][point.x]=value
    
    def copy(self):
        """Makes a copy of this map"""
        newterrain=Terrain(size=self.size)
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
    terrain=Terrain(size=Point(10,10))
    terrain[Point(-1,-1)]=WATER
    print terrain
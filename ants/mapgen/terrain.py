#!/usr/bin/env python

import collections

directions = {'N': (-1,0), 'S': (1,0), 'E': (0,1), 'W': (0,-1)}
diag_directions = {'NW': (-1,-1), 'SW': (1,-1), 'NE': (-1,1), 'SW': (1,-1)}
diag_directions.update(directions)

WATER = "%"
LAND = "."

class Point(collections.namedtuple('Point', ['x', 'y'])):
    def upto(self):
        """Iterates over all points from origin to Point"""
        for y in xrange(self.y):
            for x in xrange(self.x):
                yield Point(x,y)
    
    def normalize(self,size):
        """Returns a normalized point from a toroid(with a given size)
        Point(-9,-9).normalize(Point(10,10)) => Point(1,1)"""
        return Point(self.x%size.x,self.y%size.y)
    
    def __add__(self,other):
        return Point(self.x+other.x,self.y+other.y)
    
    def __sub__(self,other):
        return Point(self.x-other.x,self.y-other.y)
    
    def __mul__(self,number):
        return Point(self.x*number.x,self.y*number)

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
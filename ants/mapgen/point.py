#!/usr/bin/env python

import collections

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
        return Point(self.x*number,self.y*number)
#!/usr/bin/env python

import collections
import random

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
        return Point(int(self.x*number),int(self.y*number))
    
    def random_upto(self):
        """Returns a random point less than self"""
        return Point(random.randint(0,self.x),random.randint(0,self.y))

class Range(collections.namedtuple('Range', ['min','max'])):
    def __contains__(self,what):
        return what>=self.min and what<=self.max
    
    def randint(self):
        """Picks a randint inside the range and remembers it, next time being called it returns the same"""
        try:
            return self.value
        except AttributeError:
            self.value=random.randint(self.min,self.max)
            return self.value
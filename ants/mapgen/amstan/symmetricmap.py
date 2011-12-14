#!/usr/bin/env python

from map import *

class SymmetryException(Exception):
    def __init__(self, msg):
        Exception.__init__(self, msg)
        self.msg = msg
    def __str__(self):
        return "This symmetry type only supports %s." % self.msg

class SymmetricMap(Map):
    def __init__(self, **kwargs):
        Map.__init__(self, **kwargs)
        
        #setup symmetry
        symmetry_type=kwargs["symmetry"]
        if symmetry_type not in symmetry_types:
            raise Exception("Unknown symmetry type %s" % symmetry_type)
        self.symmetry_vector=self.__getattribute__("vector_"+symmetry_type)
        
        #setup translational symmetry
        num_players=kwargs["num_players"]
        try:
            self.translation_factor=kwargs["translation_factor"]
        except KeyError:
            self.translation_factor=Point(num_players-1,num_players+1)
            
        if symmetry_type=="translational":
            if (self.size.x%num_players!=0) or (self.size.y%num_players!=0):
                raise SymmetryException("map size divisible to the number of players")
            self.translation=Point(self.size.x/num_players*self.translation_factor.x,self.size.y/num_players*self.translation_factor.y)
        
        #test the symmetry
        assert(len(self.symmetry_vector(Point(0,0)))>0)
    
    def __setitem__(self,point,value):
        """Sets a point in the terrain, applies to all other points that are symmetric"""
        for symmetric_point in self.symmetry_vector(point):
            Terrain.__setitem__(self, symmetric_point, value)
    
    def add_hill(self,location):
        """Adds a hill at specified location, adds enemy hills to all other symmetric locations too."""
        for player_id,location in enumerate(self.symmetry_vector(location)):
            Map.add_hill(self, player_id, location)
    
    #vector functions given a Point will return a list of all the points that are symmetric including themselves
    def symmetry_vector(self,origin):
        """To be overridden later"""
        pass
    
    def vector_horizontal(self,origin):
        if len(self.players)!=2:
            raise SymmetryException("2 players")
        return [origin,Point(origin.x, self.size.y-origin.y-1)]
    
    def vector_diagonal(self,origin):
        if len(self.players)!=2:
            raise SymmetryException("2 players")
        if self.size.x!=self.size.y:
            raise SymmetryException("square maps")
        return [origin,Point(self.size.x-origin.y-1, self.size.y-origin.x-1)]
    
    def vector_point(self,origin):
        if len(self.players) not in [2,4,8]:
            raise SymmetryException("2, 4 or 8 players")
        
        points = [origin]
        
        if len(self.players)>=2:
            points.append(Point(self.size.x-origin.x-1, self.size.y-origin.y-1)) #vertical/horizontal(point) reflection
        
        if len(self.players)>=4:
            if self.size.x!=self.size.y:
                raise SymmetryException("square maps")
            points.append(Point(self.size.x-origin.x-1, origin.y))               #vertical reflection
            points.append(Point(origin.x, self.size.y-origin.y-1))               #horizontal reflection
        
        if len(self.players)>=8:
            points.append(Point(self.size.x-origin.y-1, self.size.y-origin.x-1)) #diagonal reflection to origin
            points.append(Point(origin.y, origin.x))                             #diagonal reflection to point reflection
            points.append(Point(origin.y, self.size.y-origin.x-1))               #diagonal reflection to vertical reflection
            points.append(Point(self.size.x-origin.y-1, origin.x))               #diagonal reflection to horizontal reflection
        
        return points
    
        
    def vector_rotational(self,origin):
        if len(self.players) not in [2,4]:
            raise SymmetryException("2 or 4 players")
        
        points = [origin]
        
        if len(self.players)>=2:
            points.append(Point(self.size.x-origin.x-1, self.size.y-origin.y-1)) #vertical/horizontal(point) reflection
        
        if len(self.players)>=4:
            if self.size.x!=self.size.y:
                raise SymmetryException("square maps")
            points.append(Point(self.size.x-origin.y-1, origin.x))
            points.append(Point(origin.y, self.size.y-origin.x-1))
        
        return points
    
    def vector_translational(self,origin):
        return [(origin+self.translation*playerid).normalize(self.size)
                for playerid in xrange(len(self.players))]
    
#Generate symmetry types based on the function names
symmetry_types=set(function[len("vector_"):] for function in dir(SymmetricMap) if function.startswith("vector_"))

if __name__=="__main__":
    for symmetry in symmetry_types:
        #title
        print symmetry
        print "="*len(symmetry)
        
        try:
            map=SymmetricMap(size=Point(20,20),num_players=4,symmetry=symmetry)
            
            #add a water spot
            map[Point(1,1)]=WATER
            
            #add player hills
            map.add_hill(Point(2,6))
            
            print map
        except SymmetryException as e:
            print e
        
        #newline
        print
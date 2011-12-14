#!/usr/bin/env python

from symmetricmap import *
from cavemap import *
import random

def mapgen(mapsizex, mapsizey, carver, symmetry, players, hills, seed):
    random.seed(seed)
    
    map=Cavemap(size=Point(mapsizex, mapsizey),num_players=players,symmetry=symmetry)
    
    #Decide where to place the hills
    for hillid in xrange(hills):
        player0hill=map.size.random_upto()
        map.add_hill(player0hill)
    
    map.generate()

    print map

if __name__=="__main__":
    import optparse, sys
    
    parser = optparse.OptionParser(usage="""Usage: %prog [options]\n""")
    
    parser.add_option("--cols", dest="mapsizex", type="int", default=60, help="Number of cols, aka x")
    parser.add_option("--rows", dest="mapsizey", type="int", default=60, help="Number of rows, aka y")
    parser.add_option("--carver", dest="carver", type="str", default=0, help="Carver type")
    parser.add_option("--symmetry", dest="symmetry", type="str", default="translational", help="Symmetry types (%s)" % ', '.join(symmetry_types))
    
    parser.add_option("--players", dest="players", type="int", default=4, help="Number of players")
    parser.add_option("--hills", dest="hills", type="int", default=2, help="Number of hills per player")
    parser.add_option("--seed", dest="seed", type="int", default=None, help="Seed for the random number generator")
    
    opts,_ = parser.parse_args(sys.argv)
        
    mapgen(**eval(str(opts)))
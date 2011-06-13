#!/usr/bin/env python
from random import shuffle
from ants import *
import logging
import sys
from optparse import OptionParser
from logutils import initLogging,getLogger

turn_number = 0
bot_version = 'v0.1'

class LogFilter(logging.Filter):
  """
  This is a filter that injects stuff like TurnNumber into the log
  """
  def filter(self,record):
    global turn_number,bot_version
    record.turn_number = turn_number
    record.version = bot_version
    return True

class GreedyBot:
    def __init__(self):
        """Add our log filter so that botversion and turn number are output correctly"""        
        log_filter  = LogFilter()
        getLogger().addFilter(log_filter)
        self.visited = [] #keep track of visited row/cols
        
    def hunt_ants(self,ants,destinations,a_row,a_col):
        getLogger().debug("Start Finding Ant")
        closest_enemy_ant = ants.closest_enemy_ant(a_row,a_col)
        getLogger().debug("Done Finding Ant")            
        moved =False
        if closest_enemy_ant!=None:
            getLogger().debug("chasing ant:start")
            directions = ants.direction(a_row,a_col,closest_enemy_ant[0],closest_enemy_ant[1])
            getLogger().debug("chasing ant:directions:%s","".join(directions))
            shuffle(directions)            
            for direction in directions:
                getLogger().debug("chasing ant:direction:%s",direction)
                (n_row,n_col) = ants.destination(a_row,a_col,direction)
                if (not (n_row,n_col) in destinations and
                    ants.unoccupied(n_row,n_col)):
                    moved=True
                    ants.issue_order((a_row,a_col,direction))
                    getLogger().debug("issue_order:%s,%d,%d,%s","chasing ant",a_row,a_col,direction)                        
                    destinations.append((n_row,n_col))
                    break
        return moved

    def hunt_food(self,ants,destinations,a_row,a_col):
        getLogger().debug("Start Finding Food")
        closest_food = ants.closest_food(a_row,a_col)
        getLogger().debug("Done Finding Food")            
        moved =False
        if closest_food!=None:
            getLogger().debug("chasing food:start")
            directions = ants.direction(a_row,a_col,closest_food[0],closest_food[1])
            getLogger().debug("chasing food:directions:%s","".join(directions))
            shuffle(directions)
            for direction in directions:
                getLogger().debug("chasing food:direction:%s",direction)
                (n_row,n_col) = ants.destination(a_row,a_col,direction)
                if (not (n_row,n_col) in destinations and
                    ants.unoccupied(n_row,n_col)):
                    moved=True
                    ants.issue_order((a_row,a_col,direction))
                    getLogger().debug("issue_order:%s,%d,%d,%s","chasing food",a_row,a_col,direction)                        
                    destinations.append((n_row,n_col))
                    break
        return moved

    def random_move(self,ants,destinations,a_row,a_col,do_visited=False):
        #if we didn't move as there was no food try a random move
        moved=False
        directions = AIM.keys()
        getLogger().debug("random move:directions:%s","".join(directions))                
        shuffle(directions)
        getLogger().debug("random move:shuffled directions:%s","".join(directions))
        for direction in directions:
            getLogger().debug("random move:direction:%s",direction)
            (n_row, n_col) = ants.destination(a_row, a_col, direction)
            if (not (n_row, n_col) in destinations and
                ants.unoccupied(n_row, n_col)):
                if (n_row,n_col) not in self.visited or (do_visited and (n_row,n_col) in self.visited):
                    moved=True
                    ants.issue_order((a_row, a_col, direction))
                    getLogger().debug("issue_order:%s,%d,%d,%s","random move",a_row,a_col,direction)
                    destinations.append((n_row, n_col))
                    break
                else:
                    continue
        return moved
        
    def do_turn(self, ants):
        global turn_number
        turn_number = turn_number+1
        destinations = []
        getLogger().debug("Starting Turn")
        for a_row, a_col in ants.my_ants():
            if not self.hunt_food(ants,destinations,a_row,a_col):
                if not self.hunt_ants(ants,destinations,a_row,a_col):
                    if not self.random_move(ants,destinations,a_row,a_col):
                        if not self.random_move(ants,destinations,a_row,a_col,True):
                            getLogger().debug("blocked:can't move:%d,%d",a_row,a_col)
                            destinations.append((a_row,a_col))
                    
if __name__ == '__main__':
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass
    try:
        Ants.run(GreedyBot())
    except KeyboardInterrupt:
        print('ctrl-c, leaving ...')

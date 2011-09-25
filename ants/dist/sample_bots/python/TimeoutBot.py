#!/usr/bin/env python
import time
from ants import *
import sys

class TimeoutBot:
    def __init__(self):
        self.gander = ['duck', 'duck', 'goose']
    def do_turn(self, ants):
        if self.gander.pop(0) == 'goose':
            sys.stderr.write("Cooking my goose...\n")
            time.sleep((ants.turntime * 2)/1000)

if __name__ == '__main__':
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass
    try:
        Ants.run(TimeoutBot())
    except KeyboardInterrupt:
        print('ctrl-c, leaving ...')

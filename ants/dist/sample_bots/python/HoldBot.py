#!/usr/bin/env python
from ants import *

class HoldBot:
    def do_turn(self, ants):
        pass

if __name__ == '__main__':
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass
    try:
        Ants.run(HoldBot())
    except KeyboardInterrupt:
        print('ctrl-c, leaving ...')

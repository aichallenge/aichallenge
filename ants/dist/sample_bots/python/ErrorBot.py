#!/usr/bin/env python
from random import choice
from ants import *

class ErrorBot:
    def do_turn(self, ants):
        if choice(('duck', 'duck', 'goose')) == 'goose':
            raise Exception('ErrorBot produces error now')

if __name__ == '__main__':
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass
    try:
        Ants.run(ErrorBot())
    except KeyboardInterrupt:
        print('ctrl-c, leaving ...')

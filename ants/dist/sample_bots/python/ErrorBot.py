#!/usr/bin/env python
from ants import *

class ErrorBot:
    def __init__(self):
        self.gander = ['duck', 'duck', 'duck', 'duck', 'goose']
    def do_turn(self, ants):
        if self.gander.pop(0) == 'goose':
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

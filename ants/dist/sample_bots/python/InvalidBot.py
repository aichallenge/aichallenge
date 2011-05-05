#!/usr/bin/env python
from random import choice
from ants import *

class InvalidBot:
    def __init__(self):
        self.gander = ['duck', 'duck', 'goose']
    def do_turn(self, ants):
        if choice(self.gander) == 'goose':
            ants.issue_order((-1, -1, 'h'))
        else:
            self.gander.append('goose')

if __name__ == '__main__':
    try:
        import psyco
        psyco.full()
    except ImportError:
        pass
    try:
        Ants.run(InvalidBot())
    except KeyboardInterrupt:
        print('ctrl-c, leaving ...')

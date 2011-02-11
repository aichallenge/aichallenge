#!/usr/bin/env python

class Game:
    def __init__(self):
        pass

    def load_map(self, filename):
        pass

    # common functions for all games used by engine
    def start_game(self):
        pass

    def start_turn(self):
        pass

    def finish_turn(self):
        pass

    def finish_game(self):
        pass

    def kill_player(self, player):
        pass

    def is_alive(self, player):
        pass

    def game_over(self): # returns boolean
        pass

    # used by engine to record full game
    def get_state(self):
        pass

    # used for turn 0, sending minimal info for bot to load
    def get_player_start(self, player):
        pass

    # used for sending state to bots for each turn
    def get_player_state(self, player):
        pass

    def do_moves(self, player, moves):
        pass

    def do_all_moves(self, bot_moves):
        return [self.do_moves(b, moves) for b, moves in enumerate(bot_moves)]

    # used for ranking
    def get_scores(self):
        pass

    # can be used to determine fairness of game and other stuff for visualizers
    def get_stats(self):
        pass

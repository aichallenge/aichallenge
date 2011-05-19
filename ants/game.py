#!/usr/bin/env python

# Games used by the engine should implement the following methods
class Game:
    def __init__(self):
        pass

    # load starting map or game board positions
    def load_map(self, filename):
        pass

    # common functions for all games used by engine
    def start_game(self):
        pass

    # do things needed for start of turn (cleanup, etc...)
    def start_turn(self):
        pass

    # do things needed for finishing a turn (resolving orders, scoring, etc...)
    def finish_turn(self):
        pass

    # do things needed for finishing a game (scoring, etc...)
    def finish_game(self):
        pass

    # remove a player from the game, may be a crashed/timed out bot
    def kill_player(self, player):
        pass

    # return if a player is alive, might be removed by game mechanics
    def is_alive(self, player):
        pass

    # returns if the game is over due to a win condition
    def game_over(self): # returns boolean
        pass

    # used by engine to get the current game state for the streaming format
    def get_state(self):
        pass

    # used for turn 0, sending minimal info for bot to load
    # when passed none, the output is used at the start of the streaming format
    def get_player_start(self, player=None):
        pass

    # used for sending state to bots for each turn
    def get_player_state(self, player):
        pass

    # process a single player's moves, may be appropriate to resolve during finish turn
    def do_moves(self, player, moves):
        # returns valid, ignored, invalid
        #         [''],  [('','')], [('','')]
        pass

    def do_all_moves(self, bot_moves):
        return [self.do_moves(b, moves) for b, moves in enumerate(bot_moves)]

    # used for ranking
    def get_scores(self):
        pass

    # can be used to determine fairness of game and other stuff for visualizers
    def get_stats(self):
        pass
    
    # used for getting a compact replay of the game
    def get_replay(self):
        pass

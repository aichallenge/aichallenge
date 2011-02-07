"""
Sample game in which bots try to choose a higher number
not greater than one chosen by the judge.

Exists only for demonstrative purposes.
"""

from random import randint
import worker.games

class HigherNumberGame(worker.games.Game):
    
    def run(self):
        magic_number = randint(1, 10)
        disqualified = []
        scores = {}
        
        for team in self.teams:
            info, runner = team[0] # assume one player per team
            try:
                with runner.time_limit(2):
                    move = int(runner.readline())
            except:
                disqualified.append(info)
                continue
            
            if move <= 0:
                disqualified.append(info)
            elif move <= magic_number:
                scores[info] = move
            else:
                scores[info] = 0
        
        if disqualified:
            return {'disqualified': disqualified}
        else:
            return {'scores': scores}

Game = HigherNumberGame

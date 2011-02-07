import sys

from worker.runner import Runner

def get_game(name):
	"""get the game class from the module requested"""
	full_name = 'worker.games.' + name
	__import__(full_name)
	return sys.modules[full_name].Game

class Game(object):
	"""game class providing useful underlying functionality"""
	def __init__(self, teams):
		self.teams = [self.start_team(team) for team in teams]
	
	def start_team(self, team):
		return [self.start_player(player) for player in team]
	
	def start_player(self, player):
		runner = Runner("python /var/lib/aichallenge/submissions/%s/MyBot.py" % player['submission_hash'])
		return (player['name'], runner)
	
	def stop_team(self, team):
		[self.stop_player(player) for player in team]
	
	def stop_player(self, player):
		_, runner = player
		runner.kill()
	
	def run(self):
		pass
	
	def stop(self):
		[self.stop_team(team) for team in self.teams]

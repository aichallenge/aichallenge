import glob
import os
import os.path
import pwd
import subprocess
import time

from worker.config import config
import worker.games
import worker.log
import worker.queue
import worker.runner
import worker.storage

try:
	from setproctitle import setproctitle
except ImportError:
	def setproctitle(title):
		pass

class Worker(object):
	"""responsible for doing the heavy lifting"""

	def __init__(self):
		"""create a Worker object"""
		self.queue = worker.queue.SQSQueue('aichallenge-work')
		self.result_queue = worker.queue.SQSQueue('aichallenge-results')
		self.logger = worker.log.setup()

	def loop(self):
		"""find and perform tasks until terminated"""
		# replace this with the actual processing logic
		self.logger.info('started')
		while True:
			message = self.queue.get()
			body = message.get_body()
			self.logger.info('message received: ' + repr(body))
			
			if 'command' not in body:
				pass
			elif body['command'] == 'run_game':
				self.run_game(body)
			
			self.queue.delete(message)
	
	def run_game(self, body):
		"""run a game based on the parameters of a queue message body"""
		
		# fetch players from storage if necessary
		for team in body['teams']:
			for player in team:
				worker.storage.fetch(player['submission_hash'], logger=self.logger)
		
		# run the game and log the result
		current_game = config.get('worker', 'current_game')
		game_class = worker.games.get_game(current_game)
		game = game_class(body['teams'])
		result = game.run()
		game.stop()
		self.logger.info('result: ' + repr(result))
		self.result_queue.put(result)

def spawn_multiple(num_procs):
	"""spawns a number of worker processes"""
	
	# ensure first that the lock directory exists
	lock_dir = os.path.expandvars("$AICHALLENGE_PREFIX/var/run/aichallenge")
	if not os.path.isdir(lock_dir):
		os.mkdir(lock_dir, 0755)
	
	pid = os.fork()
	if pid == 0:
		try:
			user = pwd.getpwnam('aichallenge')
			os.chown(lock_dir, user.pw_uid, user.pw_gid)
			os.setgid(user.pw_gid)
			os.setuid(user.pw_uid)
		except KeyError:
			print "user 'aichallenge' not found. Continuing with current UID/GID..."
		except OSError:
			print "unable to setuid/setgid. Continuing with current UID/GID..."
		
		os.setsid()
		
		for n in xrange(num_procs):
			spawn()

def spawn():
	"""spawns a new worker process"""
	pid = os.fork()
	if pid == 0:
		setproctitle('aichallenge worker')
		Worker().loop()
		exit()

def stop():
	"""stop all workers"""
	subprocess.call(['killall', 'aichallenge worker'])
	for user in worker.runner.users():
		subprocess.call(['killall', '-9', '-u', user])
	
	lock_dir = os.path.expandvars("$AICHALLENGE_PREFIX/var/run/aichallenge")
	for lock_file in glob.iglob("%s/*.lock" % lock_dir):
		os.unlink(lock_file)

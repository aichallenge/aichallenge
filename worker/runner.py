"""
This module contains code for running bots in a contained environment
suitable for playing games, and exposes an API through which the game
judging modules can interact with the bots.
"""

import pwd
import os
import os.path
import shlex
import signal
import time
from subprocess import Popen, PIPE, call

import worker.config

class Runner(object):
	"""Runs a bot"""
	
	def __init__(self, command, cwd="/tmp"):
		self.user = self.acquire_user()
		command = ["sudo", "-Hnu", self.user, "--"] + shlex.split(command)
		self.process = Popen(command, stdin=PIPE, stdout=PIPE, stderr=PIPE,
		                     close_fds=True, cwd=cwd)
		time.sleep(1) # some initial setup time
		self.send_signal(signal.SIGSTOP)
	
	def send_signal(self, signal):
		"""send a signal to the process"""
		call(["sudo", "-Hnu", self.user,
				"kill", "-s", str(signal), str(self.process.pid)])
	
	def kill(self):
		"""terminate the process and release the user"""
		self.send_signal(signal.SIGKILL)
		self.release_user(self.user)
	
	def write(self, data):
		"""write to the process' standard input"""
		self.process.stdin.write(data)
	
	def readline(self):
		"""read a line from the process' standard output"""
		return self.process.stdout.readline()
	
	def time_limit(self, timeout=1):
		"""unfreeze the process, do something with a time limit, then refreeze it"""
		return TimeLimit(runner=self, timeout=timeout)
	
	def acquire_user(self):
		"""find and lock a user for running untrusted code"""
		lock_dir = os.path.expandvars("$AICHALLENGE_PREFIX/var/run/aichallenge")
		while True:
			for user in users():
				try:
					self.lock_fd = os.open("%s/%s.lock" % (lock_dir, user),
					                       os.O_CREAT | os.O_EXCL)
					return user
				except OSError:
					pass
			
			time.sleep(5)
	
	def release_user(self, user):
		"""release the lock on a runner user"""
		lock_dir = os.path.expandvars("$AICHALLENGE_PREFIX/var/run/aichallenge")
		os.close(self.lock_fd)
		os.unlink("%s/%s.lock" % (lock_dir, user))

def users():
	try:
		for n in xrange(100):
			user = 'aichallenge-run%d' % n
			pwd.getpwnam(user)
			yield user
	except KeyError:
		pass

class TimeLimit(object):
	"""
	Context manager which limits the execution time of its suite.
	Sends SIGCONT/SIGSTOP to runner as appropriate, if applicable.
	"""
	
	def __init__(self, timeout=1, runner=None):
		self.runner = runner
		self.timeout = timeout
	
	def __enter__(self):
		def timed_out(sig, frame): raise TimeoutError()
		if self.runner: self.runner.send_signal(signal.SIGCONT)
		signal.signal(signal.SIGALRM, timed_out)
		signal.alarm(self.timeout)
		return self.runner
	
	def __exit__(self, exc_type, exc_value, traceback):
		signal.alarm(0)
		signal.signal(signal.SIGALRM, signal.SIG_DFL)
		if self.runner: self.runner.send_signal(signal.SIGSTOP)

class TimeoutError(Exception):
	pass

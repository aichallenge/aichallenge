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
	
	def with_time_limit(self, callable, timeout=1):
		"""unfreeze the process, do something with a time limit, then refreeze it"""
		def timed_out(sig, frame):
			signal.signal(signal.SIGALRM, signal.SIG_DFL)
			self.send_signal(signal.SIGSTOP)
			raise TimeoutError()
		
		self.send_signal(signal.SIGCONT)
		signal.signal(signal.SIGALRM, timed_out)
		signal.alarm(timeout)
		result = callable(self)
		signal.alarm(0)
		signal.signal(signal.SIGALRM, signal.SIG_DFL)
		self.send_signal(signal.SIGSTOP)
		return result
	
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

class TimeoutError(Exception):
	pass

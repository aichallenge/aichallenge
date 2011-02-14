"""
This module contains code for running bots in a contained environment
suitable for playing games, and exposes an API through which the game
judging modules can interact with the bots.
"""

import os
import os.path
import pwd
import shlex
import shutil
import signal
import time
from subprocess import Popen, PIPE, call

import worker.config

class Runner(object):
    """Runs a bot"""
    
    def __init__(self, origin=None, use_working=True):
        self.user = self.acquire_user()
        self.origin = origin
        self.process = None
        if use_working:
            self.create_working_dir()
        else:
            self.working = origin
    
    def create_working_dir(self):
        working = "$AICHALLENGE_PREFIX/var/lib/aichallenge/working/%s"
        self.working = os.path.expandvars(working % self.user)
        old_umask = os.umask(07007)
        shutil.copytree(self.origin, self.working, True)
        os.chown(self.working, -1, pwd.getpwnam(self.user).pw_gid)
        os.chmod(self.working, 0770)
        os.umask(old_umask)
    
    def remove_working_dir(self):
        if self.working != self.origin:
            shutil.rmtree(self.working)
    
    def run(self, command, cwd="{working}", should_stop=True):
        cwd = cwd.format(working=self.working, origin=self.origin)
        if isinstance(command, str): command = shlex.split(command)
        command = ["sudo", "-Hnu", self.user, "--"] + command
        self.process = Popen(command, stdin=PIPE, stdout=PIPE, stderr=PIPE,
                             close_fds=True, cwd=cwd)
        time.sleep(0.3) # some initial setup time
        if should_stop: self.send_signal(signal.SIGSTOP)
    
    def send_signal(self, signal):
        """send a signal to the process"""
        if self.process is not None:
            call(["sudo", "-Hnu", self.user,
                  "kill", "-s", str(signal), str(self.process.pid)],
                  stdout=PIPE, stderr=PIPE)
    
    def done(self):
        """done with the runner -- release the user and clear the work dir"""
        self.remove_working_dir()
        self.release_user(self.user)
    
    def kill(self):
        """terminate the process"""
        self.send_signal(signal.SIGKILL)
    
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
        def timed_out(sig, frame):
            raise TimeoutError()
        if self.runner:
            self.runner.send_signal(signal.SIGCONT)
        signal.signal(signal.SIGALRM, timed_out)
        signal.alarm(self.timeout)
        return self.runner
    
    def __exit__(self, exc_type, exc_value, traceback):
        signal.alarm(0)
        signal.signal(signal.SIGALRM, signal.SIG_DFL)
        if self.runner:
            self.runner.send_signal(signal.SIGSTOP)

class TimeoutError(Exception):
    pass

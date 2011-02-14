import glob
import grp
import os
import os.path
import pwd
import shutil
import subprocess
import time
import traceback

from worker.config import config
from worker.queue import SQSQueue
from worker.submission import Submission
import worker.games
import worker.log
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
        self.logger = worker.log.setup()
        self.queue = SQSQueue('aichallenge-work', logger=self.logger)
        self.result_queue = SQSQueue('aichallenge-results', logger=self.logger)

    def loop(self):
        """find and perform tasks until terminated"""
        # replace this with the actual processing logic
        self.logger.info('started')
        while True:
            try:
                message = self.queue.get()
                body = message.get_body()
                self.logger.info('message received: ' + repr(body))
            
                if 'command' not in body:
                    pass
                elif body['command'] == 'run_game':
                    if self.run_game(body):
                        self.queue.delete(message)
            except:
                self.logger.error('Uncaught exception!')
                self.logger.error(traceback.format_exc())
    
    def run_game(self, body):
        """run a game based on the parameters of a queue message body"""
        
        # fetch players from storage if necessary
        teams = self.fetch_and_compile_teams(body['teams'])
        
        # run the game and log the result
        current_game = config.get('worker', 'current_game')
        game_class = worker.games.get_game(current_game)
        game = game_class(body, teams)
        try:
            result = game.run()
            self.logger.info('result: ' + repr(result))
            self.result_queue.put(result)
            return True
        except:
            return False
        finally:
            # Ensure that runner resources are freed
            game.stop()
    
    def fetch_and_compile_teams(self, teams):
        return [[self.fetch_and_compile(player) for player in team]
                    for team in teams]
    
    def fetch_and_compile(self, player):
        worker.storage.fetch(player['submission_hash'], logger=self.logger)
        dir = "$AICHALLENGE_PREFIX/var/lib/aichallenge/submissions/%s"
        dir = os.path.expandvars(dir % player['submission_hash'])
        # TODO: make the submission number meaningful
        subm = Submission(player['name'], 0, dir)
        subm.compile_if_needed(logger=self.logger)
        return subm

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
            initgroups(user.pw_name)
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
    
    working_dir = "$AICHALLENGE_PREFIX/var/lib/aichallenge/working"
    working_dir = os.path.expandvars(working_dir)
    for dir in glob.iglob("%s/aichallenge*" % working_dir):
        shutil.rmtree(dir)

def initgroups(username):
    """set up supplementary group IDs prioer to dropping privileges"""
    os.setgroups([g.gr_gid for g in grp.getgrall() if username in g.gr_mem])

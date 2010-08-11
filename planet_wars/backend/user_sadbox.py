#!/usr/bin/python
#
# Copyright 2010 owners of the AI Challenge project
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy
# of the License at http://www.apache.org/licenses/LICENSE-2.0 . Unless
# required by applicable law or agreed to in writing, software distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied. See the License for the
# specific language governing permissions and limitations under the License.
#
# Author(s): Syed S. Albiz (s.s.albiz@gmail.com)
# Executes a given shell command inside a qemu VM instance

import fcntl
import getopt
import MySQLdb
import os
from Queue import Queue
from server_info import server_info
import shlex
import signal
import subprocess
import sys
from threading import Thread
import time

def usage():
  print  """sadbox.py - Executes commands in a sandbox VM
Required Arguments:
--directory, -d : directory where executable and related files live
--command, -c : shell command to execute inside sandboxed VM
--security, -s: is not set, command is run on current host, otherwise,
                funneled through VM
"""

# Grabs a random jail user and attempts to mark it as locked in an atomic
# fashion. If this succeeds, the username is returned. If no jail user is
# successfully locked, then None is returned.
def lock_jail_user():
  connection = MySQLdb.connect(host = server_info["db_host"],
                               user = server_info["db_username"],
                               passwd = server_info["db_password"],
                               db = server_info["db_name"])
  cursor = connection.cursor(MySQLdb.cursors.DictCursor)
  cursor.execute("SELECT * FROM jail_users WHERE in_use = 0 ORDER BY RAND()")
  jail_users = cursor.fetchall()
  username = None
  if len(jail_users) > 0:
    jail_user_id = jail_users[0]["jail_user_id"]
    query = "UPDATE jail_users SET in_use = 1 WHERE jail_user_id = " + \
      str(jail_user_id) + " AND in_use = 0"
    cursor.execute(query)
    if connection.affected_rows() > 0:
      username = jail_users[0]["username"]
  cursor.close()
  connection.close()
  return username

# Releases a jail user.
def release_jail_user(username):
  if username is None:
    return
  connection = MySQLdb.connect(host = server_info["db_host"],
                               user = server_info["db_username"],
                               passwd = server_info["db_password"],
                               db = server_info["db_name"])
  cursor = connection.cursor(MySQLdb.cursors.DictCursor)
  cursor.execute("UPDATE jail_users SET in_use = 0 WHERE username = '" + \
    str(username) + "'")
  cursor.close()
  connection.close()

def monitor_input_channel(sadbox):
  while sadbox.is_alive:
    try:
      line = sadbox.command_process.stdout.readline()
    except:
      sadbox.kill()
      break
    if line is None or len(line) == 0:
      sadbox.kill()
      break
    sadbox.stdout_queue.put(line.strip())

# The sadbox class is used to invoke arbitrary shell commands. Its main feature
# is that it has the option to launch the shell command inside a dedicated
# virtual machine, in order to totally isolate the command.
class Sadbox:
  # Initializes a new sadbox and invoke the given shell command inside it.
  #   working_directory: the directory in which the shell command should be
  #                      launched. If security is enabled, files from this
  #                      directory are copied into the VM before the shell
  #                      command is executed.
  #   shell_command: the shell command to launch inside the sadbox.
  #   security_on: a boolean flag that specifies whether real security is
  #                enabled. If false, then the shell command is executed on
  #                bare metal, without any security. If true, the shell command
  #                is executed inside its own virtual machine so it is totally
  #                isolated.
  def __init__(self, working_directory, shell_command, security_on):
    self.is_alive = False
    self.command_process = None
    self.stdout_queue = Queue()
    self.jail_username = None
    if security_on:
      self.jail_username = lock_jail_user()
      if self.jail_username is None:
        return
      os.system("ssh -i jail_id_rsa " + self.jail_username + \
        "@localhost \"rm -rf *\"")
      os.system("scp -r -i jail_id_rsa " + str(working_directory) + " " + \
        self.jail_username + "@localhost:~/ > /dev/null 2> /dev/null")
      ssh_command = "ssh -i jail_id_rsa " + self.jail_username + \
        "@localhost " + shell_command
      self.command_process = subprocess.Popen(shlex.split(ssh_command), \
                                              stdin=subprocess.PIPE, \
                                              stdout=subprocess.PIPE, \
                                              stderr=subprocess.PIPE)
      self.is_alive = not self.command_process is None
      stdout_monitor = Thread(target=monitor_input_channel, args=(self,))
      stdout_monitor.start()
    else:
      self.command_process = subprocess.Popen(shlex.split(shell_command), \
                                         stdin=subprocess.PIPE, \
                                         stdout=subprocess.PIPE, \
                                         stderr=subprocess.PIPE, \
                                         cwd=working_directory)
      stdout_fd = self.command_process.stdout.fileno()
      fcntl_handle = fcntl.fcntl(stdout_fd, fcntl.F_GETFL)
      fcntl.fcntl(stdout_fd, fcntl.F_SETFL, fcntl_handle | os.O_NONBLOCK)
      self.is_alive = not self.command_process is None

  # Shuts down the sadbox, cleaning up any spawned processes, threads, and
  # other resources. The shell command running inside the sadbox may be
  # suddenly terminated.
  def kill(self):
    if self.is_alive:
      os.system("ssh -i jail_id_rsa " + self.jail_username + "@localhost " + \
        "killall -u " + self.jail_username + " > /dev/null 2> /dev/null")
      os.kill(self.command_process.pid, signal.SIGKILL)
      self.is_alive = False
      release_jail_user(self.jail_username)

  def write(self, line):
    if not self.is_alive:
      return
    try:
      self.command_process.stdin.write(line)
      self.command_process.stdin.flush()
      return 0
    except:
      self.kill()
      return -1

  def write_line(self, line):
    if not self.is_alive:
      return
    try:
      self.command_process.stdin.write(line + "\n")
      self.command_process.stdin.flush()
      return 0
    except:
      self.kill()
      return -1

  def read_line(self):
    if not self.is_alive:
      return None
    try:
      return self.stdout_queue.get(block=False, timeout=0)
    except:
      return None

  def read_error_line(self):
    if not self.is_alive:
      return None
    return self.command_process.stderr.readline().strip()

def main():
  sadbox = Sadbox("../submissions/122734/.", "./MyBot", True)
  #sadbox = Sadbox("../submissions/122742/.", "python MyBot.py", False)
  time.sleep(1)
  sadbox.write_line("P 0 0 1 34 2")
  sadbox.write_line("P 7 9 2 34 2")
  sadbox.write_line("P 3.14 2.71 0 15 5")
  sadbox.write_line("go")
  time.sleep(1)
  while True:
    response = sadbox.read_line()
    if response is None:
      print "No more responses. Terminating."
      break
    print "response: " + response
  sadbox.kill()

if __name__ == "__main__":
  main()

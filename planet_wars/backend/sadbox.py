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
import os
import signal
import sys
import time
import shlex
import subprocess

def usage():
  print  """sadbox.py - Executes commands in a sandbox VM
Required Arguments:
--directory, -d : directory where executable and related files live
--command, -c : shell command to execute inside sandboxed VM
--security, -s: is not set, command is run on current host, otherwise,
                funneled through VM
"""

qemu_process = None
child_process = None

def copy_exec_dir(qemu_port, qemu_identfile, sadbox_path, dest_path):
  scp_cmd = "scp -r -P " + str(qemu_port) + " -i " + qemu_identfile + \
    " " + sadbox_path + " " + dest_path
  sys.stderr.write( scp_cmd + "\n")
  scp_args = shlex.split(scp_cmd)
  scp_process = subprocess.Popen(scp_args)
  sys.stderr.write( "copying files to instance\n" )
  scp_process.wait()
  return scp_process

def construct_qemu_shell_cmd(image, port):
  return "/usr/bin/qemu " + image + " -net nic -net user -redir tcp:" + \
    str(port) + "::22 -nographic " #-loadvm contest"

def launch_qemu():
  img_name = "test.img"
  qemu_port = 5555
  qemu_cmd = construct_qemu_shell_cmd(img_name, qemu_port)
  sys.stderr.write( qemu_cmd + "\n")
  qemu_args = shlex.split(qemu_cmd)
  sys.stderr.write( "spinning up qemu instance\n")
  qemu_process = subprocess.Popen(qemu_args, \
                                  stdin=subprocess.PIPE, \
                                  stdout=subprocess.PIPE, \
                                  close_fds=True)
  time.sleep(1)
  qemu_process.poll()
  while qemu_process.returncode != None and qemu_port < 5565:
    qemu_port += 1
    qemu_cmd = construct_qemu_shell_cmd(img_name, qemu_port)
    qemu_args = shlex.split(qemu_cmd)
    qemu_process = subprocess.Popen(qemu_args, \
                                    stdin=subprocess.PIPE, \
                                    stdout=subprocess.PIPE, \
                                    close_fds=True)
    time.sleep(1)
    qemu_process.poll()
    if (qemu_process.returncode != None):
      sys.stderr.write( "Error starting qemu instance, max retries exceeded\n")
      sys.exit(2)
  print "Starting qemu process on port " + str(qemu_port)
  print "Waiting for 60 seconds"
  time.sleep(60)
  return (qemu_process, qemu_port)

def handler(signum, frame):
  if child_process:
    os.kill(child_process.pid, signal.SIGTERM)
  if qemu_process:
    os.kill(qemu_process.pid, signal.SIGTERM)

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
    if security_on:
      sadbox_dir = os.path.basename(sadbox_path)
      cmd = "cd ./" + sadbox_dir + "/; " + cmd
      (qemu_proc, port) = launch_qemu()
      scp_proc = copy_exec_dir(port, qemu_identfile, sadbox_path, dest_path)
      child_cmd = "ssh -p " + str(port) + " -i " + qemu_identfile + \
        " contestvm@localhost " + cmd
      child_args = shlex.split(child_cmd)
      child_process = subprocess.Popen(child_args, \
                                       stdin=subprocess.PIPE, \
                                       stdout=subprocess.PIPE, \
                                       stderr=subprocess.PIPE, \
                                       close_fds=True)
      sys.stderr.write("executing command inside VM\n")
      child_process.wait()
      os.kill(qemu_proc.pid, signal.SIGINT)
      qemu_proc.wait()
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
      os.kill(self.command_process.pid, signal.SIGKILL)
      self.is_alive = False

  def write_line(self, line):
    if not self.is_alive:
      return
    self.command_process.stdin.write(line + "\n")
    self.command_process.stdin.flush()

  def read_line(self):
    if not self.is_alive:
      return None
    try:
      return self.command_process.stdout.readline().strip()
    except:
      return None

  def read_error_line(self):
    if not self.is_alive:
      return None
    return self.command_process.stderr.readline().strip()

def main():
  sadbox = Sadbox("../submissions/122743/.", "java -jar MyBot.jar", False)
  sadbox.write_line("go")
  time.sleep(1)
  print "read_line: " + str(sadbox.read_line())
  print "read_line: " + str(sadbox.read_line())
  sadbox.kill()
  sys.exit(0)

  signal.signal(signal.SIGINT, handler)
  qemu_identfile = "vmkey"
  dest_path = "contestvm@localhost:~"
  try:
    opts, args = getopt.getopt(sys.argv[1:], \
                               "d:c:s:", \
                               ["directory=", "command=", "security="])
  except getopt.GetoptError, err:
    sys.stderr.write( str(err) )
    usage()
    sys.exit(2)
  sadbox_path = None
  cmd = None
  security_on = False
  for o, a in opts:
    if o in ("-d", "--directory"):
      sadbox_path = a
    elif o in ("-c", "--command"):
      cmd = a
    elif o in ("-s", "--security"):
      security_on = a.lower() in ("on", "true", "1")
    else:
      usage()
  if sadbox_path == None:
    usage()
    sys.exit(2)
  if security_on:
    sadbox_dir = os.path.basename(sadbox_path)
    cmd = "cd ./" + sadbox_dir + "/; " + cmd
    (qemu_proc, port) = launch_qemu()
    scp_proc = copy_exec_dir(port, qemu_identfile, sadbox_path, dest_path)
    child_cmd = "ssh -p " + str(port) + " -i " + qemu_identfile + \
      " contestvm@localhost " + cmd
    child_args = shlex.split(child_cmd)
    child_process = subprocess.Popen(child_args, close_fds=True)
    sys.stderr.write( "executing command inside VM\n")
    child_process.wait()
    os.kill(qemu_proc.pid, signal.SIGINT)
    qemu_proc.wait()
  else:
    cmd = cmd.replace("+", " ")
    args = shlex.split(cmd)
    os.chdir(sadbox_path)
    p = subprocess.Popen(args)
    p.wait();

if __name__ == "__main__":
  main()

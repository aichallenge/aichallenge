#!/usr/bin/python
import os
from Queue import Queue
import shlex
import signal
import subprocess
import sys
from threading import Thread, Timer
import time

def usage():
    print    """sandbox.py - Executes commands in a sandbox VM
Required Arguments:
--directory, -d : directory where executable and related files live
--command, -c : shell command to execute inside sandboxed VM
--security, -s: is not set, command is run on current host, otherwise,
                                funneled through VM
"""

def monitor_input_channel(sandbox):
    #print "start monitor"
    while sandbox.is_alive:
        try:
            line = sandbox.command_process.stdout.readline()
        except:
            print sys.exc_info()
            sandbox.kill()
            break
        if line is None or len(line) == 0:
            try:
                sandbox.kill()
            except:
                pass
            break
        sandbox.stdout_queue.put(line.strip())
    #print "end monitor"
    e = sandbox.command_process.stderr.read().strip()
    if len(e) > 0:
        print e

# The sandbox class is used to invoke arbitrary shell commands. Its main feature
# is that it has the option to launch the shell command inside a dedicated
# virtual machine, in order to totally isolate the command.
class Sandbox:
    # Initializes a new sandbox and invoke the given shell command inside it.
    #     working_directory: the directory in which the shell command should be
    #                                            launched. If security is enabled, files from this
    #                                            directory are copied into the VM before the shell
    #                                            command is executed.
    #     shell_command: the shell command to launch inside the sandbox.
    def __init__(self, working_directory, shell_command, jailuser=None):
        shell_command = shell_command.replace('\\','/')
        self.is_alive = False
        self.command_process = None
        self.stdout_queue = Queue()
        self.stderr_queue = Queue()
        self.command_process = subprocess.Popen(shlex.split(shell_command),
                                                stdin=subprocess.PIPE,
                                                stdout=subprocess.PIPE,
                                                stderr=subprocess.PIPE,
                                                cwd=working_directory)
        self.is_alive = not self.command_process is None
        stdout_monitor = Timer(1, monitor_input_channel, args=(self,))
        stdout_monitor.start()

    # Shuts down the sandbox, cleaning up any spawned processes, threads, and
    # other resources. The shell command running inside the sandbox may be
    # suddenly terminated.
    def kill(self):
        if self.is_alive:
            try:
                self.command_process.kill()
                self.command_process.wait()
            except:
                pass
            #os.kill(self.command_process.pid, signal.SIGKILL)
            self.is_alive = False

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

def main():
    sandbox = Sandbox(".", "python bots\\MyBot.py")
    #sandbox = Sandbox("../submissions/122742/.", "python MyBot.py", False)
    time.sleep(1)
    sandbox.write_line("D 10 10")
    sandbox.write_line("ready")
    time.sleep(1)
    while True:
        response = sandbox.read_line()
        if response is None:
            print "No more responses. Terminating."
            break
        print "response: " + response
    sandbox.kill()

if __name__ == "__main__":
    main()

#!/usr/bin/python
import os
import shlex
import signal
import subprocess
import sys
import time
from optparse import OptionParser
from Queue import Queue, Empty
from threading import Thread

def _monitor_input_channel(sandbox):
    while sandbox.is_alive:
        try:
            line = sandbox.command_process.stdout.readline()
        except:
            print >> sys.stderr, sys.exc_info()
            sandbox.kill()
            break
        if not line:
            sandbox.kill()
            break
        sandbox.stdout_queue.put(line.strip())

class Sandbox:
    """Provide a sandbox to run arbitrary commands in.

    The sandbox class is used to invoke arbitrary shell commands. Its main
    feature is that it has the option to launch the shell command inside a
    jail, in order to totally isolate the command.

    """

    def __init__(self, working_directory, shell_command, stderr=None):
        """Initialize a new sandbox and invoke the given shell command inside.

        working_directory: the directory in which the shell command should
                           be launched. If security is enabled, files from
                           this directory are copied into the VM before the
                           shell command is executed.
        shell_command: the shell command to launch inside the sandbox.
        stderr: where the bot's stderr output should be written out to

        """
        shell_command = shell_command.replace('\\','/')
        self.is_alive = False
        self.command_process = None
        self.stdout_queue = Queue()
        self.stderr_queue = Queue()

        self.command_process = subprocess.Popen(shlex.split(shell_command),
                                                stdin=subprocess.PIPE,
                                                stdout=subprocess.PIPE,
                                                stderr=stderr,
                                                cwd=working_directory)
        self.is_alive = not self.command_process is None
        stdout_monitor = Thread(target=_monitor_input_channel, args=(self,))
        stdout_monitor.start()

    def kill(self):
        """ Shuts down the sandbox.

        Shuts down the sandbox, cleaning up any spawned processes, threads, and
        other resources. The shell command running inside the sandbox may be
        suddenly terminated.

        """
        if self.is_alive:
            try:
                self.command_process.kill()
                self.command_process.wait()
            except OSError:
                pass
            self.is_alive = False

    def pause(self):
        """Pause the process by sending a SIGSTOP to the child

        This method is a no-op on Windows
        """
        try:
            self.command_process.send_signal(signal.SIGSTOP)
        except ValueError, AttributeError:
            pass

    def resume(self):
        """Resume the process by sending a SIGCONT to the child

        This method is a no-op on Windows
        """
        try:
            self.command_process.send_signal(signal.SIGCONT)
        except ValueError, AttributeError:
            pass

    def write(self, str):
        """Write str to stdin of the process being run"""
        if not self.is_alive:
            return False
        try:
            self.command_process.stdin.write(str)
            self.command_process.stdin.flush()
        except (OSError, IOError):
            self.kill()
            return False
        return True

    def write_line(self, line):
        """Write line to stdin of the process being run

        A newline is appended to line and written to stdin of the child process

        """
        if not self.is_alive:
            return False
        try:
            self.command_process.stdin.write(line + "\n")
            self.command_process.stdin.flush()
        except (OSError, IOError):
            self.kill()
            return False
        return True

    def read_line(self):
        """Read line from child process

        Returns a line of the child process' stdout, if one isn't available
        returns None.

        """
        try:
            return self.stdout_queue.get(block=False)
        except Empty:
            return None

def main():
    parser = OptionParser(usage="usage: %prog [options] <command to run>")
    parser.add_option("-d", "--directory", action="store", dest="working_dir",
            default=".",
            help="Working directory to run command in (copied in secure mode)")
    parser.add_option("-l", action="append", dest="send_lines", default=list(),
            help="String to send as a line on commands stdin")
    parser.add_option("-s", "--send-delay", action="store", dest="send_delay",
            type="float", default=0.0,
            help="Time in seconds to sleep after sending a line")
    parser.add_option("-r", "--receive-delay", action="store",
            dest="resp_delay", type="float", default=0.01,
            help="Time in seconds to sleep before checking for a response line")
    options, args = parser.parse_args()
    if len(args) == 0:
        parser.error("Must include a command to run.\
                \nRun with --help for more information.")

    sandbox = Sandbox(options.working_dir, " ".join(args))
    for line in options.send_lines:
        if not sandbox.write_line(line):
            print >> sys.stderr, "Could not send line '%s'" % (line,)
            sandbox.kill()
            sys.exit(1)
        print "sent:", line
        time.sleep(options.send_delay)
    while True:
        time.sleep(options.resp_delay)
        response = sandbox.read_line()
        if response is None:
            print "No more responses. Terminating."
            break
        print "response: " + response
    sandbox.kill()

if __name__ == "__main__":
    main()

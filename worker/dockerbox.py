#!/usr/bin/python
from __future__ import print_function
import os
import shlex
import signal
import subprocess
import sys
import time
import pexpect
from optparse import OptionParser
from threading import Thread


class DockerBox(object):
    """ Provide a secure sandbox to run arbitrary commands in.

    This will only function on specially prepared Ubuntu systems.

    """

    def __init__(self):
        self._is_alive = False
        self.command_process = None

    # def __del__(self):
    #     if self.locked:
    #         raise SandboxError("Jail object for %s freed without being released"
    #                            % (self.name))

    @property
    def is_alive(self):
        """Indicates whether a command is currently running in the sandbox"""
        if self.command_process is None:
            return False

        return self.command_process.isalive

    def start(self, shell_command):
        """Start a command running in the sandbox"""
        if self.is_alive:
            raise SandboxError("Tried to run command with one in progress.")
        fout = open("log.txt", "wb")
        self.command_process = pexpect.pty_spawn.spawn(
            "docker run -it test:0.1")
        self.command_process.logfile_read = fout
        # self.command_process.expect("root")
        self.command_process.sendline("cd bot && python GreedyBot.py")
        # self.command_process.expect("root")

    def kill(self):
        self.command_process.terminate(force=True)

    def write(self, data, word="go"):
        for line in data.splitlines():
            self.command_process.sendline(line)

        # i = self.command_process.expect([pexpect.TIMEOUT, word])

        # if i == 0:
        #     print('ERROR! could not connect. Here is what was said:')
        #     print(self.command_process.before, self.command_process.after)
        #     print(str(self.command_process))
        #     self.kill()
        #     sys.exit(1)

    def write_line(self, line):
        self.write(line)

    def read_line(self):
        return self.command_process.readline()

    def read_lines(self, timeout=0):
        """Read line from child process

        Returns a line of the child process' stdout, if one isn't available
        within timeout seconds it returns None. Also guaranteed to return None
        at least once after each command that is run in the sandbox.

        """
        return self.command_process.readline()
        # return self.command_process.before.splitlines()

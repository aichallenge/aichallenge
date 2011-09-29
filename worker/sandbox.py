#!/usr/bin/python
from __future__ import print_function
import os
import shlex
import signal
import subprocess
import sys
import time
from optparse import OptionParser
from threading import Thread
try:
    from Queue import Queue, Empty
except ImportError:
    from queue import Queue, Empty

# make python 3.x compatible with python 2.x
if sys.version_info >= (3,):
    def unicode(s, errors="strict"):
        if isinstance(s, str):
            return s
        elif isinstance(s, bytes) or isinstance(s, bytearray):
            return s.decode("utf-8", errors)
        raise SandboxError("Tried to convert unrecognized type to unicode")

try:
    from server_info import server_info
    _SECURE_DEFAULT = server_info.get('secure_jail', True)
except ImportError:
    _SECURE_DEFAULT = False

class SandboxError(Exception):
    pass

def _guard_monitor(jail):
    guard_out = jail.command_process.stdout
    while True:
        line = guard_out.readline()
        if not line:
            end_item = (time.time(), None)
            jail.resp_queue.put(end_item)
            jail.stdout_queue.put(end_item)
            jail.stderr_queue.put(end_item)
            break
        line = line.rstrip("\r\n")
        words = line.split(None, 2)
        if len(words) < 3:
            msg, ts = words
            data = ""
        else:
            msg, ts, data = words
        ts = float(ts)
        data = unicode(data, errors="replace")
        if msg == "STDOUT":
            jail.stdout_queue.put((time, data))
        elif msg == "STDERR":
            jail.stderr_queue.put((time, data))
        elif msg == "SIGNALED":
            jail.resp_queue.put((time, data))

class Jail(object):
    """ Provide a secure sandbox to run arbitrary commands in.

    This will only function on specially prepared Ubuntu systems.

    """
    def __init__(self, working_directory):
        """Initialize a new sandbox for the given working directory.

        working_directory: the directory in which the shell command should
                           be launched. Files from this directory are copied
                           into the secure space before the shell command is
                           executed.
        """
        self.locked = False
        jail_base = "/srv/chroot"
        all_jails = os.listdir(jail_base)
        all_jails = [j for j in all_jails if j.startswith("jailuser")]
        for jail in all_jails:
            lock_dir = os.path.join(jail_base, jail, "locked")
            try:
                os.mkdir(lock_dir)
            except OSError:
                # if the directory could not be created, that should mean the
                # jail is already locked and in use
                continue
            with open(os.path.join(lock_dir, "lock.pid"), "w") as pid_file:
                pid_file.write(str(os.getpid()))
            self.locked = True
            self.name = jail
            break
        else:
            raise SandboxError("Could not find an unlocked jail")
        self.jchown = os.path.join(server_info["repo_path"], "worker/jail_own")
        self.base_dir = os.path.join(jail_base, jail)
        self.number = int(jail[len("jailuser"):])
        self.chroot_cmd = "sudo -u {0} schroot -u {0} -c {0} -d {1} -- jailguard.py ".format(
                self.name, "/home/jailuser")

        self._is_alive = False
        self.command_process = None
        self.resp_queue = Queue()
        self.stdout_queue = Queue()
        self.stderr_queue = Queue()
        self._prepare_with(working_directory)

    def __del__(self):
        if self.locked:
            raise SandboxError("Jail object for %s freed without being released"
                    % (self.name))

    @property
    def is_alive(self):
        """Indicates whether a command is currently running in the sandbox"""
        if self._is_alive:
            sub_result = self.command_process.poll()
            if sub_result is None:
                return True
            self._is_alive = False
        return False

    def release(self):
        """Release the sandbox for further use

        Unlocks and releases the jail for reuse by others.
        Must be called exactly once after Jail.is_alive == False.

        """
        if self.is_alive:
            raise SandboxError("Sandbox released while still alive")
        if not self.locked:
            raise SandboxError("Attempt to release jail that is already unlocked")
        if os.system("sudo umount %s" % (os.path.join(self.base_dir, "root"),)):
            raise SandboxError("Error returned from umount of jail %d"
                    % (self.number,))
        lock_dir = os.path.join(self.base_dir, "locked")
        pid_filename = os.path.join(lock_dir, "lock.pid")
        with open(pid_filename, 'r') as pid_file:
            lock_pid = int(pid_file.read())
            if lock_pid != os.getpid():
                # if we ever get here something has gone seriously wrong
                # most likely the jail locking mechanism has failed
                raise SandboxError("Jail released by different pid, name %s, lock_pid %d, release_pid %d"
                        % (self.name, lock_pid, os.getpid()))
        os.unlink(pid_filename)
        os.rmdir(lock_dir)
        self.locked = False

    def _prepare_with(self, command_dir):
        if os.system("%s c %d" % (self.jchown, self.number)) != 0:
            raise SandboxError("Error returned from jail_own c %d in prepare"
                    % (self.number,))
        scratch_dir = os.path.join(self.base_dir, "scratch")
        if os.system("rm -rf %s" % (scratch_dir,)) != 0:
            raise SandboxError("Could not remove old scratch area from jail %d"
                    % (self.number,))
        home_dir = os.path.join(scratch_dir, "home/jailuser")
        os.makedirs(os.path.join(scratch_dir, "home"))
        if os.system("cp -r %s %s" % (command_dir, home_dir)) != 0:
            raise SandboxError("Error copying working directory '%s' to jail %d"
                    % (command_dir, self.number))
        if os.system("sudo mount %s" % (os.path.join(self.base_dir, "root"),)):
            raise SandboxError("Error returned from mount of %d in prepare"
                    % (self.number,))
        if os.system("%s j %d" % (self.jchown, self.number)) != 0:
            raise SandboxError("Error returned from jail_own j %d in prepare"
                    % (self.number,))
        self.home_dir = home_dir
        self.command_dir = command_dir

    def retrieve(self):
        """Copy the working directory back out of the sandbox."""
        if self.is_alive:
            raise SandboxError("Tried to retrieve sandbox while still alive")
        os.system("rm -rf %s" % (self.command_dir,))
        if os.system("%s c %d" % (self.jchown, self.number)) != 0:
            raise SandboxError("Error returned from jail_own c %d in prepare"
                    % (self.number,))
        os.system("cp -r %s %s" % (self.home_dir, self.command_dir))

    def start(self, shell_command):
        """Start a command running in the sandbox"""
        if self.is_alive:
            raise SandboxError("Tried to run command with one in progress.")
        shell_command = self.chroot_cmd + shell_command
        shell_command = shlex.split(shell_command.replace('\\','/'))
        try:
            self.command_process = subprocess.Popen(shell_command,
                                                    stdin=subprocess.PIPE,
                                                    stdout=subprocess.PIPE)
        except OSError:
            raise SandboxError('Failed to start {0}'.format(shell_command))
        self._is_alive = True
        monitor = Thread(target=_guard_monitor, args=(self,))
        monitor.daemon = True
        monitor.start()

    def _signal(self, signal):
        if not self.locked:
            raise SandboxError("Attempt to send %s to unlocked jail" % (signal,))
        result = subprocess.call("sudo -u {0} kill -{1} -1".format(
            self.name, signal), shell=True)
        if result != 0:
            raise SandboxError("Error returned from jail %s sending signal %s"
                    % (self.name, signal))

    def kill(self):
        """Stops the sandbox.

        Stops down the sandbox, cleaning up any spawned processes, threads, and
        other resources. The shell command running inside the sandbox may be
        suddenly terminated.

        """
        try:
            self.command_process.stdin.write("KILL\n")
            self.command_process.stdin.flush()
        except IOError as exc:
            if exc.errno != 32:
                raise
        try:
            item = self.resp_queue.get(timeout=5)
            if item[1] != "KILL" and item[1] is not None:
                raise SandboxError("Bad response from jailguard after kill, %s"
                        % (item,))
        except Empty:
            pass
        self._signal("CONT")
        for i in range(20):
            if self.command_process.poll() != None:
                break
            if i == 10:
                self._signal("KILL")
            time.sleep(0.1)

        # final check to make sure processes are died and raise error if not
        if self.is_alive:
            raise SandboxError("Could not kill sandbox children")

    def pause(self):
        """Pause the process by sending a SIGSTOP to the child"""
        try:
            self.command_process.stdin.write("STOP\n")
            self.command_process.stdin.flush()
        except IOError as exc:
            if exc.errno == 32: # Broken pipe, guard exited
                return
            raise
        item = self.resp_queue.get()
        if item[1] != "STOP" and item[1] is not None:
            raise SandboxError("Bad response from jailguard after pause, %s"
                    % (item,))


    def resume(self):
        """Resume the process by sending a SIGCONT to the child"""
        try:
            self.command_process.stdin.write("CONT\n")
            self.command_process.stdin.flush()
        except IOError as exc:
            if exc.errno == 32: # Broken pipe, guard exited
                return
            raise
        item = self.resp_queue.get()
        if item[1] != "CONT" and item[1] is not None:
            raise SandboxError("Bad response from jailguard after resume, %s"
                    % (item,))

    def write(self, data):
        """Write str to stdin of the process being run"""
        for line in data.splitlines():
            self.write_line(line)

    def write_line(self, line):
        """Write line to stdin of the process being run

        A newline is appended to line and written to stdin of the child process

        """
        if not self.is_alive:
            return False
        try:
            self.command_process.stdin.write("SEND %s\n" % (line,))
            self.command_process.stdin.flush()
        except (OSError, IOError):
            self.kill()

    def read_line(self, timeout=0):
        """Read line from child process

        Returns a line of the child process' stdout, if one isn't available
        within timeout seconds it returns None. Also guaranteed to return None
        at least once after each command that is run in the sandbox.

        """
        if not self.is_alive:
            timeout=0
        try:
            time, line = self.stdout_queue.get(block=True, timeout=timeout)
            return line
        except Empty:
            return None

    def read_error(self, timeout=0):
        """Read line from child process' stderr

        Returns a line of the child process' stderr, if one isn't available
        within timeout seconds it returns None. Also guaranteed to return None
        at least once after each command that is run in the sandbox.

        """
        if not self.is_alive:
            timeout=0
        try:
            time, line = self.stderr_queue.get(block=True, timeout=timeout)
            return line
        except Empty:
            return None

    def check_path(self, path, errors):
        resolved_path = os.path.join(self.home_dir, path)
        if not os.path.exists(resolved_path):
            errors.append("Output file " + str(path) + " was not created.")
            return False
        else:
            return True


def _monitor_file(fd, q):
    while True:
        line = fd.readline()
        if not line:
            q.put(None)
            break
        line = unicode(line, errors="replace")
        line = line.rstrip('\r\n')
        q.put(line)

class House:
    """Provide an insecure sandbox to run arbitrary commands in.

    The sandbox class is used to invoke arbitrary shell commands.
    This class provides the same interface as the secure Sandbox but doesn't
    provide any actual security or require any special system setup.

    """

    def __init__(self, working_directory):
        """Initialize a new sandbox for the given working directory.

        working_directory: the directory in which the shell command should
                           be launched.
        """
        self._is_alive = False
        self.command_process = None
        self.stdout_queue = Queue()
        self.stderr_queue = Queue()
        self.working_directory = working_directory

    @property
    def is_alive(self):
        """Indicates whether a command is currently running in the sandbox"""
        if self._is_alive:
            sub_result = self.command_process.poll()
            if sub_result is None:
                return True
            self.child_queue.put(None)
            self._is_alive = False
        return False

    def start(self, shell_command):
        """Start a command running in the sandbox"""
        if self.is_alive:
            raise SandboxError("Tried to run command with one in progress.")
        working_directory = self.working_directory
        self.child_queue = Queue()
        shell_command = shlex.split(shell_command.replace('\\','/'))
        try:
            self.command_process = subprocess.Popen(shell_command,
                                                    stdin=subprocess.PIPE,
                                                    stdout=subprocess.PIPE,
                                                    stderr=subprocess.PIPE,
                                                    universal_newlines=True,
                                                    cwd=working_directory)
        except OSError:
            raise SandboxError('Failed to start {0}'.format(shell_command))
        self._is_alive = True
        stdout_monitor = Thread(target=_monitor_file,
                                args=(self.command_process.stdout, self.stdout_queue))
        stdout_monitor.daemon = True
        stdout_monitor.start()
        stderr_monitor = Thread(target=_monitor_file,
                                args=(self.command_process.stderr, self.stderr_queue))
        stderr_monitor.daemon = True
        stderr_monitor.start()
        Thread(target=self._child_writer).start()

    def kill(self):
        """Stops the sandbox.

        Shuts down the sandbox, cleaning up any spawned processes, threads, and
        other resources. The shell command running inside the sandbox may be
        suddenly terminated.

        """
        if self.is_alive:
            try:
                self.command_process.kill()
            except OSError:
                pass
            self.command_process.wait()
            self.child_queue.put(None)

    def retrieve(self):
        """Copy the working directory back out of the sandbox."""
        if self.is_alive:
            raise SandboxError("Tried to retrieve sandbox while still alive")
        pass

    def release(self):
        """Release the sandbox for further use

        If running in a jail unlocks and releases the jail for reuse by others.
        Must be called exactly once after Sandbox.kill has been called.

        """
        if self.is_alive:
            raise SandboxError("Sandbox released while still alive")
        pass

    def pause(self):
        """Pause the process by sending a SIGSTOP to the child

        A limitation of the method is it will only pause the initial
        child process created any further (grandchild) processes created
        will not be paused.

        This method is a no-op on Windows.
        """
        try:
            self.command_process.send_signal(signal.SIGSTOP)
        except (ValueError, AttributeError, OSError):
            pass

    def resume(self):
        """Resume the process by sending a SIGCONT to the child

        This method is a no-op on Windows
        """
        try:
            self.command_process.send_signal(signal.SIGCONT)
        except (ValueError, AttributeError, OSError):
            pass

    def _child_writer(self):
        queue = self.child_queue
        stdin = self.command_process.stdin
        while True:
            ln = queue.get()
            if ln is None:
                break
            try:
                stdin.write(ln)
                stdin.flush()
            except (OSError, IOError):
                self.kill()
                break

    def write(self, str):
        """Write str to stdin of the process being run"""
        if not self.is_alive:
            return False
        self.child_queue.put(str)

    def write_line(self, line):
        """Write line to stdin of the process being run

        A newline is appended to line and written to stdin of the child process

        """
        if not self.is_alive:
            return False
        self.child_queue.put(line + "\n")

    def read_line(self, timeout=0):
        """Read line from child process

        Returns a line of the child process' stdout, if one isn't available
        within timeout seconds it returns None. Also guaranteed to return None
        at least once after each command that is run in the sandbox.

        """
        if not self.is_alive:
            timeout=0
        try:
            return self.stdout_queue.get(block=True, timeout=timeout)
        except Empty:
            return None

    def read_error(self, timeout=0):
        """Read line from child process' stderr

        Returns a line of the child process' stderr, if one isn't available
        within timeout seconds it returns None. Also guaranteed to return None
        at least once after each command that is run in the sandbox.

        """
        if not self.is_alive:
            timeout=0
        try:
            return self.stderr_queue.get(block=True, timeout=timeout)
        except Empty:
            return None

    def check_path(self, path, errors):
        resolved_path = os.path.join(self.working_directory, path)
        if not os.path.exists(resolved_path):
            errors.append("Output file " + str(path) + " was not created.")
            return False
        else:
            return True

def get_sandbox(working_dir, secure=None):
    if secure is None:
        secure = _SECURE_DEFAULT
    if secure:
        return Jail(working_dir)
    else:
        return House(working_dir)

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
    parser.add_option("-r", "--receive-wait", action="store",
            dest="resp_wait", type="float", default=600,
            help="Time in seconds to wait for another response line")
    parser.add_option("-j", "--jail", action="store_true", dest="secure",
            default=_SECURE_DEFAULT,
            help="Run in a secure jail")
    parser.add_option("-o", "--open", action="store_false", dest="secure",
            help="Run without using a secure jail")
    options, args = parser.parse_args()
    if len(args) == 0:
        parser.error("Must include a command to run.\
                \nRun with --help for more information.")

    print("Using secure sandbox: %s" % (options.secure))
    print("Sandbox working directory: %s" % (options.working_dir))
    sandbox = get_sandbox(options.working_dir, secure=options.secure)
    try:
        print()
        sandbox.start(" ".join(args))
        for line in options.send_lines:
            sandbox.write_line(line)
            print("sent: " + line)
            time.sleep(options.send_delay)
        while True:
            response = sandbox.read_line(options.resp_wait)
            if response is None:
                print()
                print("No more responses. Terminating.")
                break
            print("response: " + response)
        sandbox.kill()
    finally:
        sandbox.release()

if __name__ == "__main__":
    main()

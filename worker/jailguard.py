#!/usr/bin/python
# relay stdio to and from subprocess
# send stop and continue signal to subprocess and any processes the child starts

import os
import sys
import time
from Queue import Queue, Empty
from threading import Thread
from signal import SIGSTOP, SIGCONT, SIGKILL
from subprocess import Popen, PIPE

# Seconds between updating potential child processes
_UPDATE_INTERVAL = 0.5

def _get_active_pids():
    return [int(pid) for pid in os.listdir("/proc") if pid.isdigit()]

class Guard(object):
    def __init__(self, args):
        self.checked_pids = set(_get_active_pids())
        self.child_pids = set()
        self.running = True

        self.out_queue = Queue()
        self.child_queue = Queue()
        self.child = Popen(args, stdin=PIPE, stdout=PIPE, stderr=PIPE)
        self.checked_pids.add(self.child.pid)
        self.child_pids.add(self.child.pid)
        self.child_streams = 2
        Thread(target=self.reader, args=("STDOUT", self.child.stdout)).start()
        Thread(target=self.reader, args=("STDERR", self.child.stderr)).start()
        Thread(target=self.writer).start()
        Thread(target=self.child_writer).start()
        cmd_thread = Thread(target=self.cmd_loop, args=(sys.stdin,))
        cmd_thread.daemon = True
        cmd_thread.start()

    def writer(self):
        queue = self.out_queue
        while self.running or self.child_streams or not queue.empty():
            item = queue.get()
            if item:
                sys.stdout.write("%s %f %s\n" % item)
                sys.stdout.flush()

    def reader(self, name, pipe):
        queue = self.out_queue
        try:
            while True:
                ln = pipe.readline()
                if not ln:
                    break
                queue.put((name, time.time(), ln[:-1]))
        finally:
            self.child_streams -= 1
            queue.put(None)

    def child_writer(self):
        queue = self.child_queue
        stdin = self.child.stdin
        while True:
            ln = queue.get()
            if ln is None:
                break
            try:
                stdin.write(ln)
                stdin.flush()
            except IOError as exc:
                if exc.errno == 32:
                    break
                raise

    def signal_children(self, sig):
        cpids = frozenset(self.child_pids)
        for pid in cpids:
            try:
                os.kill(pid, sig)
            except OSError as exc:
                if exc.errno == 3:
                    self.child_pids.remove(pid)
                    self.checked_pids.remove(pid)
                else:
                    raise

    def cmd_loop(self, pipe):
        while True:
            cmd = pipe.readline()
            if not cmd or cmd == "EXIT\n":
                self.kill()
                break
            elif cmd == "STOP\n":
                self.signal_children(SIGSTOP)
                self.out_queue.put(("SIGNALED", time.time(), "STOP"))
            elif cmd == "CONT\n":
                self.signal_children(SIGCONT)
                self.out_queue.put(("SIGNALED", time.time(), "CONT"))
            elif cmd == "KILL\n":
                self.signal_children(SIGKILL)
                self.out_queue.put(("SIGNALED", time.time(), "KILL"))
            elif cmd.startswith("SEND"):
                self.child_queue.put(cmd[5:])
            else:
                self.kill()
                raise ValueError("Unrecognized input found '%s'" % (cmd,))

    def kill(self):
        try:
            self.child.kill()
        except OSError as exc:
            if exc.errno != 3:
                raise
        self.running = False

    def run(self):
        checked = self.checked_pids
        uid = os.getuid()
        while self.child.poll() is None:
            pids = [pid for pid in _get_active_pids() if pid not in checked]
            checked.update(pids)
            cpids = []
            for pid in pids:
                try:
                    if os.stat("/proc/%d" % (pid,)).st_uid == uid:
                        cpids.append(pid)
                except OSError as exc:
                    if exc.errno != 2:
                        raise
            self.child_pids.update(cpids)
            time.sleep(_UPDATE_INTERVAL)
        self.running = False
        self.out_queue.put(None)
        self.child_queue.put(None)

if __name__ == "__main__":
    g = Guard(sys.argv[1:])
    g.run()


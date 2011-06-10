#!/usr/bin/python
# relay stdio to and from subprocess
# send stop and continue signal to subprocess and any processes the child starts

import os
import sys
import time
from Queue import Queue, Empty
from threading import Thread
from signal import SIGSTOP, SIGCONT
from subprocess import Popen, PIPE

# Seconds between updating potential child processes
_UPDATE_INTERVAL = 0.5

def _get_active_pids():
    return [int(pid) for pid in os.listdir("/proc") if pid.isdigit()]

class Guard(object):
    def __init__(self, args):
        self.checked_pids = set(_get_active_pids())
        self.child_pids = set()
        self.uid = os.getuid()
        self.running = True

        self.out_queue = Queue()
        self.child = Popen(args, stdin=PIPE, stdout=PIPE, stderr=PIPE)
        self.checked_pids.add(self.child.pid)
        self.child_pids.add(self.child.pid)
        Thread(target=self.reader, args=("STDOUT", self.child.stdout)).start()
        Thread(target=self.reader, args=("STDERR", self.child.stderr)).start()
        Thread(target=self.writer).start()
        Thread(target=self.pid_monitor).start()
        cmd_thread = Thread(target=self.cmd_loop, args=(sys.stdin,))
        cmd_thread.daemon = True
        cmd_thread.start()

    def pid_monitor(self):
        checked = self.checked_pids
        child_pids = self.child_pids
        uid = self.uid
        while self.running:
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
            child_pids.update(cpids)
            time.sleep(_UPDATE_INTERVAL)

    def writer(self):
        queue = self.out_queue
        while self.running or not queue.empty():
            try:
                item = queue.get(timeout=1)
            except Empty:
                item = None
            if item:
                print "%s %f %s" % item
                sys.stdout.flush()

    def reader(self, name, pipe):
        queue = self.out_queue
        while self.running:
            ln = pipe.readline()
            if not ln:
                break
            queue.put((name, time.time(), ln[:-1]))

    def cmd_loop(self, pipe):
        while True:
            cmd = pipe.readline()
            if not cmd or cmd == "EXIT\n":
                self.kill()
                break
            elif cmd == "STOP\n":
                for pid in self.child_pids:
                    try:
                        os.kill(pid, SIGSTOP)
                    except OSError as exc:
                        if exc.errno == 3:
                            self.child_pids.remove(pid)
                            self.checked_pids.remove(pid)
                        else:
                            raise
                self.out_queue.put(("SIGNALED", time.time(), "STOP"))
            elif cmd == "CONT\n":
                for pid in self.child_pids:
                    try:
                        os.kill(pid, SIGCONT)
                    except OSError as exc:
                        if exc.errno == 3:
                            self.child_pids.remove(pid)
                            self.checked_pids.remove(pid)
                        else:
                            raise
                self.out_queue.put(("SIGNALED", time.time(), "CONT"))
            elif cmd.startswith("SEND"):
                self.child.stdin.write(cmd[5:])
                self.child.stdin.flush()
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
        self.child.wait()
        self.kill()
        self.out_queue.put(None)

if __name__ == "__main__":
    g = Guard(sys.argv[1:])
    g.run()


import os
import signal
import subprocess
import sys
import time
process = subprocess.Popen(["python", "client.py"],
                           stdin=subprocess.PIPE,
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
stdout_monitor_process = os.fork()
if stdout_monitor_process == 0:
  while True:
    line = process.stdout.readline()
    print "stdout:", line.strip()
stderr_monitor_process = os.fork()
if stderr_monitor_process == 0:
  while True:
    line = process.stderr.readline()
    print "stderr:", line.strip()
start_time = time.clock()
while time.clock() - start_time < 1.0:
  pass
print "One second of the master's CPU time has elapsed. Shutting down."
os.kill(stdout_monitor_process, signal.SIGKILL)
os.kill(stderr_monitor_process, signal.SIGKILL)
os.kill(process.pid, signal.SIGKILL)


#!/usr/bin/python
import os
import time
import signal
import sys

sadbox_path = sys.argv[1]
sadbox_dir = os.path.basename(sadbox_path)
img_name = "test.img"
qemu_cmd = "/usr/bin/qemu"
qemu_netopt = "-net" 
qemu_nicopt = "nic" 
qemu_hostfwd = "user,hostfwd=tcp:127.0.0.1:5555-:22"
qemu_identfile = "vmkey"
dest_path = "contestvm@localhost:~"
cmd = "cd ./" + sadbox_dir + "/;" +  sys.argv[2]

vmpid = os.fork()
if (vmpid == 0):
	os.execl(qemu_cmd, qemu_cmd, img_name, qemu_netopt, qemu_nicopt, qemu_netopt, qemu_hostfwd)
print "spinning up qemu instance"
time.sleep(15) #allow time for vm to spin up
pid = os.fork()
if (pid == 0):
	os.execlp("scp", "", "-r", "-P 5555", "-i", qemu_identfile, sadbox_path, dest_path)
print "copying files to instance"
os.wait()
pid = os.fork()
if (pid == 0):
	os.execlp("ssh", "", "-p 5555", "-i",  qemu_identfile, "contestvm@localhost", cmd)
print "executing command inside VM"
os.wait() #wait for command to finish execution
os.kill(vmpid, signal.SIGINT)
os.wait() #wait for vm to die

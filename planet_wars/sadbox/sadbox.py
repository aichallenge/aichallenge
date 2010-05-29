#!/usr/bin/python
import os
import time
import signal
import sys
import getopt
import subprocess
import shlex

def copy_exec_dir(qemu_port, qemu_identfile, sadbox_path, dest_path):
	scp_cmd = "scp -r -P " + str(qemu_port) + " -i " + qemu_identfile + " " + sadbox_path + " " + dest_path
	scp_args = shlex.split(scp_cmd)
	scp_process = subprocess.Popen(scp_args, close_fds=True)
	print "copying files to instance"
	scp_process.wait()
	return scp_process
#...



def launch_qemu():
	img_name = "test.img"
	qemu_port = 5555
        qemu_cmd = "/usr/bin/qemu " + img_name + " -net nic -net user,hostfwd=tcp:127.0.0.1:" + str(qemu_port) + "-:22 -nographic"
	qemu_args = shlex.split(qemu_cmd)
	print "spinning up qemu instance"
	qemu_process = subprocess.Popen(qemu_args, stdin=subprocess.PIPE, stdout=subprocess.PIPE, close_fds=True)
	time.sleep(1)
	qemu_process.poll
	while qemu_process.returncode != None and qemu_port < 5565:
		qemu_port += 1
        	qemu_cmd = "/usr/bin/qemu " + img_name + " -net nic -net user,hostfwd=tcp:127.0.0.1:" + str(qemu_port) + "-:22 -nographic"
		qemu_args = shlex.split(qemu_cmd)
		qemu_process = subprocess.Popen(qemu_args, stdin=subprocess.PIPE, stdout=subprocess.PIPE, close_fds=True)
		time.sleep(1)
	time.sleep(10) #allow time for vm to spin up
	return (qemu_process, qemu_port)
#...

def main():
	qemu_identfile = "sshkey/vmkey"
	dest_path = "contestvm@localhost:~"
	try:
		opts, args = getopt.getopt(sys.argv[1:], "d:c:", ["directory=", "command="])
	except getopt.GetoptError, err:
		print str(err)
		sys.exit(2)
	sadbox_path = None
	cmd = None
	for o, a in opts:
		if o in ("-d", "--directory"):
			sadbox_path = a
		elif o in ("-c", "--command"):
			cmd = a
		else:
			print o, a
	sadbox_dir = os.path.basename(sadbox_path)
	cmd = "cd ./" + sadbox_dir + "/; " + cmd
	print sadbox_path, sadbox_dir, cmd
	(qemu_proc, port) = launch_qemu()
	scp_proc = copy_exec_dir(port, qemu_identfile, sadbox_path, dest_path)
	child_cmd = "ssh -p " + str(port) + " -i " + qemu_identfile + " contestvm@localhost " + cmd
	child_args = shlex.split(child_cmd)
	child_process = subprocess.Popen(child_args, close_fds=True)
	print "executing command inside VM"

	child_process.wait()
	#wait for command to finish execution
	os.kill(qemu_proc.pid, signal.SIGINT)
	qemu_proc.wait() #wait for vm to die
#...

if __name__ == "__main__":
	main()

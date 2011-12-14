# This script is purposefully left non-executable as it can easily have
# unintended, hard to reverse effects on a server
#
# This script is meant to be run on a fresh install of ubuntu (such as a newly
# instantiated Ec2 instance) and will create a user, download the contest
# code and run the worker_setup.py script so it can then finish setting up a
# contest worker instance.
#
# usage: worker_init.py download_url worker_api_key

WARNING = "WARNING, this script will make invasive changes to your system!"

import os
import os.path
import sys
from subprocess import Popen

def run_cmd(cmd):
    """ Run a command in a shell """
    print "Executing:", cmd
    proc = Popen(cmd, shell=True)
    status = proc.wait()
    if status != 0:
        raise Exception("Command %s exited with %d" % (cmd, status))

def setup_contest_user():
    """ Setup the contest user that all worker scripts run under """
    if not os.path.exists("/home/contest"):
        run_cmd('adduser --disabled-password --gecos "" contest')

def get_contest_files(download_url):
    """ Get the contest files downloaded and placed in the current directory """
    if os.path.exists("aichallenge"):
        return
    run_cmd("wget %s/worker-src.tgz" % (download_url,))
    run_cmd("tar -xf worker-src.tgz")
    run_cmd("rm worker-src.tgz")

def main():
    if len(sys.argv) < 3:
        print "usage: %s api_base_url worker_api_key [source_url]"
        print WARNING
        sys.exit(1)
    setup_contest_user()
    os.chdir("/home/contest")
    get_contest_files(sys.argv[1])
    os.chdir("aichallenge/setup")
    setup_cmd = "./worker_setup.py -y --username contest --api-url %s \
            --api-key %s --install-cronjob --start" % (sys.argv[1], sys.argv[2])
    if len(sys.argv) > 3:
        setup_cmd += " " + " ".join(sys.argv[3:])
    run_cmd(setup_cmd)

if __name__=="__main__":
    main()


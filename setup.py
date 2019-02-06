#!/usr/bin/python

import os
from subprocess import Popen, PIPE

def run_cmd(cmd, capture_stdout=False):
    """ Run a command in a shell """
    print "Executing:", cmd
    stdout_loc = PIPE if capture_stdout else None
    proc = Popen(cmd, shell=True, stdout=stdout_loc)
    output, error_out = proc.communicate()
    status = proc.wait()
    if status != 0:
        raise CmdError(cmd, status)
    return output

if __name__ == "__main__":
    try:
        os.chdir("ants/dist/starter_bots")
        run_cmd("make")
        run_cmd("make install")
        os.chdir("../../..")

        run_cmd("easy_install Markdown")
        run_cmd("easy_install Pygments")
        os.chdir("website")
        if not os.path.exists("aichallenge.wiki"):
            run_cmd("git clone git://github.com/aichallenge/aichallenge.wiki.git")
        run_cmd("python setup.py")
        os.chdir("..")

    except KeyboardInterrupt:
        print('Setup Aborted')

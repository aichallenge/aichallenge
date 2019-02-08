#!/usr/bin/python

import os
from subprocess import Popen, PIPE
from install_tools import CD, run_cmd

import create_worker_archive

if __name__ == "__main__":
    try:
        with CD("ants/dist/starter_bots"):
            run_cmd("make")
            run_cmd("make install")

        run_cmd("easy_install Markdown")
        run_cmd("easy_install Pygments")
        with CD("website"):
            if not os.path.exists("aichallenge.wiki"):
                run_cmd("git clone git://github.com/aichallenge/aichallenge.wiki.git")
            run_cmd("python setup.py")
        
            # if not os.path.exists("worker-src.tgz"):
            create_worker_archive.main(".")

    except KeyboardInterrupt:
        print('Setup Aborted')

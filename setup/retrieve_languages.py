#!/usr/bin/python
# Download third party language packages to the directory specified

import os.path
import sys
import urllib2
import urlparse

from install_tools import CD, run_cmd, CmdError

sources = [
    ("https://deb.nodesource.com/setup_11.x", "nodejs.deb")
]

if len(sys.argv) < 2:
    print "usage: %s <destination directory>"
    sys.exit()

out_dir = os.path.abspath(sys.argv[1])
if not os.path.isdir(out_dir):
    print "Destination directory does not exist"
    sys.exit(1)

with CD(out_dir):
    print "Downloading files to %s" % (out_dir,)
    for url, filename in sources:
        try:
            run_cmd("wget -U NewName/1.0 '%s' -O %s" % (url, filename))
        except CmdError, exc:
            print >>sys.stderr, str(exc)

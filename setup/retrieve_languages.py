#!/usr/bin/python
# Download third party language packages to the directory specified

import os.path
import sys
import urllib2
import urlparse

from install_tools import CD, run_cmd, CmdError

sources = [
    ("http://repo1.maven.org/maven2/org/clojure/clojure/1.3.0/clojure-1.3.0.zip",
        "clojure.zip"),
    ("https://github.com/jashkenas/coffee-script/tarball/1.1.2",
        "coffeescript.tgz"),
    ("http://ftp.digitalmars.com/dmd_2.054-0_amd64.deb",
        "dmd.deb"),
    ("https://github.com/downloads/aichallenge/aichallenge/golang_60.1-9753~natty1_amd64.deb",
        "golang.deb"),
    ("http://dist.groovy.codehaus.org/distributions/installers/deb/groovy_1.7.8-1_all.deb",
        "groovy.deb"),
    ("https://github.com/downloads/aichallenge/aichallenge/nodejs_0.4.10~natty1~ppa201107202043_amd64.deb",
        "nodejs.deb"),
    ("http://download.racket-lang.org/installers/5.2/racket/racket-5.2-bin-x86_64-linux-debian-lenny.sh", 
        "racket.sh"),
    ("http://www.scala-lang.org/downloads/distrib/files/scala-2.9.0.1.tgz",
        "scala.tgz"),
    ("https://github.com/downloads/aichallenge/aichallenge/dart-frogsh-r1499.tgz", "dart.tgz"),
    ("https://bitbucket.org/pypy/pypy/downloads/pypy-1.7-linux64.tar.bz2", "pypy.tar.bz2"),
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

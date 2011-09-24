#!/usr/bin/python
# Download third party language packages to the directory specified

import os.path
import sys
import urllib2
import urlparse

from install_tools import CD, run_cmd

sources = [
    ("http://cloud.github.com/downloads/clojure/clojure/clojure-1.2.0.zip",
        "clojure.zip"),
    ("https://github.com/jashkenas/coffee-script/tarball/1.1.2",
        "coffeescript.tgz"),
    ("http://ftp.digitalmars.com/dmd_2.054-0_amd64.deb",
        "dmd.deb"),
    ("https://launchpad.net/~gophers/+archive/go/+files/golang_60.1-0%7E9753%7Enatty1_amd64.deb",
        "golang.deb"),
    ("http://dist.groovy.codehaus.org/distributions/installers/deb/groovy_1.7.8-1_all.deb",
        "groovy.deb"),
    ("https://launchpad.net/~jerome-etienne/+archive/neoip/+files/nodejs_0.4.10%7Enatty1%7Eppa201107202043_amd64.deb",
        "nodejs.deb"),
    ("http://www.scala-lang.org/downloads/distrib/files/scala-2.9.0.1.tgz",
        "scala.tgz"),
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
        print "Downloading: %s" % (url,)
        response = urllib2.urlopen(url)
        with open(filename, 'w') as ofile:
            print "Saving as %s" % (filename,)
            ofile.write(response.read())


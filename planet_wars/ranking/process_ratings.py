#!/usr/bin/python

import sys

if len(sys.argv) < 2:
	print "usage:", sys.argv[0], "<results file> [output file]"
	sys.exit(1)

f=open(sys.argv[1], "r")
if len(sys.argv) > 2:
	g=open(sys.argv[2], "w")
else:
	g=sys.stdout

f=open("ratings.txt", "r")
for line in f:
	print >> g, ','.join(line.strip().split())


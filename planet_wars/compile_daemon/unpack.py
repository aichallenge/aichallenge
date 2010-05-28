# compile_submission.py
# Author: Jeff Cameron (jeff@jpcameron.com)
#
# Compiles a submission. The submission_id is passed to this
# script on the command line.

import os
import sys

print "Attempting to unpack submission."

# This is a list of the acceptable submission archive file names,
# as well as instructions for unpacking them.
zip_files = [
  ("entry.tar.gz", "tar xfz"),
  ("entry.tgz", "tar xfz"),
  ("entry.zip", "unzip")
]

# Unpack the submission file
found_archive_file = False
for file in zip_files:
  file_name = file[0]
  command = file[1]
  if os.path.exists(file_name):
    print "Found " + file_name
    os.system(command + " " + file_name)
    found_archive_file = True
    break

if not found_archive_file:
  sys.stderr.write("Didn't find zip, tar.gz, or tgz file!\n")
  sys.exit(1)

# If the zip file produced a directory, then move all the files
# from that directory back down into this directory.
os.system("mv */* . > /dev/null 2> /dev/null")

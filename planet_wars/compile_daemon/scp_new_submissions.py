# scp_new_submissions.py
# Author: Jeff Cameron (jeff@jpcameron.com)
#
# Looks in the database to see if there's any newly submitted zip files. If so
# then the newly submitted files are SCP-ed over in the contest container.
#
# These land in a directory in the main CSC NFS. However, we need to send these
# newly submitted zip files into the contest container, which does not have
# access to the main CSC NFS. 

import MySQLdb
import MySQLdb.cursors
import os
import sys

connection = MySQLdb.connect("localhost",
                             "contest2",
                             "d49pJ6GoCzueMWOfB7T6",
                             "contest2",
                             cursorclass=MySQLdb.cursors.DictCursor)
query = "SELECT * FROM contest_submissions WHERE status = 15"
cursor = connection.cursor()
cursor.execute(query)
rows = cursor.fetchall()
cursor.close()
for row in rows:
  submission_id = str(row['submission_id'])
  print "Copying submission " + submission_id + ":"
  command = "scp -rp ~/tron/entries/" + str(submission_id) + \
      " contest@contest:/home/contest/tron/entries/"
  print command
  os.system(command)
  command = "php ~/tron/submission_pipeline/update_submission_status.php " + \
      str(submission_id) + " 20"
  os.system(command)
  os.chmod("~/tron/entries/" + str(submission_id) + "/entry.zip", 600)
connection.close()

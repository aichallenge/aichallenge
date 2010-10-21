import MySQLdb
import os
from server_info import server_info
import sys

if len(sys.argv) != 2:
  print "USAGE: python delete_some_old_submissions.py <number-to-delete>"
  sys.exit(1)

connection = MySQLdb.connect(host = server_info["db_host"],
                             user = server_info["db_username"],
                             passwd = server_info["db_password"],
                             db = server_info["db_name"])
cursor = connection.cursor(MySQLdb.cursors.DictCursor)
cursor.execute("""
  select min(submission_id) as id
  from submissions
  where cleanup_status = 0
  group by user_id
  having min(submission_id) <> max(submission_id)
  limit """ + str(sys.argv[1]))
submissions = list(cursor.fetchall()) 
for s in submissions:
  id = s["id"]
  cmd = "rm -rf /home/contest/ai-contest/planet_wars/submissions/" + str(id)
  print cmd
  query = "update submissions set cleanup_status = 1 where " + \
    "submission_id = " + str(id)
  print query
  os.system(cmd)
  cursor.execute(query)

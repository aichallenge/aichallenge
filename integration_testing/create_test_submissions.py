#!/usr/bin/env python
import MySQLdb
import os
import random
from server_info import server_info
import sys

if len(sys.argv) != 2:
  print "USAGE: python create_test_submissions.py num_submissions"
  sys.exit(1)
n = int(sys.argv[1])
connection = MySQLdb.connect(host = server_info["db_host"],
                             user = server_info["db_username"],
                             passwd = server_info["db_password"],
                             db = server_info["db_name"])
cursor = connection.cursor(MySQLdb.cursors.DictCursor)
query = "SELECT user_id, username FROM users WHERE username LIKE 'testbot%'"
cursor.execute(query)
accounts = cursor.fetchall()
if len(accounts) == 0:
  print "ERROR: there are no test accounts in the database. You can " + \
    "create some using the create_test_accounts.py script."
  sys.exit(1);
for i in range(n):
  account = random.choice(accounts)
  cursor.execute("INSERT INTO submissions (user_id,status,timestamp," + \
    "language_id) VALUES (" + str(account["user_id"]) + ",20," + \
    "CURRENT_TIMESTAMP,3)")
  submission_id = connection.insert_id()
  path = "../submissions/" + str(submission_id) + "/"
  os.mkdir(path)
  os.chdir("entry")
  os.system("zip -r ../" + path + "entry.zip *.java > /dev/null 2> /dev/null")
  os.chdir("..")
cursor.close()
connection.close()

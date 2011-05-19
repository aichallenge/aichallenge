#!/usr/bin/env python
import sys
import MySQLdb
from server_info import server_info

connection = MySQLdb.connect(host = server_info["db_host"],
                             user = server_info["db_username"],
                             passwd = server_info["db_password"],
                             db = server_info["db_name"])
cursor = connection.cursor(MySQLdb.cursors.DictCursor)
query = "SELECT user_id, username FROM users WHERE username LIKE 'testbot%'"
cursor.execute(query)
accounts = cursor.fetchall()
if len(accounts) > 0:
  submission_ids = ",".join([str(acc["user_id"]) for acc in accounts])
  query = "DELETE FROM submissions WHERE user_id IN (" + submission_ids + ")"
  cursor.execute(query)
  cursor.execute("DELETE FROM users WHERE username LIKE 'testbot%'")
cursor.close()
connection.close()

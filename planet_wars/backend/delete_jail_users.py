import MySQLdb
import os
from server_info import server_info
import sys

userscript = """
  userdel [username];
  rm -rf /home/[username]/;
"""

def main(argv):
  if len(argv) != 1:
    print "USAGE: python delete_jail_users.py"
    return 0
  connection = MySQLdb.connect(host = server_info["db_host"],
                             user = server_info["db_username"],
                             passwd = server_info["db_password"],
                             db = server_info["db_name"])
  cursor = connection.cursor(MySQLdb.cursors.DictCursor)
  cursor.execute("SELECT * FROM jail_users")
  for user in cursor.fetchall():
    command = userscript.replace("[username]", user["username"])
    os.system(command)
  cursor.execute("DELETE FROM jail_users")
  cursor.close()
  connection.close()

if __name__ == "__main__":
  main(sys.argv)

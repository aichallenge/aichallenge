import MySQLdb
import os
from server_info import server_info
import sys

groupscript = """
  groupadd jailusers;
  echo "@jailusers hard nproc 10 # contest" >> /etc/security/limits.conf
  echo "@jailusers hard cpu 10 # contest" >> /etc/security/limits.conf
"""

userscript = """
  useradd -G jailusers -d /home/[username] -m [username];
  mkdir /home/[username]/.ssh;
  cp jail_id_rsa.pub /home/[username]/.ssh/authorized_keys;
  chown [username] /home/[username]/.ssh;
  #chown [username] /home/[username]/.ssh/authorized_keys;
  chmod 700 /home/[username]/.ssh;
  chmod 644 /home/[username]/.ssh/authorized_keys;
  iptables -A OUTPUT -p tcp -m owner --uid-owner [username] -j DROP;
"""

def main(argv):
  if len(argv) != 2:
    print "USAGE: python create_jail_users.py num_users_to_create"
    return 0
  n = int(argv[1])
  connection = MySQLdb.connect(host = server_info["db_host"],
                             user = server_info["db_username"],
                             passwd = server_info["db_password"],
                             db = server_info["db_name"])
  cursor = connection.cursor(MySQLdb.cursors.DictCursor)
  os.system(groupscript)
  for i in range(n):
    username = "jailuser" + str(i + 1)
    print "CREATING " + username
    command = userscript.replace("[username]", username)
    os.system(command)
    cursor.execute("INSERT INTO jail_users (username,in_use) VALUES ('" + \
       username + "',0)")
  cursor.close()
  connection.close()

if __name__ == "__main__":
  main(sys.argv)

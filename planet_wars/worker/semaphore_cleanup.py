
import os
import subprocess
from collections import defaultdict

import MySQLdb
from server_info import server_info

ipcs_proc = subprocess.Popen("ipcs -s", shell=True, stdout=subprocess.PIPE)
ipcs_out, ipcs_err = ipcs_proc.communicate()
ipcs_out = ipcs_out.splitlines()
user_semaphores = defaultdict(list)
for line in ipcs_out:
    values = line.split()
    if len(values) == 5 and values[2].startswith("jailuser"):
        user_semaphores[values[2]].append(values[1])

connection = MySQLdb.connect(host = server_info["db_host"],
        user = server_info["db_username"],
        passwd = server_info["db_password"],
        db = server_info["db_name"])
cursor = connection.cursor(MySQLdb.cursors.DictCursor)
query = "UPDATE jail_users SET in_use = 1 WHERE username = '%s' AND in_use = 0"

for jail_user, semaphores in user_semaphores.items():
    cursor.execute(query % (jail_user,))
    if connection.affected_rows() > 0:
        try:
            for semaphore in semaphores:
                os.system("ipcrm -s "+ semaphore)
        finally:
            cursor.execute(
                    "UPDATE jail_users SET in_use = 0 WHERE username = '%s'" %
                    (jail_user,))

cursor.close()
connection.close()


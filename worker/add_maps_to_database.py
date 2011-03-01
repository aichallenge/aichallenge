import sys
import MySQLdb
from server_info import server_info

n = int(sys.argv[1])
connection = MySQLdb.connect(host = server_info["db_host"],
                             user = server_info["db_username"],
                             passwd = server_info["db_password"],
                             db = server_info["db_name"])
cursor = connection.cursor(MySQLdb.cursors.DictCursor)
for i in range(1, n+1):
  query = "INSERT INTO maps (name,path,priority) VALUES ('Map " + str(i) + \
    "','map" + str(i) + ".txt',1)"
  print query
  cursor.execute(query)
cursor.close()
connection.close()

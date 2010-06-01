import sys
import MySQLdb
from server_info import server_info

connection = MySQLdb.connect(host = server_info["db_host"],
                             user = server_info["db_username"],
                             passwd = server_info["db_password"],
                             db = server_info["db_name"])
cursor = connection.cursor(MySQLdb.cursors.DictCursor)
cursor.execute("DELETE FROM users WHERE username LIKE 'testbot%'")
cursor.close()
connection.close()

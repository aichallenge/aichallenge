#!/usr/bin/python

import os
import sys

import MySQLdb
from server_info import server_info

def main(map_dir):
    map_files = os.listdir(map_dir)
    map_files = [m for m in map_files
            if m.startswith("map") and m.endswith(".txt")]
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor(MySQLdb.cursors.DictCursor)
    values = []
    for map in map_files:
        cursor.execute("SELECT count(1) FROM maps WHERE path ='%s'" % (map,))
        count = cursor.fetchone()['count(1)']
        if count > 0:
            continue
        values.append((os.path.splitext(map)[0], map, 1))
    if len(values) == 0:
        print "No new maps found"
        sys.exit()
    print "Adding %d maps to database" % (len(values),)
    values = ["('%s','%s','%s')" % (n,m,p) for n,m,p in values]
    values = ",".join(values)
    cursor.execute("INSERT INTO maps (name,path,priority) VALUES "+ values)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "usage: %s <map directory>" % sys.argv[0]
        sys.exit()
    main(sys.argv[1])


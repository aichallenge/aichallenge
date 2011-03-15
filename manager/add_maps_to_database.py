#!/usr/bin/python

import os
import sys

import MySQLdb
from server_info import server_info
from sql import sql

def main():
    # get list of all map files
    map_files = os.listdir(server_info["maps_path"])
    map_files = set([m for m in map_files if m.endswith(".map")])
    
    # get list of maps in database
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor()
    cursor.execute(sql["select_map_filenames"])
    db_maps = set([row[0] for row in cursor.fetchall()])
    
    # get maps not in database
    new_maps = map_files.difference(db_maps)
    
    # add new maps to database with top priority
    cursor.execute(sql["update_map_priorities"])
    cursor.execute(sql["insert_map_filenames"], new_maps)

if __name__ == "__main__":
    main()


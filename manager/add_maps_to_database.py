#!/usr/bin/python

import os
import sys

import MySQLdb
from server_info import server_info
from sql import sql

def main():
    # get list of all map files
    maps_path = server_info["maps_path"]
    map_files = set()
    for root, dirs, files in os.walk(maps_path):
        for filepath in files:
            if filepath.endswith(".map"):
                map_files.add(os.path.join(root, filepath)[len(maps_path)+1:])
    
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
    if len(new_maps) > 0:
        cursor.execute(sql["update_map_priorities"])
        for mapfile in new_maps:
            players = 0
            with open(os.path.join(maps_path,mapfile), 'r') as f:
                for line in f:
                    if line.startswith('players'):
                        players = int(line.split()[1])
                        break
            if players:
                cursor.execute(sql["insert_map_filenames"], (mapfile, players))
            print(mapfile)
        connection.commit()
        print('{0} maps added to database'.format(len(new_maps)))
    else:
        print("No maps added, priorities not changed.")
    
if __name__ == "__main__":
    main()


#!/usr/bin/python

import os
import sys

import MySQLdb
from server_info import server_info
from sql import sql

def main():
    # get the list of workers
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor()
    cursor.execute(sql["select_workers"])
    print cursor.fetchall()
    
if __name__ == "__main__":
    main()


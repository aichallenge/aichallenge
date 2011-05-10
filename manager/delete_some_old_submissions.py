import MySQLdb
import os
from server_info import server_info
from sql import sql
import sys
import platform

def main():
    # get list of cached submissions
    cached_submissions = set(os.listdir(server_info["uploads_path"]))
    
    # get list of latest submissions
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor()
    cursor.execute(sql["get_latest_submissions"])
    latest_submissions = set([str(row[0]) for row in cursor.fetchall()])
    
    # get list of submissions to delete
    old_submissions = cached_submissions.different(latest_submissions)
    
    for id in old_submissions:
        if platform.system() == 'Windows':
            cmd = "rmdir /s /q " + os.path.join(server_info["uploads_path"], id)
            print cmd
            os.system(cmd)
        else:
            cmd = "rm -rf " + os.path.join(server_info["uploads_path"], id)
            print cmd
            os.system(cmd)

if __name__ == '__main__':
    main()
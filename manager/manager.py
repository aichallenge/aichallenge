#!/usr/bin/python

import MySQLdb
from server_info import server_info

def update_trueskill(game_id):
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor(MySQLdb.cursors.DictCursor)

    # get list of players and their mu/sigma values from the database
    
    cursor.execute('''
    ''')
    results = cursor.fetchall()
    

def reset_submissions():
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor(MySQLdb.cursors.DictCursor)
    
    cursor.execute('update submission set status = 20 where latest = 1')
    connection.commit()
    
def main():
    reset_submissions()
    
if __name__ == '__main__':
    main()
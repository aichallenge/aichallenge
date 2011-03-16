#!/usr/bin/python
import os
import sys
import zipfile
import MySQLdb
from server_info import server_info

def create_test_bot(name):
    bot_filename = os.path.join("../ants/bots/python", name + ".py")
    bot_support_filename = "../ants/bots/python/ants.py"
    if not os.path.exists(bot_filename):
        print("No bot named %s" % name)
        return False

    
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor(MySQLdb.cursors.DictCursor)
    
    # get next bot name number
    cursor.execute("""
    select username
    from user
    where username like '%s%%'
    """ % name)
    bot_id = max([int(row["username"][len(name):])
                 for row in cursor.fetchall()] or [0]) + 1
    
    # create user database entry
    cursor.execute("""
    insert into user
    values (null,'%s%s','$6$rounds=54321$hQd}`.j1e#X&PuN*$D8.wbEp6vwwLoC27GpiGVOFediuAWaGTQ2MPHD64i/bVGxtj0XNeRJeJRKVgDC/uTh.W2m5YoaoA6To1cJ7ZF/',
    '%s%s@ai-contest.com',1,'7b3f9842775fa9c9d489a3714e857580',0,'Test Account',11,current_timestamp(),0,0);
    """ % (name, bot_id, name, bot_id))
    user_id = cursor.lastrowid
    print("user_id: %s" % user_id)
    
    # create submission entry
    cursor.execute("""
    insert into submission (user_id, version, status, timestamp, language_id)  
    values (%s, 1, 20, current_timestamp(), 6)
    """ % (user_id))
    submission_id = cursor.lastrowid
    print("submission_id: %s" % submission_id)
    
    connection.commit()
    connection.close()
    
    # create submission file
    bot_dir = os.path.join(server_info['submissions_path'], str(submission_id))
    os.mkdir(bot_dir)

    bot_zip_filename = os.path.join(bot_dir, "entry.zip")
    with zipfile.ZipFile(bot_zip_filename, 'w') as bot_zip:
        bot_zip.write(bot_filename, 'MyBot.py')
        bot_zip.write(bot_support_filename, 'ants.py')

if __name__ == '__main__':
    create_test_bot(sys.argv[1])
#!/usr/bin/env python
import os
import sys
import random
import MySQLdb

sys.path.append("../manager")
from server_info import server_info

def create_test_data(user_count=10000, map_count=1000, game_count=30000, matchup_count=10):
    connection = MySQLdb.connect(host = server_info["db_host"],
                                 user = server_info["db_username"],
                                 passwd = server_info["db_password"],
                                 db = server_info["db_name"])
    cursor = connection.cursor(MySQLdb.cursors.DictCursor)
    
    # create loads of users
    for i in range(user_count):
        cursor.execute("""
        insert into user
        values (null,'TestUser%s','$6$rounds=54321$hQd}`.j1e#X&PuN*$D8.wbEp6vwwLoC27GpiGVOFediuAWaGTQ2MPHD64i/bVGxtj0XNeRJeJRKVgDC/uTh.W2m5YoaoA6To1cJ7ZF/',
        'TestUser%s@ai-contest.com',1,'7b3f9842775fa9c9d489a3714e857580',0,'Test Account',11,current_timestamp(),0,0);
        """ % (i, i))
    connection.commit()        
    
    # create layers of old and new submissions
    user_max = int(user_count * 0.95) # ensure a few users don't have submissions
    version = 1
    while user_max > 1:
        cursor.execute("""
        insert into submission (user_id, version, status, timestamp, language_id, latest)
        select user_id, %s, 40, CURRENT_TIMESTAMP, 6, 0
        from user
        order by user_id
        limit %s;
        """ % (version, user_max))
        user_max = int(user_max * 0.6)
        version += 1
    connection.commit()
    
    # set last submission as latest
    cursor.execute("""
    update submission
    set latest = 0;
    """)
    connection.commit()
    cursor.execute("""
    update submission
    inner join (
        select s.user_id, MAX(s.submission_id) as submission_id
        from submission s
        group by s.user_id
    ) as sub_max on sub_max.submission_id = submission.submission_id
    set latest = 1;
    """)
    
    # get valid user and submission data
    connection.commit()
    submission_id = {}
    cursor.execute("""
    select user_id, submission_id
    from submission
    where latest = 1
    """)
    rows = cursor.fetchall()
    for row in rows[:-10]: # ensure a few users don't have games
        submission_id[row["user_id"]] = row["submission_id"]
        
    # create loads of maps
    for i in range(map_count):
        cursor.execute("""
        insert into map (filename, priority, players)
        values ('map%s', %s, %s);
        """ % (i, random.randrange(1,10), random.randrange(2,26)))
    connection.commit()
    map_pool = []
    cursor.execute("""
    select map_id
    from map
    """)
    for row in cursor.fetchall():
        map_pool.append(row["map_id"])
    
    # create loads of games
    for i in range(game_count):
        seed_id = random.choice(list(submission_id.keys()))
        map_id = random.choice(map_pool)
        cursor.execute("""
        insert into game (seed_id, map_id, timestamp, worker_id, replay_path)
        values (%s, %s, CURRENT_TIMESTAMP(), 1, '');
        """ % (seed_id, map_id))
        game_id = cursor.lastrowid
        cursor.execute("""
        select players from map where map_id = %s
        """ % map_id)
        player_count = cursor.fetchone()['players']
        user_ids = random.sample(list(submission_id.keys()), player_count)
        if not seed_id in user_ids:
            user_ids[random.randrange(0,player_count-1)] = seed_id
        for player_id in range(player_count):
            #print(user_ids[player_id])
            #print(submission_id[user_ids[player_id]])
            cursor.execute("""
            insert into game_player (game_id, user_id, submission_id, player_id, game_rank, game_score, sigma_before, mu_before)
            values (%s, %s, %s, %s, 1, 1, 50.0, 16.6667);
            """ % (game_id, user_ids[player_id], submission_id[user_ids[player_id]], player_id))
    connection.commit()

    # create a few matchups
    for i in range(matchup_count):
        seed_id = random.choice(list(submission_id.keys()))
        map_id = random.choice(map_pool)
        cursor.execute("""
        insert into matchup (seed_id, map_id, worker_id)
        values (%s, %s, 1);
        """ % (seed_id, map_id))
        matchup_id = cursor.lastrowid
        cursor.execute("""
        select players from map where map_id = %s
        """ % map_id)
        player_count = cursor.fetchone()['players']
        user_ids = random.sample(list(submission_id.keys()), player_count)
        if not seed_id in user_ids:
            user_ids[random.randrange(0,player_count-1)] = seed_id
        for player_id in range(player_count):
            cursor.execute("""
            insert into matchup_player (matchup_id, user_id, submission_id, player_id)
            values (%s, %s, %s, %s);
            """ % (matchup_id, user_ids[player_id], submission_id[user_ids[player_id]], player_id))
    connection.commit()

    # create small set of lurkers with no submission
    for i in range(user_count/10):
        cursor.execute("""
        insert into user
        values (null,'TestUser%s','$6$rounds=54321$hQd}`.j1e#X&PuN*$D8.wbEp6vwwLoC27GpiGVOFediuAWaGTQ2MPHD64i/bVGxtj0XNeRJeJRKVgDC/uTh.W2m5YoaoA6To1cJ7ZF/',
        'TestUser%s@ai-contest.com',1,'7b3f9842775fa9c9d489a3714e857580',0,'Test Account',11,current_timestamp(),0,0);
        """ % (i, i))
    connection.commit()        
        
if __name__ == '__main__':
    create_test_data()
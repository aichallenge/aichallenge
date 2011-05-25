# this file gathers all sql into one place for ease of changing the database
sql = {
    # used in add_maps_to_database.py
    "select_map_filenames": "select filename from map",
    "update_map_priorities": "update map set priority = priority + 1",
    "insert_map_filenames": "insert into map (filename, players) values (%s, %s)",
    
    # used in delete_some_old_submissions.py
    "select_latest_submissions": "select submission_id from submission where latest = 1",
    
    # used in snapshot_rankings.py
    "select_rankings_detail": """
        select
        @leader := (select max(leaderboard_id) from leaderboard) as leaderboard_id, 
        s.user_id, s.submission_id, s.version,
        @count1 := @count1 + 1 as seq,
        if(s.latest > 0, @count2 := @count2 + 1 , null) as rank,
        0 as rank_change, s.mu as skill,
        s.mu - isnull((select skill
                from ranking
                where leaderboard_id = @leader - 1
                and submission_id = s.submission_id), 0) as skill_change,
        s.latest, s.timestamp as age
    from
        submission s,
        (select @count1 := 0) c1,
        (select @count2 := 0) c2
    where latest = 1
    or submission_id in (
        select submission_id
        from ranking
        where leaderboard_id = @leader - 1
        and seq in (
            select min(seq)
            from ranking
            where leaderboard_id = @leader - 1
            group by user_id
        )
    )
    order by s.mu desc, s.sigma asc, s.timestamp asc;""",
    
    # used in manager.py
    "select_game_players": "select gp.submission_id, gp.game_rank, s.mu, s.sigma, gp.mu_after from game_player gp inner join submission s on s.submission_id = gp.submission_id where gp.game_id = %s",
    
    "update_game_player_trueskill": """
        update game_player
        set mu_before = %s,
            sigma_before = %s,
            mu_after = %s,
            sigma_after = %s
        where game_id = %s and
              submission_id = %s""",
    
    "update_submission_trueskill": """
        update submission s
        inner join game_player gp
            on s.submission_id = gp.submission_id
        set s.mu = gp.mu_after,
            s.sigma = gp.sigma_after
        where game_id = %s;""",
    
    "insert_leaderboard": """
        insert into leaderboard (timestamp, algorithm_name, calculation_time, complete)
        values (now(), 'TrueSkill', 0, 1);""",
    
    "insert_leaderboard_data": """
        insert into ranking (leaderboard_id, user_id, submission_id, version, seq, rank, rank_change, skill, skill_change, latest, age)
        select
            @leader := (select max(leaderboard_id) from leaderboard) as leaderboard_id, 
            s.user_id, s.submission_id, s.version,
            @count1 := @count1 + 1 as seq,
            @rank := if(s.latest > 0, @count2 := @count2 + 1 , null) as rank,
            r.rank - @rank as rank_change, s.mu as skill, r.skill - s.mu as skill_change, s.latest, timediff(now(),s.timestamp) as age
        from
            submission s
            left outer join ranking r
                on s.submission_id = r.submission_id and r.leaderboard_id = @leader - 1,
            (select @count1 := 0) c1,
            (select @count2 := 0) c2
        where s.latest = 1
        or s.submission_id in (
            select submission_id
            from ranking
            where leaderboard_id = @leader - 1
            and seq in (
                select min(seq)
                from ranking
                where leaderboard_id = @leader - 1
                group by user_id
            )
        )
        order by s.mu desc, s.sigma asc, s.timestamp asc""",
    "update_sigma", """update submission
        set sigma = sigma + (0.1 * (
            select count(*) from submission where timestamp > (
                select max(timestamp) from leaderboard
            )
        ))
        where latest = 1"""
}

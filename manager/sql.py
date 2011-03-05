# this file gathers all sql into one place for ease of changing the database
sql = {
    # used in add_maps_to_database.py
    "select_map_filenames": "select filename from map",
    "update_map_priorities": "update map set priority = priority + 1",
    "insert_map_filenames": "insert into map (filename) values (?)",
    
    # used in delete_some_old_submissions.py
    "select_latest_submissions": "select submission_id from submission where latest = 1",
    
    # used in snapshot_rankings.py
    "insert_rankings_detail": """select
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
order by s.mu desc, s.sigma asc, s.timestamp asc"""
}
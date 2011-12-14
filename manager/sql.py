# this file gathers all sql into one place for ease of changing the database
sql = {
    # used in worker_ssh
    "select_workers" : "select worker_id, ip_address from worker order by worker_id desc",
    
    # used in add_maps_to_database.py
    "select_map_filenames": "select filename from map",
    "update_map_priorities": """
        update map set priority = priority + 1
            where priority >= 0
        """,
    "insert_map_filenames": "insert into map (filename, players, max_turns, timestamp) values (%s, %s, 1000, current_timestamp)",

    # used in delete_some_old_submissions.py
    "select_latest_submissions": "select submission_id from submission where latest = 1",
    
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
            s.mu_change = gp.mu_after - gp.mu_before,
            s.sigma = gp.sigma_after,
            s.sigma_change = gp.sigma_after - gp.sigma_before
        where game_id = %s;"""
}

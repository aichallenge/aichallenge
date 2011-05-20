<?php

$sql = array(
    "select_next_compile" => "select submission_id
                              from submission
                              where status = 20
                              or (status = 30 and worker_id = %s)
                              order by submission_id asc
                              limit 1;",
    "update_submission_compiling" => "update submission
                                      set status = 30,
                                          worker_id = %s
                                      where submission_id = %s;",
    "update_submission_success" => "update submission
                                      set status = 40,
                                          language_id = %s,
                                          latest = 1
                                      where worker_id = %s
                                      and submission_id = %s;",
    "update_submission_failure" => "update submission
                                      set status = %s,
                                          language_id = %s,
                                          errors = '%s'
                                      where worker_id = %s
                                      and submission_id = %s;",
    "select_submission_language_id" => "select language_id
                                        from language
                                        where name = '%s'",
    "insert_new_language" => "insert into language (name) values ('%s')",
    "select_next_matchup" => "select matchup.*, map.filename
                              from matchup
                              left join map on matchup.map_id = map.map_id
                              where worker_id is null
                              or worker_id = %s
                              order by matchup_id asc
                              limit 1;",
    "select_matchup_players" => "select *
                                 from matchup_player
                                 where matchup_id = %s
                                 order by player_id;",
    "select_game_metadata" => "select u.user_id, username, submission_id
                               from game g
                               inner join game_player gp
                                   on g.game_id = gp.game_id
                               inner join user u
                                   on u.user_id = gp.user_id
                               where g.game_id = %s
                               order by player_id;",
    "lock_matchup" => "update matchup
                       set worker_id = %s
                       where matchup_id = %s;",
    "update_matchup_failed" => "update matchup
                                set error = error + '%s'
                                where matchup_id = %s",
    "select_languages" => "select *
                           from language;",
    "select_player_skills" => "select player_id, sigma, mu
                              from matchup_player p
                              inner join submission s on p.submission_id = s.submission_id
                              where matchup_id = %s
                              order by player_id;",
    "insert_game_data" => "insert into game
                           select null, seed_id, map_id, current_timestamp, worker_id, null
                           from matchup
                           where matchup_id = %s;",
    "insert_game_player" => "insert into game_player
                             select %s, p.user_id, p.submission_id, player_id,
                             null, null, %s, %s,
                             null, null, null, null, 1
                             from matchup_player p
                             where matchup_id = %s
                             and player_id = %s;",
    "delete_matchup" => "delete from matchup where matchup_id = %s;",
    "delete_matchup_player" => "delete from matchup_player where matchup_id = %s;",
    "get_user_from_activation_code" => "select username from user where activation_code = '%s'",
    "activate_user" => "update user set activated = 1 where activation_code =  '%s';",
    "insert_new_submission" => "insert into submission (user_id, version, status, timestamp, language_id)
                               select user_row.user_id,
                                      coalesce(max(s.version), 0) + 1 as next_version,
                                      user_row.status,
                                      user_row.timestamp,
                                      user_row.language_id
                               from (select %s as user_id,
                                     20 as status,
                                     current_timestamp() as timestamp,
                                     0 as language_id) user_row
                               left outer join submission s
                                   on s.user_id = user_row.user_id
                               group by user_row.user_id,
                                        user_row.status,
                                        user_row.timestamp,
                                        user_row.language_id;",
    "select_rankings" => "
        select u.user_id, u.username,
               c.country_id, c.name as country, c.country_code, c.flag_filename,
               l.language_id, l.name as programming_language,
               o.org_id, o.name as org_name,
               r.*
        from ranking r
        inner join user u
            on r.user_id = u.user_id
        inner join organization o
            on u.org_id = o.org_id
        inner join submission s
            on r.submission_id = s.submission_id
        inner join language l
            on l.language_id = s.language_id
        inner join country c
            on u.country_id = c.country_id
        where r.leaderboard_id = (
            select max(leaderboard_id)
            from leaderboard
        )
        order by seq",
    "select_rankings_by_org" => "
        select u.user_id, u.username,
               c.country_id, c.name as country, c.country_code, c.flag_filename,
               l.language_id, l.name as programming_language,
               o.org_id, o.name as org_name,
               r.*
        from ranking r
        inner join user u
            on r.user_id = u.user_id
        inner join organization o
            on u.org_id = o.org_id
        inner join submission s
            on r.submission_id = s.submission_id
        inner join language l
            on l.language_id = s.language_id
        inner join country c
            on u.country_id = c.country_id
        where r.leaderboard_id = (
            select max(leaderboard_id)
            from leaderboard
        )
        and o.org_id = %s
        order by seq",
    "select_rankings_by_language" => "
        select u.user_id, u.username,
               c.country_id, c.name as country, c.country_code, c.flag_filename,
               l.language_id, l.name as programming_language,
               o.org_id, o.name as org_name,
               r.*
        from ranking r
        inner join user u
            on r.user_id = u.user_id
        inner join organization o
            on u.org_id = o.org_id
        inner join submission s
            on r.submission_id = s.submission_id
        inner join language l
            on l.language_id = s.language_id
        inner join country c
            on u.country_id = c.country_id
        where r.leaderboard_id = (
            select max(leaderboard_id)
            from leaderboard
        )
        and l.language_id = %s
        order by seq",
    "select_rankings_by_country" => "
        select u.user_id, u.username,
               c.country_id, c.name as country, c.country_code, c.flag_filename,
               l.language_id, l.name as programming_language,
               o.org_id, o.name as org_name,
               r.*
        from ranking r
        inner join user u
            on r.user_id = u.user_id
        inner join organization o
            on u.org_id = o.org_id
        inner join submission s
            on r.submission_id = s.submission_id
        inner join language l
            on l.language_id = s.language_id
        inner join country c
            on u.country_id = c.country_id
        where r.leaderboard_id = (
            select max(leaderboard_id)
            from leaderboard
        )
        and c.country_id = %s
        order by seq",
    "select_countries" => "select * from country",
    "select_languages" => "select * from language",
    "select_organizations" => "select * from organization",
    // holy ant colony batman! all games?
    "select_game_list" => "
        select g.game_id, g.timestamp,
               gp.user_id, gp.submission_id, u.username,
               gp.sigma_after as sigma, gp.mu_after as mu, gp.player_id, gp.game_rank,
               m.players, m.map_id, m.filename as map_name
        from game g
        inner join map m
            on m.map_id = g.map_id
        inner join game_player gp
            on g.game_id = gp.game_id
        inner join user u
            on gp.user_id = u.user_id
        order by game_id desc, gp.game_rank
    ",
    // yes robin. all games.
    "select_game_list_by_user" => "
        select g.game_id, g.timestamp,
               gp.user_id, gp.submission_id, u.username,
               gp.sigma_after as sigma, gp.mu_after as mu, gp.player_id, gp.game_rank,
               m.players, m.map_id, m.filename as map_name
        from game g
        inner join map m
            on m.map_id = g.map_id
        inner join game_player gp
            on g.game_id = gp.game_id
        inner join user u
            on gp.user_id = u.user_id
        where g.game_id in (
            select gp2.game_id
            from game_player gp2
            where gp2.user_id = %s
        )
        order by game_id desc, gp.game_rank
    ",
    "select_game_list_by_submission" => "
        select g.game_id, g.timestamp,
               gp.user_id, gp.submission_id, u.username,
               gp.sigma_after as sigma, gp.mu_after as mu, gp.player_id, gp.game_rank,
               m.players, m.map_id, m.filename as map_name
        from game g
        inner join map m
            on m.map_id = g.map_id
        inner join game_player gp
            on g.game_id = gp.game_id
        inner join user u
            on gp.user_id = u.user_id
        where g.game_id in (
            select gp2.game_id
            from game_player gp2
            where gp2.submission_id = %s
        )
        order by game_id desc, gp.game_rank
    ",
    "select_game_list_by_map" => "
        select g.game_id, g.timestamp,
               gp.user_id, gp.submission_id, u.username,
               gp.sigma_after as sigma, gp.mu_after as mu, gp.player_id, gp.game_rank,
               m.players, m.map_id, m.filename as map_name
        from game g
        inner join map m
            on m.map_id = g.map_id
        inner join game_player gp
            on g.game_id = gp.game_id
        inner join user u
            on gp.user_id = u.user_id
        where g.map_id = %s
        order by game_id desc, gp.game_rank
    "
);

?>
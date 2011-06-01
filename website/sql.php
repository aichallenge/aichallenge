<?php

$contest_sql = array(
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
    "update_submission_latest" => "update submission
                                     set latest = 0
                                     where user_id = (
                                        select user_id
                                        from (select * from submission) s
                                        where submission_id = %s
                                     )
                                     and submission_id != %s;",
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
    "select_game_metadata" => "select gp.user_id, u.username, gp.submission_id
                               from game_player gp
                               left outer join user u
                                   on u.user_id = gp.user_id
                               where gp.game_id = %s
                               order by gp.player_id;",
    "lock_matchup" => "update matchup
                       set worker_id = %s
                       where matchup_id = %s;",
    "update_matchup_failed" => "update matchup
                                set error = '%s',
                                    worker_id = -2
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
                             '%s', '%s', %s, %s,
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
    "select_rankings" => "select u.user_id, u.username,
               c.country_id, c.name as country, c.country_code, c.flag_filename,
               l.language_id, l.name as programming_language,
               o.org_id, o.name as org_name,
               r.*
        from ranking r
        inner join user u
            on r.user_id = u.user_id
        inner join submission s
            on r.submission_id = s.submission_id
        left outer join organization o
            on u.org_id = o.org_id
        left outer join language l
            on l.language_id = s.language_id
        left outer join country c
            on u.country_id = c.country_id
        where r.leaderboard_id = (
            select max(leaderboard_id)
            from leaderboard
        )
        order by seq",
    "select_rankings_by_org" => "select u.user_id, u.username,
               c.country_id, c.name as country, c.country_code, c.flag_filename,
               l.language_id, l.name as programming_language,
               o.org_id, o.name as org_name,
               r.*,
               if(r.rank is not null, @count1 := @count1 + 1 , null) as filter_rank
        from ranking r
        inner join user u
            on r.user_id = u.user_id
        inner join submission s
            on r.submission_id = s.submission_id
        left outer join organization o
            on u.org_id = o.org_id
        left outer join language l
            on l.language_id = s.language_id
        left outer join country c
            on u.country_id = c.country_id,
        (select @count1 := 0) c1
        where r.leaderboard_id = (
            select max(leaderboard_id)
            from leaderboard
        )
        and o.org_id = %s
        order by seq",
    "select_rankings_by_language" => "select u.user_id, u.username,
               c.country_id, c.name as country, c.country_code, c.flag_filename,
               l.language_id, l.name as programming_language,
               o.org_id, o.name as org_name,
               r.*,
               if(r.rank is not null, @count1 := @count1 + 1 , null) as filter_rank
        from ranking r
        inner join user u
            on r.user_id = u.user_id
        inner join submission s
            on r.submission_id = s.submission_id
        left outer join organization o
            on u.org_id = o.org_id
        left outer join language l
            on l.language_id = s.language_id
        left outer join country c
            on u.country_id = c.country_id,
        (select @count1 := 0) c1
        where r.leaderboard_id = (
            select max(leaderboard_id)
            from leaderboard
        )
        and l.language_id = %s
        order by seq",
    "select_rankings_by_country" => "select u.user_id, u.username,
               c.country_id, c.name as country, c.country_code, c.flag_filename,
               l.language_id, l.name as programming_language,
               o.org_id, o.name as org_name,
               r.*,
               if(r.rank is not null, @count1 := @count1 + 1 , null) as filter_rank
        from ranking r
        inner join user u
            on r.user_id = u.user_id
        inner join submission s
            on r.submission_id = s.submission_id
        left outer join organization o
            on u.org_id = o.org_id
        left outer join language l
            on l.language_id = s.language_id
        left outer join country c
            on u.country_id = c.country_id,
        (select @count1 := 0) c1
        where r.leaderboard_id = (
            select max(leaderboard_id)
            from leaderboard
        )
        and c.country_id = %s
        order by seq",
    "select_countries" => "select * from country",
    "select_languages" => "select * from language",
    "select_organizations" => "select * from organization",
    "select_users" => "select * from user",
    "select_game_list_page_count" => "select count(*)
        from game g
        inner join game_player gp
            on g.game_id = gp.game_id
        where %s = %s",
    "select_game_list" => "select g.game_id, g.timestamp,
           gp.user_id, gp.submission_id, u.username,
           gp.sigma_after as sigma, gp.mu_after as mu, gp.player_id, gp.game_rank,
           s.version,
           m.players, m.map_id, m.filename as map_name
     from (
         select *
         from game g2
         where g2.game_id in (
             select game_id
             from game_player gp2
             where %s = %s
         )
         order by g2.game_id desc
         limit %s offset %s
     ) g
     inner join map m
         on m.map_id = g.map_id
     inner join game_player gp
         on g.game_id = gp.game_id
     inner join user u
         on gp.user_id = u.user_id
     inner join submission s
        on gp.submission_id = s.submission_id
     order by g.game_id desc, gp.game_rank",
     "select_map_game_list_page_count" => "select count(*)
        from game g
        where %s = %s",
    "select_map_game_list" => "select g.game_id, g.timestamp,
           gp.user_id, gp.submission_id, u.username,
           gp.sigma_after as sigma, gp.mu_after as mu, gp.player_id, gp.game_rank,
           m.players, m.map_id, m.filename as map_name
     from (
         select *
         from game g2
         where %s = %s
         order by g2.game_id desc
         limit %s offset %s
     ) g
     inner join map m
         on m.map_id = g.map_id
     inner join game_player gp
         on g.game_id = gp.game_id
     inner join user u
         on gp.user_id = u.user_id
     order by g.game_id desc, gp.game_rank",
    "select_game_errors" => "select gp.user_id, gp.errors, gp.status, u.username
        from game_player gp
        inner join user u
            on u.user_id = gp.user_id
        where gp.game_id = %s
        and (gp.status = 'timeout'
            or gp.status = 'crashed'
            or gp.status = 'invalid')
    ",
    "select_worker_stats" => "select count(*)/5 as gpm, g.worker_id,
        (select count(*) from matchup where worker_id = -g.worker_id)/5 as epm
        from game g
        where timestamp > timestampadd(minute, -5, current_timestamp)
        group by g.worker_id, epm;"
);

?>

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
                              where deleted = 0
                              and (worker_id is null
                              or (worker_id > 0
                                  and matchup_timestamp < (NOW() - INTERVAL 20 MINUTE)))
                              order by matchup_id asc
                              limit 1;",
    "select_matchup_confirm" => "select worker_id from matchup
                                 where matchup_id = %s",
    "select_matchup_players" => "select *
                                 from matchup_player
                                 where matchup_id = %s
                                 order by player_id;",
    "select_game_metadata" => "select gp.user_id, u.username, gp.submission_id,
                               r.rank, r.skill
                               from game_player gp
                               left outer join user u
                                   on u.user_id = gp.user_id
                               left outer join ranking r
                                   on r.submission_id = gp.submission_id
                               and (r.leaderboard_id = (
                                   select max(leaderboard_id)
                                   from leaderboard
                               ) or r.leaderboard_id is null)
                               where gp.game_id = %s
                               order by gp.player_id;",
    "lock_matchup" => "update matchup
                       set worker_id = %s,
                       matchup_timestamp = current_timestamp
                       where matchup_id = %s;",
    "update_matchup_failed" => "update matchup
                                set error = '%s',
                                    worker_id = -worker_id
                                where matchup_id = %s",
    "select_languages" => "select *
                           from language;",
    "select_player_skills" => "select player_id, sigma, mu
                              from matchup_player p
                              inner join submission s on p.submission_id = s.submission_id
                              where matchup_id = %s
                              order by player_id;",
    "insert_game_data" => "insert into game (seed_id, map_id, timestamp, worker_id, turns, winning_turn, ranking_turn) 
                           select seed_id, map_id, current_timestamp, worker_id, %s, %s, %s
                           from matchup
                           where matchup_id = %s;",
    "insert_game_player" => "insert into game_player
                             select %s, p.user_id, p.submission_id, player_id,
                             '%s', '%s', %s, %s,
                             null, null, null, null, 1
                             from matchup_player p
                             where matchup_id = %s
                             and player_id = %s;",
    //"delete_matchup" => "delete from matchup where matchup_id = %s;",
    "delete_matchup" => "update matchup set deleted = 1 where matchup_id = %s;",
    //"delete_matchup_player" => "delete from matchup_player where matchup_id = %s;",
    "delete_matchup_player" => "update matchup_player set deleted = 1 where matchup_id = %s;",
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
               r.*,
               gt.game_count,
               timestampdiff(second, gmin.timestamp, gmax.timestamp)/60/gt.game_count as game_rate
        from ranking r
        inner join (
            select submission_id, min(game_id) min_game, max(game_id) max_game, count(*) as game_count
            from game_player
            group by submission_id
        ) gt
        	on gt.submission_id = r.submission_id
        inner join game gmin
            on gmin.game_id = gt.min_game
        inner join game gmax
            on gmax.game_id = gt.max_game
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
               if(r.rank is not null, @count1 := @count1 + 1 , null) as filter_rank,
               gt.game_count,
               timestampdiff(second, gmin.timestamp, gmax.timestamp)/60/gt.game_count as game_rate
        from ranking r
        inner join (
            select submission_id, min(game_id) min_game, max(game_id) max_game, count(*) as game_count
            from game_player
            group by submission_id
        ) gt
        	on gt.submission_id = r.submission_id
        inner join game gmin
            on gmin.game_id = gt.min_game
        inner join game gmax
            on gmax.game_id = gt.max_game
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
               if(r.rank is not null, @count1 := @count1 + 1 , null) as filter_rank,
               gt.game_count,
               timestampdiff(second, gmin.timestamp, gmax.timestamp)/60/gt.game_count as game_rate
        from ranking r
        inner join (
            select submission_id, min(game_id) min_game, max(game_id) max_game, count(*) as game_count
            from game_player
            group by submission_id
        ) gt
        	on gt.submission_id = r.submission_id
        inner join game gmin
            on gmin.game_id = gt.min_game
        inner join game gmax
            on gmax.game_id = gt.max_game
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
               if(r.rank is not null, @count1 := @count1 + 1 , null) as filter_rank,
               gt.game_count,
               timestampdiff(second, gmin.timestamp, gmax.timestamp)/60/gt.game_count as game_rate
        from ranking r
        inner join (
            select submission_id, min(game_id) min_game, max(game_id) max_game, count(*) as game_count
            from game_player
            group by submission_id
        ) gt
        	on gt.submission_id = r.submission_id
        inner join game gmin
            on gmin.game_id = gt.min_game
        inner join game gmax
            on gmax.game_id = gt.max_game
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
               m.players, m.map_id, m.filename as map_name,
               g.turns, g.winning_turn, g.ranking_turn,
    		   gp.user_id, gp.submission_id, u.username, s.version,
    		   gp.player_id, gp.game_rank,
               gp.mu_after - 3 * gp.sigma_after as skill,
               gp.mu_after as mu, gp.sigma_after as sigma,
               (gp.mu_after - 3 * gp.sigma_after) - (gp.mu_before - 3 * gp.sigma_before) as skill_change,
               gp.mu_after - gp.mu_before as mu_change, gp.sigma_after - gp.sigma_before as sigma_change
         from (
             select g2.*
             from game g2
             inner join game_player gp2
                on g2.game_id = gp2.game_id
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
         inner join submission s
            on gp.submission_id = s.submission_id
         order by g.game_id desc, gp.game_rank",
    "select_map_game_list_page_count" => "select count(*)
        from game g
        where %s = %s",
    "select_map_game_list" => "select g.game_id, g.timestamp,
               m.players, m.map_id, m.filename as map_name,
               g.turns, g.winning_turn, g.ranking_turn,
    		   gp.user_id, gp.submission_id, u.username, s.version,
    		   gp.player_id, gp.game_rank,
               gp.mu_after - 3 * gp.sigma_after as skill,
               gp.mu_after as mu, gp.sigma_after as sigma,
               (gp.mu_after - 3 * gp.sigma_after) - (gp.mu_before - 3 * gp.sigma_before) as skill_change,
               gp.mu_after - gp.mu_before as mu_change, gp.sigma_after - gp.sigma_before as sigma_change
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
    "select_worker_stats" => "select count(*)/15 as gpm, g.worker_id,
        (select count(*) from matchup where worker_id = -g.worker_id)/15 as epm
        from game g
        where timestamp > timestampadd(minute, -15, current_timestamp)
        group by g.worker_id, epm;",
    "select_next_game_in" => "select @players_ahead as players_ahead,
               Round(@players_per_minute, 1) as players_per_minute,
               @time_used as time_used,
               @players_ahead / @players_per_minute as next_game_in,
               Round(@players_ahead / @players_per_minute - @time_used, 1) as next_game_in_adjusted
        from
        (select @players_ahead := ((select count(*) from submission where latest = 1 and status = 40) -
               (select count(distinct user_id) from game_player
                where game_id >
                    (select max(game_id) from game_player where user_id = %s)
               ))) c1,
        (select @players_per_minute := (select count(*)/30
                from game
                inner join game_player
                    on game.game_id = game_player.game_id
                where timestamp > timestampadd(minute, -30, current_timestamp)
               )) c2,
        (select @time_used := ifnull((select avg(timestampdiff(second, matchup_timestamp, current_timestamp)/60)
                               from matchup
                               where deleted = 0
                               and worker_id > 0),0)) c3;",
	"select_in_game" => "select *
        from matchup_player
        where user_id = %s
        and deleted = 0;",
    "select_profile_user" => "select
          u.username,
          u.created,
          u.bio,
          c.flag_filename,
          o.org_id,
          o.name as org_name,
          c.country_id,
          c.name as country_name,
          u.email,
          u.activation_code,
          r.rank, r.rank_change,
          r.skill, r.skill_change,
          r.mu, r.mu_change,
          r.sigma, r.sigma_change
        from
          user u
          left outer join ranking r
          	on u.user_id = r.user_id and r.latest = 1
          left outer join organization o on o.org_id = u.org_id
          left outer join country c on c.country_id = u.country_id
        where
          u.user_id = %s"
);

?>

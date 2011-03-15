<?php

$sql = array(
    "select_next_compile" => "select submission_id
                              from submission
                              where status = 20
                              or (status = 24 and worker_id = %s)
                              order by submission_id asc
                              limit 1;",
    "update_submission_compiling" => "update submission
                                      set status = 24,
                                          worker_id = %s
                                      where submission_id = %s;",
    "update_submission_success" => "update submission
                                      set status = 40,
                                          latest = 1
                                      where worker_id = %s
                                      and submission_id = %s;",
    "update_submission_failure" => "update submission
                                      set status = %s
                                      where worker_id = %s
                                      and submission_id = %s;",
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
                             s.sigma, null, s.mu, null, 1
                             from matchup_player p
                             inner join submission s on p.submission_id = s.submission_id
                             where matchup_id = %s
                             and player_id = %s;",
    "delete_matchup" => "delete from matchup where matchup_id = %s;",
    "delete_matchup_player" => "delete from matchup_player where matchup_id = %s;"
                           
);

?>
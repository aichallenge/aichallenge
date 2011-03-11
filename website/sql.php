<?php

$sql = array(
    "select_next_compile" => "select submission_id
                              from submission
                              where status = 20
                              or (status = 24 and worker_id = %s)
                              order by submission_id asc
                              limit 1",
    "update_submission_compiling" => "update submission
                                      set status = 24,
                                          worker_id = %s
                                      where submission_id = %s",
    "update_submission_success" => "update submission
                                      set status = 40,
                                          latest = 1
                                      where worker_id = %s
                                      and submission_id = %s",
    "update_submission_failure" => "update submission
                                      set status = %s
                                      where worker_id = %s
                                      and submission_id = %s",
	"select_next_match" => "select `match`.*, map.filename
                            from `match`
                            left join map on `match`.map_id = map.map_id
                            where worker is null
                            or worker = %s
                            order by match_id asc
                            limit 1",
    "select_match_players" => "select *
                               from match_player
                               where match_id = %s
                               order by player_id",
    "select_languages" => "select *
                           from language"
                           
);

?>
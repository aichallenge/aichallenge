<?php

require_once('api_functions.php');
header("Content-type: application/json");

// look for compile task first
$compile_result = contest_query("select_next_compile",
                                $worker["worker_id"]);
if ($compile_result) {
    while ($row = mysql_fetch_assoc($compile_result)) {
        if (contest_query("update_submission_compiling",
                          $worker["worker_id"],
                          $row["submission_id"])) {
            echo json_encode(array( "task" => "compile",
                                    "timestamp" => date(DATE_ATOM),
                                    "submission_id" => $row["submission_id"]));
            api_log('worker '.$worker['worker_id'].' ('.$worker['ip_address'].') requesting task, sending compile: '.$row["submission_id"]);
            die();
        } else {
            api_log(sprintf("Error updating submission for compile task: %s", mysql_error()));
        }
    }
} else {
    api_log(sprintf("Error selecting next compile: %s", mysql_error()));
}

// look for match
$start_time = time();
$check_time = $start_time;
$matchup_tries = 0;
$match_result = null;
$match_row = null;
while ((!$match_result or mysql_num_rows($match_result) == 0)
        and $matchup_tries < 600) {
    $matchup_tries += 1;
    $match_result = contest_query("select_next_matchup");

    if ($match_result and mysql_num_rows($match_result) != 0) {
        $match_row = mysql_fetch_assoc($match_result);
        $match_worker = $match_row["worker_id"];
        if ($match_worker === null) {
            $match_worker = "null";
        }
        $lock_result = contest_query("lock_matchup",
                                     $worker["worker_id"],
                                     $match_row["matchup_id"],
                                     $match_worker);
        // Check that we actually got the lock
        if (!$lock_result or mysql_affected_rows() < 1) {
            $match_result = null;
            continue;
        }
    } else {
        sleep(rand(1, 5));
        if (time() - $check_time > 30) {
            $check_time = time();
            api_log(sprintf(
                "Worker %d still waiting for a matchup after %d seconds",
                $worker["worker_id"], $check_time - $start_time));
        }
    }
}
if (time() - $start_time > 20) {
    api_log(sprintf("Worker %d got a match after %d seconds",
        $worker["worker_id"], time() - $start_time));
}

if ($match_row) {
    $json = array( "task" => "game",
                   "timestamp" => date(DATE_ATOM),
                   "matchup_id" => $match_row["matchup_id"],
                   "map_filename" => $match_row["filename"],
                   "max_turns" => $match_row["max_turns"],
                   "submissions" => array(),
                   "game_options" => $server_info["game_options"]);

    $player_result = contest_query("select_matchup_players",
                                   $json["matchup_id"]);
    if ($player_result) {
        while ($player_row = mysql_fetch_assoc($player_result)) {
            $json["submissions"][] = $player_row["submission_id"];
        }
        echo json_encode($json);
        api_log('worker '.$worker['worker_id'].' ('.$worker['ip_address'].') requesting task, sending matchup: '.$json["matchup_id"]);
        die();
    } else {
        api_log(sprintf("Error selecting matchup players: %s", mysql_error()));
    }
} else {
    api_log(sprintf("Error selecting next match: %s", mysql_error()));
}

// nothing to do
echo json_encode(array( "task" => mysql_error(), "timestamp" => date(DATE_ATOM) ));
?>

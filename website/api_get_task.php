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
$match_result = contest_query("select_next_matchup");
if (!$match_result or mysql_num_rows($match_result) == 0) {
    /*
     * php is a god awful inconsistant language written by idiots
     * it consists of a cesspool of global functions
     * most function have the requirments to mess with your logic
     * which is like having a script kid go, "there, fixed that for you"
     * which means programmers don't actually have real control over the system
     * unless they tweak system wide parameters located in an .ini file
     * therefore, I cannot run a stored procedure on 1 connection
     * therefore, I cannot create a persistant connect and a non-persistant
     * 	connection without the second clobering the first
     * so I've resorted to using 2 interfaces, which kinda works sometimes
     * perhaps a php "expert" could teach me the correct dance of settings,
     * 	global function, global variables and code templates to do this
     * 	the "php" way, to which I would say, "or I could use a real language"
     * django is not far away, FML
     * P.S. the space.invaders genetic programming guys are still cool
     */
    $mysqli = new MySQLI($db_host, $db_username, $db_password, $db_name);
    $mysqli->multi_query('call generate_matchup;');
    while ($mysqli->more_results() && $mysqli->next_result());
    $mysqli->close();

    $match_result = contest_query("select_next_matchup",
                                  $worker["worker_id"]);
    if (!$match_result or mysql_num_rows($match_result) == 0) {
        api_log('trying to reset mysql...');
        // mysql_query('ROLLBACK;');
    }
}

if ($match_result) {
    while ($match_row = mysql_fetch_assoc($match_result)) {
        $json = array( "task" => "game",
                       "timestamp" => date(DATE_ATOM),
                       "matchup_id" => $match_row["matchup_id"],
                       "map_filename" => $match_row["filename"],
                       "max_turns" => $match_row["max_turns"],
                       "submissions" => array(),
                       "game_options" => $server_info["game_options"]);
        $lock_result = contest_query("lock_matchup",
                                     $worker["worker_id"],
                                     $json["matchup_id"]);
        if ($lock_result) {
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
            api_log(sprintf("Error locking matchup %s for worker %s", $json["matchup_id"], $worker["worker_id"]));
        }
    }
} else {
    api_log(sprintf("Error selected next match: %s", mysql_error()));
}

// nothing to do
echo json_encode(array( "task" => mysql_error(), "timestamp" => date(DATE_ATOM) ));
?>

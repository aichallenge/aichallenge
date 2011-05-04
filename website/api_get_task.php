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
			                        "submission_id" => $row["submission_id"]));
			die();
		} else {
			api_log(sprintf("Error updating submission for compile task: %s", mysql_error()));
		}
	}
} else {
	api_log(sprintf("Error selecting next compile: %s", mysql_error()));
}
		
// look for match
$match_result = contest_query("select_next_matchup",
                              $worker["worker_id"]);
if (!$match_result) {
    $match_result = mysql_query("call generate_matchup;");
    $match_result = contest_query("select_next_matchup",
                                  $worker["worker_id"]);
}

if ($match_result) {
	while ($match_row = mysql_fetch_assoc($match_result)) {
		$json = array( "task" => "game",
		               "matchup_id" => $match_row["matchup_id"],
		               "map_filename" => $match_row["filename"],
		               "players" => array());
		
		$player_result = contest_query("select_matchup_players",
		                               $match_row["matchup_id"]);
		if ($player_result) {
			while ($player_row = mysql_fetch_assoc($player_result)) {
				$json["players"][] = $player_row["submission_id"];
			}
			echo json_encode($json);
			die();
		} else {
			api_log(sprintf("Error selecting matchup players: %s", mysql_error()));
		}
	}
} else {
	api_log(sprintf("Error selected next match: %s", mysql_error()));
}

// nothing to do
echo json_encode(array( "task" => "expand lolcode specification" ));
?>
<?php
// ini_set('error_reporting', E_ALL);
// ini_set('display_errors', true);

require_once('api_functions.php');

header("Content-type: application/json");

// look for compile task first
$compile_result = mysql_query(sprintf($sql["select_next_compile"],
                              $worker["worker_id"]));
if ($compile_result) {
	while ($row = mysql_fetch_assoc($compile_result)) {
		if (mysql_query(sprintf($sql["update_submission_compiling"],
		                        $worker["worker_id"],
		                        $row["submission_id"]))) {
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
$match_result = mysql_query(sprintf($sql["select_next_match"],
                                    $worker["worker_id"]));
if ($match_result) {                               
	while ($match_row = mysql_fetch_assoc($match_result)) {
		$json = array( "task" => "game",
		               "match_id" => $match_row["match_id"],
		               "map_url" => $match_row["filename"],
		               "players" => array());
		
		$player_result = mysql_query(sprintf($sql["select_match_players"],
		                                     $row["match_id"]));
		while ($player_row = mysql_fetch_assoc($player_result)) {
			$json["players"][] = $player_row["submission_id"];
		}
		echo json_encode($json);
		die();
	}
} else {
	api_log(sprintf("Error selected next match: %s", mysql_error()));
}

// nothing to do
echo json_encode(array( "task" => "expand lolcode specification" ));

?>
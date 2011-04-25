<?php
// ini_set('error_reporting', E_ALL);
// ini_set('display_errors', true);

require_once('api_functions.php');

header("Content-type: application/json");

$json_string = file_get_contents('php://input');
$json_hash = md5($json_string);
$gamedata = json_decode($json_string);

if ($gamedata == null) {
    api_log("Did not recieve post data for game result.");
} else {
    if (array_key_exists('error', $gamedata)) {
    	// set to non-existant worker and email admin
    	if (contest_query('update_matchup_failed')) {
    		echo json_encode(array( "hash" => $json_hash ));
    	} else {
    		api_log(sprintf("Error updating failed matchup %s",
    		                $gamedata->matchup_id));
    	}
    } else {
        // move matchup data to game table
        mysql_query("SET AUTOCOMMIT=0");
        mysql_query("START TRANSACTION");
    	if (!contest_query("insert_game_data", $gamedata->matchup_id)) {
    	    api_log(sprintf("Error updating game table for matchup %s",
    		                $gamedata->matchup_id));
            api_log(mysql_error());
            mysql_query("ROLLBACK");
            die();
    	}
	    $game_id = mysql_insert_id();
	    for ($i = 0, $size = sizeof($gamedata->rank); $i < $size; ++$i) {
	        if (!contest_query("insert_game_player",
	                           $game_id,
	                           $gamedata->rank[$i],
	                           $gamedata->score[$i],
	                           $gamedata->matchup_id,
	                           $i)) {
    		    api_log(sprintf("Error updating game players for matchup %s",
    		                    $gamedata->matchup_id));
	            api_log(mysql_error());
                mysql_query("ROLLBACK");
	            die();
    		}
	    }
	    if (!contest_query("delete_matchup_player", $gamedata->matchup_id)) {
            mysql_query("ROLLBACK");
    	    api_log(sprintf("Error deleting players for matchup %s",
    		                $gamedata->matchup_id));
    		api_log(mysql_error());
    		die();
	    }
        if (!contest_query("delete_matchup", $gamedata->matchup_id)) {
            mysql_query("ROLLBACK");
    	    api_log(sprintf("Error deleting matchup %s",
    		                $gamedata->matchup_id));
    		api_log(mysql_error());
    		die();
        }
        $replay_dir = $server_info["replay_path"] . ((int) ($game_id / 10000));
        if (!file_exists($replay_dir)) {
            api_log($replay_dir);
            mkdir($replay_dir);
        }
        $replay_filename = $replay_dir . "/" . $game_id . ".replay";
        api_log($replay_filename);
        try {
            api_log($gamedata->replay);
            api_log(getcwd());
            $replay_file = fopen($replay_filename, 'w') or api_log(json_encode(error_get_last()));
            fwrite($replay_file, $gamedata->replay);
            fclose($replay_file);
            echo json_encode(array( "hash" => $json_hash ));
            mysql_query("COMMIT");
        } catch (Exception $e) {
            api_log(json_encode($e));
            mysql_query("ROLLBACK");
        }
    }        
}

?>
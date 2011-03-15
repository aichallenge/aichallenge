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
    	if (mysql_query($sql['update_matchup_failed'])) {
    		echo json_encode(array( "hash" => $json_hash ));
    	} else {
    		api_log(sprintf("Error updating failed matchup %s",
    		                $gamedata->matchup_id));
    	}
    } else {
        // move matchup data to game table
        mysql_query("SET AUTOCOMMIT=0");
        mysql_query("START TRANSACTION");
        $stmt = sprintf($sql["insert_game_data"], $gamedata->matchup_id);
    	if (mysql_query($stmt)) {
    	    $game_id = mysql_insert_id();
    	    for ($i = 0, $size = sizeof($gamedata->rank); $i < $size; ++$i) {
    	        $stmt = sprintf($sql["insert_game_player"],
		                        $game_id,
		                        $gamedata->rank[$i],
		                        $gamedata->score[$i],
		                        $gamedata->matchup_id,
		                        $i);
		        if (!mysql_query($stmt)) {
		            api_log(mysql_error());
                    mysql_query("ROLLBACK");
        		    api_log(sprintf("Error updating game players for matchup %s: %s",
        		                    $gamedata->matchup_id,
        		                    $stmt));
        		    die();
        		}
    	    }
    	    if (mysql_query(sprintf($sql["delete_matchup_player"], $gamedata->matchup_id))) {
    	        if (mysql_query(sprintf($sql["delete_matchup"], $gamedata->matchup_id))) {
                    mysql_query("COMMIT");
		            echo json_encode(array( "hash" => $json_hash ));
    	        } else {
                    mysql_query("ROLLBACK");
            	    api_log(sprintf("Error deleting matchup %s",
            		                $gamedata->matchup_id));
    	        }
    	    } else {
                mysql_query("ROLLBACK");
        	    api_log(sprintf("Error deleting players for matchup %s",
        		                $gamedata->matchup_id));
    	    }
    	} else {
            mysql_query("ROLLBACK");
    	    api_log(sprintf("Error updating game table for matchup %s: %s",
    		                $gamedata->matchup_id,
    		                $stmt));
    	}        
    }
}

?>
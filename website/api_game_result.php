<?php
// ini_set('error_reporting', E_ALL);
// ini_set('display_errors', true);

require_once('api_functions.php');

header("Content-type: application/json");

$json_string = file_get_contents('php://input');
$json_hash = md5($json_string);
// api_log($json_string);
$gamedata = json_decode($json_string);

if ($gamedata == null) {
    api_log("Did not recieve post data for game result as proper json.");
} else {
    if (array_key_exists('error', $gamedata)) {
        // set to non-existant worker and email admin
        if (contest_query('update_matchup_failed', json_encode($gamedata->error), $gamedata->matchup_id)) {
            echo json_encode(array( "hash" => $json_hash ));
        } else {
            api_log(sprintf("Error updating failed matchup %s",
                            $gamedata->matchup_id));
        }
    } else {
        // move matchup data to game table
        // mysql_query("SET AUTOCOMMIT=0;");
        // mysql_query("START TRANSACTION;");
        if (!contest_query("insert_game_data", $gamedata->matchup_id)) {
            api_log(sprintf("Error updating game table for matchup %s",
                            $gamedata->matchup_id));
            api_log(mysql_error());
            // mysql_query("ROLLBACK;");
            die();
        }
        $game_id = mysql_insert_id();
        for ($i = 0, $size = sizeof($gamedata->rank); $i < $size; ++$i) {
        	if (!contest_query("insert_game_player",
                               $game_id,
                               $gamedata->errors[$i],
                               $gamedata->status[$i],
                               $gamedata->rank[$i],
                               $gamedata->score[$i],
                               $gamedata->matchup_id,
                               $i)) {
                api_log(sprintf("Error updating game players for matchup %s",
                                $gamedata->matchup_id));
                api_log(mysql_error());
                // mysql_query("ROLLBACK;");
                die();
            }
        }
        if (!contest_query("delete_matchup_player", $gamedata->matchup_id)) {
            // mysql_query("ROLLBACK;");
            api_log(sprintf("Error deleting players for matchup %s",
                            $gamedata->matchup_id));
            api_log(mysql_error());
            die();
        }
        if (!contest_query("delete_matchup", $gamedata->matchup_id)) {
            // mysql_query("ROLLBACK;");
            api_log(sprintf("Error deleting matchup %s",
                            $gamedata->matchup_id));
            api_log(mysql_error());
            die();
        }
        // update game data with meta data
        $gamedata->playernames = array();
        $gamedata->submission_ids = array();
        $gamedata->user_ids = array();
        $result = contest_query("select_game_metadata",
                                $game_id);
        if ($result) {
            while ($meta_row = mysql_fetch_assoc($result)) {
                $gamedata->playernames[] = $meta_row["username"];
                $gamedata->submission_ids[] = $meta_row["submission_id"];
                $gamedata->user_ids[] = $meta_row["user_id"];
            }
        }
        $gamedata->user_url = "http://" . $gamedata->location . "/profile.php?user=~";
        $gamedata->game_url = "http://" . $gamedata->location . "/visualizer.php?game=~";
        $gamedata->date = date(DATE_ATOM);
        $gamedata->game_id = $game_id;
        // errors may contain sensitive info, such as code tracebacks
        unset($gamedata->errors);
        // create pathname to replay file
        $replay_dir = $server_info["replay_path"] . "/" . strval((int) ($game_id / 1000000)) . "/" . strval((int) (($game_id / 1000) % 1000));
        if (!file_exists($replay_dir)) {
            api_log("Replay dir: " . $replay_dir);
            mkdir($replay_dir, 0775, true);
        }
        $replay_filename = $replay_dir . "/" . $game_id . ".replaygz";
        api_log("Replay file: " . $replay_filename);
        try {
            api_log("Cwd: " . getcwd());
            $replay_file = fopen($replay_filename, 'w') or api_log(json_encode(error_get_last()));
            fwrite($replay_file, gzencode(json_encode($gamedata), 9));
            fclose($replay_file);
            chmod($replay_filename, 0664);
            echo json_encode(array( "hash" => $json_hash ));
            // mysql_query("COMMIT;");
            // update trueskill
            $cmd = "python " . $server_info["repo_path"] . "/manager/manager.py -g " . $game_id;
            api_log("TrueSkill update cmd: " . $cmd);
            exec($cmd);
            // put game id in memcache for front page
            if ($memcache) {
                $memcache->set('last_game_id', $game_id);
            }
        } catch (Exception $e) {
            api_log(json_encode($e));
            // mysql_query("ROLLBACK;");
        }
    }
}

?>

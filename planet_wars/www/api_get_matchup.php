<?
require_once('api_functions.php');

/*
  This script returns the next bot a tournament_manager should play. 
 */


function get_matchup() {
    $BACKEND_DIR =
        "/home/contest/ai-contest/planet_wars/backend";
    $matchup = NULL;
    while ($matchup === NULL) {
        $sql = "SELECT * FROM matchups
            WHERE matchup_id = (SELECT MIN(matchup_id) FROM matchups
                WHERE dispatch_time IS NULL
                    OR dispatch_time < (NOW() - INTERVAL 6 MINUTE))";
        $result = mysql_query($sql);
        if (mysql_num_rows($result) == 0) {
            # trying to create matchups here instead of just dieing
            # leads to the server overloading as every worker tries to create
            # pairings
            sleep(5);
            die();
        }
        $row = mysql_fetch_assoc($result);
        $matchup_id = $row['matchup_id'];
        $dispatch_time = $row['dispatch_time'];
        $sql = "UPDATE matchups SET dispatch_time = NOW()
            WHERE matchup_id=".$matchup_id;
        if ($dispatch_time === NULL) {
            $sql = $sql." AND dispatch_time IS NULL";
        } else {
            $sql = $sql." AND dispatch_time = '$dispatch_time'";
        }
        mysql_query($sql);
        if (mysql_affected_rows() > 0) $matchup = $row;
    }
    return $matchup;
}

function get_submission($sub_id) {
    $sql = "SELECT s.*, l.*, l.name as language_name                
        FROM submissions s INNER JOIN languages l
            ON l.language_id = s.language_id
        WHERE s.submission_id=".$sub_id;
    $result = mysql_query($sql);
    return mysql_fetch_assoc($result);
}

while (True) {
    $matchup = get_matchup();
    $submission_one = get_submission($matchup['player_one']);
    $submission_two = get_submission($matchup['player_two']);
    if ($submission_one['latest'] != 1 or $submission_two['latest'] != 1) {
        mysql_query("DELETE FROM matchups
            WHERE player_one='".$matchup['player_one']."'
               AND '".$matchup['player_two']."'");
        continue;
    }
    break;
}

$sql = "SELECT * FROM maps WHERE map_id=".$matchup['map_id'];
$result = mysql_query($sql);
$row = mysql_fetch_assoc($result);
$map_name = $row['path'];

echo json_encode(array(
    'players'=>array($submission_one,$submission_two),
    'map'=>array('id'=>$matchup['map_id'],'name'=>$map_name),
    ));

# update last game timestamp of players
# doing it here as the game is handed out instead of when the game is
# turned in avoids bias against players that take a longer time to play
$sql = "UPDATE submissions set last_game_timestamp = current_timestamp
    WHERE submission_id = '".$matchup['player_one']."'
        OR submission_id = '".$matchup['player_two']."'";
mysql_query($sql);

?>

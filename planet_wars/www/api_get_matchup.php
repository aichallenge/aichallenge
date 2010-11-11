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
            chdir($BACKEND_DIR);
            exec("python create_matchups.py");
            continue;
        }
        $row = mysql_fetch_assoc($result);
        $matchup_id = $row['matchup_id'];
        $dispatch_time = $row['dispatch_time'];
        $sql = "UPDATE matchups SET dispatch_time = NOW()
            WHERE matchup_id=".$matchup_id;
        if ($dispatch_time === NULL) {
            $sql = $sql." AND dispatch_time IS NULL";
        } else {
            $sql = $sql." AND dispatch_time = ".$dispatch_time;
        }
        mysql_query($sql);
        if (mysql_affected_rows() != 0) $matchup = $row;
    }
    return $matchup;
}

function get_submission($sub_id) {
    $sql = "SELECT s.*, l.*, l.name as language_name,
                IF(r.rank is NULL, 500, r.rank) as rank
        FROM submissions s INNER JOIN languages l
            ON l.language_id = s.language_id
            LEFT OUTER JOIN rankings r ON r.submission_id = s.submission_id
                AND r.leaderboard_id = (
                    select max(leaderboard_id) from leaderboards)
        WHERE s.submission_id=".$sub_id;
    $result = mysql_query($sql);
    return mysql_fetch_assoc($result);
}

while (True) {
    $matchup = get_matchup();
    $submission_one = get_submission($matchup['player_one']);
    $submission_two = get_submission($matchup['player_two']);
    if ($submission_one['latest'] != 1 or $submission_two['latest'] != 1) {
        mysql_quert("DELETE FROM matchups
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

?>

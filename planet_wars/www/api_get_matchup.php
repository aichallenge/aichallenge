<?
require_once('api_functions.php');

/*
  This script returns the next bot a tournament_manager should play. 
  This script needs to be fast. Future complicated 
  game picking should be done somewhere else and fed here.
*/

$conditions = array();
$languages_condition = 'l.language_id <> 15 AND l.language_id <> 10'; 
$conditions[]=$languages_condition;
$conditions = implode(" AND ",$conditions);
// This is a temporary language restriction.
// Lisp bots won't compile on cloud servers. Haven't had time to debug.
// Someone wrote a node js bot that crashes the server by using too much memory.
// Added memory limits, but again have not had time to debug.

// Future:
// I want to make it randomly choose only more recent submissions to play.
// AND timestamp > timestampadd(hour, -24, current_timestamp)


$sql = "SELECT s.*, l.*, l.name as language_name, IF(r.rank is NULL, 500, r.rank) as rank
FROM submissions s USE INDEX (latest)
INNER JOIN languages l ON l.language_id = s.language_id
LEFT OUTER JOIN rankings r ON r.submission_id = s.submission_id AND r.leaderboard_id = (select max(leaderboard_id) from leaderboards)
WHERE 
  s.status = 40
  AND s.latest = 1
  AND $conditions
order by rank desc
";
$q = mysql_query($sql);

$submissions = array();
$p1_offset = Null;
$i = 0;
while($row = mysql_fetch_assoc($q)){
  $submissions[$i] = $row;
  if($p1_offset===Null){
    $p1_offset = 0;
  }
  
  $a = (int)ereg_replace('^[0-9]+-|[^0-9]','',$submissions[$i]['last_game_timestamp']);
  $b = (int)ereg_replace('^[0-9]+-|[^0-9]','',$submissions[$p1_offset]['last_game_timestamp']);
  if($a < $b ){
    // Pick the player that needs a game the most!
    // Which for now is the player that played last!
    $p1_offset=$i;
  }
  $i++;
}


// Pick a match, probably near in skill.
$p2_offset = $p1_offset;
while($p1_offset == $p2_offset ){
  $offset_range = min( 2 + floor(pareto_random(0.5)), count($submissions));
  $rank_offset =  rand(-$offset_range,$offset_range);
  $provisional_offset = $p1_offset + $rank_offset;
  if($provisional_offset>count($submissions)-1 || $provisional_offset < 0){
    continue;
  }
  $p2_offset = $provisional_offset;
}

// Pack up the data, and send it off.
$submission_one = $submissions[(int)$p1_offset];
$submission_two = $submissions[(int)$p2_offset];
$map_id = rand(1,99);
$map = array('id'=>$map_id,'name'=>'map'.$map_id.'.txt');
echo json_encode(array(
    'players'=>array($submission_one,$submission_two),
    'map'=>$map,
    'offset_range'=>$offset_range,
    'rank_offset'=>$rank_offset
    ));

// Update the last played status for the bots.
// Yes, I know it's ahead of time, but otherwise 
// all the servers would be playing with the same player one.
update_last_game_played($submission_one);
update_last_game_played($submission_two);
    

function update_last_game_played($submission){
  $submission_id = $submission['submission_id'];
  $sql = "UPDATE submissions set last_game_timestamp = current_timestamp WHERE submission_id = '".addslashes($submission_id)."'";
  mysql_query($sql);
}

function pareto_random($alpha){
  // Creates a long tail distribution. 
  // Close values are much more likely to be chosen.
  $u = 1.0*(rand() / getrandmax());
  return 1.0 / pow($u, 1.0/$alpha);
}


?>
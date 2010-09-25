<?
require_once('api_functions.php');

/*
  This script returns the next bot a tournament_manager should play. 
  At the moment it's got a very simple way of picking bots, since this script needs to be fast.
  In the near future, we'll have to add a matchup maker python script that inserts desired matches into the database.
  We'll change this script to just fetch the top match from that table.
*/




$sql = "SELECT s.*, l.*, r.rank
FROM submissions s
INNER JOIN languages l ON l.language_id = s.language_id
LEFT OUTER JOIN rankings r ON r.submission_id = s.submission_id AND r.leaderboard_id = (select max(leaderboard_id) from leaderboards)
WHERE 
  s.status = 40
  AND l.language_id in(1,6,9,11) -- Non-compiled languages only, for now
  AND timestamp > timestampadd(hour, -24, current_timestamp)
order by rank desc;
";

$q = mysql_query($sql);

$submissions = array();
while($row = mysql_fetch_assoc($q)){
  $submissions[] = $row;
}


$p1_offset = rand(0,count($submissions));
$p2_offset = $p1_offset;
while($p1_offset == $p2_offset){
  $new_offset = $p1_offset + rand(-20,+20);
  if($new_offset>count($submissions) || $new_offset < 0){
    continue;
  } 
  $p2_offset = $new_offset;
}

$submission_one = $submissions[$p1_offset];
$submission_two = $submissions[$p2_offset];
$map_id = rand(1,99);
$map = array('id'=>$map_id,'name'=>'map'.$map_id.'.txt');
echo json_encode(array(
    'players'=>array($submission_one,$submission_two),
    'map'=>$map
    ));







?>
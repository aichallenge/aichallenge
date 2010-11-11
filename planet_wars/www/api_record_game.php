<?
require_once('api_functions.php');

/* 
  This script records game results.
  Possibly in the future we will also check that the game we are storing is a match we had previously handed out.
*/

function update_last_game_played($submission_id){
  $sql = "UPDATE submissions set last_game_timestamp = current_timestamp
      WHERE submission_id = '".addslashes($submission_id)."'";
  mysql_query($sql);
}

$gamedata = json_decode(file_get_contents('php://input'));

$sql = "INSERT INTO games (winner,loser,map_id,draw,timestamp,".
          "player_one,player_two,worker) 
          VALUES (".
            "'".addslashes($gamedata->winner) . "', ".
            "'".addslashes($gamedata->loser) . "', ".
            "'".addslashes($gamedata->map_id) . "', ".
            "'".addslashes($gamedata->draw) . "', ".
            " current_timestamp, ".
            "'".addslashes($gamedata->player_one) . "',".
            "'".addslashes($gamedata->player_two) . "',".
            "'".addslashes($worker['worker_id'])."'".
          ")";
mysql_query($sql);
if(mysql_error()!=''){
  die(mysql_error()."\n".$sql);
}

$game_id = mysql_insert_id();
$compressed_playback_string = gzcompress($gamedata->playback_string);
$sql = "INSERT INTO playback SET 
          game_id = '".$game_id."', 
          playback_string = '".addslashes($compressed_playback_string)."'";
$r=mysql_query($sql);

if(isset($gamedata->errors)){
  foreach($gamedata->errors as $error){
    $sql = "INSERT INTO errors (submission_id,game_id,turn,`error`,timestamp)".
            "VALUES (".
                "'".addslashes($error->submission_id) . "', ".
                "'".addslashes($game_id) . "', ".
                "'".addslashes($error->turn) . "', ".
                "'".addslashes($error->error) . "', ".
                " current_timestamp ".
              ")";
    mysql_query($sql); 
  }
}

update_last_game_played($gamedata->player_one);
update_last_game_played($gamedata->player_two);

$sql = "DELETE FROM matchups
    WHERE player_one='".addslashes($gamedata->player_one)."'
        AND player_two='".addslashes($gamedata->player_two)."'";
mysql_query($sql);

if($r){
  echo("done");
}else{
  echo("error saving");
}

?>

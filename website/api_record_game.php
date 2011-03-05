<?
require_once('api_functions.php');

/* 
  This script records game results.
  Possibly in the future we will also check that the game we are storing is a match we had previously handed out.
*/

$gamedata = json_decode(file_get_contents('php://input'));

$sql = "SELECT count(1) FROM game
    WHERE player_one = ".$gamedata->player_one."
        AND player_two = ".$gamedata->player_two."
        AND map_id = ".$gamedata->map_id;
$result = mysql_query($sql);
$row = mysql_fetch_assoc($result);
if ($row['count(1)'] != 0) {
  die("Appears this pairing and map are already in the database");
}

$sql = "INSERT INTO game (winner,loser,map_id,draw,timestamp,".
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
    $sql = "INSERT INTO error (submission_id,game_id,turn,`error`,timestamp)".
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

# remove the matchup from the queue
$sql = "DELETE FROM matchup
    WHERE player_one='".addslashes($gamedata->player_one)."'
        AND player_two='".addslashes($gamedata->player_two)."'";
mysql_query($sql);

if($r){
  echo("done");
}else{
  echo("error saving");
}

?>

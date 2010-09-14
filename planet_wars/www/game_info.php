<?php

include "mysql_login.php";

$game_id = (int)($_GET["game_id"]);

if($game_id == 0){
  echo "No game_id given.\n";
}else{
    $query = "SELECT
      g.*,
      u1.username AS player_one,
      u2.username AS player_two,
      g.player_one as player_one_id,
      g.player_two as player_two_id
    FROM games g
    INNER JOIN submissions s1 ON g.player_one = s1.submission_id
    INNER JOIN users u1 ON s1.user_id = u1.user_id
    INNER JOIN submissions s2 ON g.player_two = s2.submission_id
    INNER JOIN users u2 ON s2.user_id = u2.user_id
    WHERE g.game_id = $game_id";
    $result = mysql_query($query);
    if(!$result){
      echo "Could not query the database.\n";
    }else{
      $row = mysql_fetch_assoc($result);
      if($playback_string = get_playback($game_id)){
        $row['playback_string'] = $playback_string;
        $row['using_compression'] = true;
      }
      if($row){
        foreach ($row as $key => $value) {
          echo "$key=$value\n";
        }
      }
    }
}


function get_playback($game_id){
  $sql = "SELECT * from playback where game_id = $game_id";
  $q = mysql_query($sql);
  $result = mysql_fetch_assoc($q);
  if($result['playback_string']!=''){
    return gzuncompress($result['playback_string']);
  }
}

?>

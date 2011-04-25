<?php

require_once("mysql_login.php");

$game_id = (int)($_GET["game_id"]);

if($game_id == 0){
  echo "No game_id given.\n";
}else{
    $query = "SELECT
      g.*,
      u1.username AS player_one,
      u2.username AS player_two,
      g.player_one as player_one_id,
      g.player_two as player_two_id,
      s1.user_id as user_one_id,
      s2.user_id as user_two_id
    FROM game g
    INNER JOIN submission s1 ON g.player_one = s1.submission_id
    INNER JOIN user u1 ON s1.user_id = u1.user_id
    INNER JOIN submission s2 ON g.player_two = s2.submission_id
    INNER JOIN user u2 ON s2.user_id = u2.user_id
    WHERE g.game_id = $game_id";
    $result = mysql_query($query);
    if(!$result || mysql_num_rows($result) == 0){
      $query = str_replace("FROM games g", "FROM games_archive g", $query);
      $result = mysql_query($query);
    }
    if(!$result){
      echo "Could not query the database.\n";
    }else{
      $row = mysql_fetch_assoc($result);
      if($playback_string = get_playback($game_id)){
        $row['playback_string'] = $playback_string;
        $row['using_compression'] = true;
      }
      if($error_message = get_error_message($game_id)){
        $row['error_message'] = $error_message;
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




function get_error_message($game_id){
  $error_types = array(
    'TIMEOUT' => 'crashed / did not start / timeout',
    'BAD_ORDER' => 'issued an invalid order.',
    'UNPARSEABLE_ORDER' => 'issued an order that could not be parsed',
    );
  
  $sql = "SELECT e.*, u.username from error e inner join submission s ON e.submission_id = s.submission_id inner join user u on u.user_id = s.user_id where game_id = '".mysql_real_escape_string($game_id)."'";
  $q = mysql_query($sql);
  $out = array();
  while($result = mysql_fetch_assoc($q)){
    if($result){
      $nice_error = $error_types[$result['error']];
      if($nice_error==''){
        $nice_error = $result['error'];
      }
      $out[]=$result['username'].' '.$nice_error.".";
    }
  }
  return implode(' ',$out);
  
}

?>

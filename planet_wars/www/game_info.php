<?php

include "mysql_login.php";

$game_id = $_GET["game_id"];
if (strlen($game_id) > 0) {
  $query = file_get_contents("game_info_query.sql") . $game_id;
  $result = mysql_query($query);
  if ($result) {
    if ($row = mysql_fetch_assoc($result)) {
      foreach ($row as $key => $value) {
        echo "$key=$value\n";
      }
    }
  } else {
    echo "Could not query the database.\n";
  }
} else {
  echo "No game_id given.\n";
}

?>

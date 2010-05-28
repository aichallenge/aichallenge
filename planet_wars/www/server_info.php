<?php

function get_server_info() {
  $server_info = array();
  $lines = file("server_info.txt");
  foreach ($lines as $line_num => $line) {
    $line = trim($line);
    $tokens = split(":", $line);
    if (count($tokens) != 2) {
      return NULL;
    }
    $server_info[$tokens[0]] = $tokens[1];
  }
  return $server_info;
}

?>

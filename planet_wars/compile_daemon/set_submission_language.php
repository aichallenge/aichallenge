<?php

include 'mysql_login.php';

$submission_id = $argv[1];
$language = $argv[2];
$query = "UPDATE contest_submissions SET programming_language = '" . $language . "' WHERE " .
  "submission_id = " . $submission_id;
mysql_query($query);

?>

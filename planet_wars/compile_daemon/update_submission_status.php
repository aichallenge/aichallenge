<?php

include 'mysql_login.php';

$submission_id = $argv[1];
$new_status = $argv[2];
$query = "UPDATE contest_submissions SET status = " . $new_status . " WHERE " .
  "submission_id = " . $submission_id;
mysql_query($query);

?>

<?php

include 'mysql_login.php';

$submission_ids = array( );

// Get the compile-able entries from the database. This is basically any
// submission which is
//   (1) the latest submitted by a particular user, and
//   (2) has status code 20 (ready for compilation).
$query = "SELECT u.email, (SELECT s.submission_id FROM contest_submissions s " .
  "WHERE s.user_id = u.user_id ORDER BY s.timestamp DESC LIMIT 1) AS submission_id, " .
  "(SELECT s.status FROM contest_submissions s WHERE s.user_id = u.user_id " .
  "ORDER BY s.timestamp DESC LIMIT 1) AS status FROM contest_users u ORDER BY (SELECT s.timestamp FROM contest_submissions s WHERE s.user_id = u.user_id ORDER BY s.timestamp DESC LIMIT 1)";
$result = mysql_query($query);
while ($row = mysql_fetch_assoc($result)) {
  $submission_id = $row['submission_id'];
  $status = $row['status'];
  if (is_numeric($submission_id)) {
    if ($status == 20) {
      $submission_ids[] = $submission_id;
    }
  }
}

/*
if ($handle = opendir('/users/j3camero/www/entries/')) {
  while (false !== ($file = readdir($handle))) {
    if (is_numeric($file)) {
      $directories[] = $file;
    }
  }
  closedir($handle);
}
*/

//$submissions_to_be_compiled = array_intersect($directories, $submission_ids);
$submissions_to_be_compiled = $submission_ids;

//foreach ($submissions_to_be_compiled as $submission_id) {
//  print $submission_id . "\n";
//}

if (count($submissions_to_be_compiled) == 0) {
  die();
}
$submission_id = $submissions_to_be_compiled[0];
fwrite(STDERR, "Grabbed submission_id = $submission_id\n");
$query = "UPDATE contest_submissions SET status = 24 WHERE " .
  "submission_id = $submission_id AND status = 20";
fwrite(STDERR, "query = $query\n");
$result = mysql_query($query);
if (!result) {
  die();
}
$affected_rows = mysql_affected_rows();
fwrite(STDERR, "affected_rows = $affected_rows\n");
if (mysql_affected_rows() > 0) {
  print $submission_id . "\n";
}

?>

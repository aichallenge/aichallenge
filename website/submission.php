<?php

require_once('memcache.php');

/*
 * Submission Statuses
 * 10: Created: entry record created in database.
 * 20: Uploaded: Ready to be unzipped and compiled.
 * 30: Compiling: worker is compiling and running tests
 * 40: Runable: compiled successfully and passed test cases.  Ready to be run.
 * 50: Download Error: error receiving submission zip file.
 * 60: Unpack Error: error while unzipping submission file.
 * 70: Compile Error: error while compiling submission.
 * 80: Test Error: compiled successfully but failed test cases.
 * 90: Upload Error: server failed to retrieve uploaded file correctly.
 * 100: Inactivated: submission must be activated to conintue being chosen as a seed
 *                   it can still be pulled in as an opponent
 */

function create_new_submission_for_current_user() {
    global $memcache;
    
    if ($memcache) {
        $memcache->delete("lookup:submission_id");
    }
    if (current_user_id() == -1) {
        return FALSE;
    }
    return contest_query("insert_new_submission", current_user_id());
}

function current_submission_id() {
  $user_id = current_user_id();
  if ($user_id == NULL) {
    return -1;
  }
  $query = "SELECT * FROM submission " .
    "WHERE user_id = " . $user_id . " ORDER BY timestamp DESC LIMIT 1";
  $result = mysql_query($query);
  if (!$result) {
    print $query . "\n";
    print mysql_error() . "\n";
    return -1;
  }
  if ($row = mysql_fetch_assoc($result)) {
    return $row['submission_id'];
  } else {
    return -1;
  }
}

function current_submission_status() {
  $user_id = current_user_id();
  if ($user_id == NULL) {
    return -1;
  }
  $query = "SELECT * FROM submission " .
    "WHERE user_id = " . $user_id . " ORDER BY timestamp DESC";
  $result = mysql_query($query);
  if ($row = mysql_fetch_assoc($result)) {
    return $row['status'];
  } else {
    return -1;
  }
}

/* 
 * Returns true if it finds a submission from within the last 10 minutes that
 * either successfully entered the contest or is still in the process of
 * entering.
 */
function has_recent_submission() {
  $user_id = current_user_id();
  if ($user_id == NULL) {
    return FALSE;
  }
  $query = "SELECT COUNT(*) FROM submission WHERE user_id = '".$user_id."' AND
    (status < 30 OR (status in (40, 100) AND timestamp >= (NOW() - INTERVAL 1 MINUTE)))";
  $result = mysql_query($query);
  if (!$row = mysql_fetch_row($result)) {
    return FALSE;
  }
  if ($row[0] == 0) {
    return FALSE;
  }
  return TRUE;
}

function submission_status($submission_id) {
  $query = "SELECT * FROM submission " . "WHERE submission_id = " . $submission_id;
  $result = mysql_query($query);
  if ($row = mysql_fetch_assoc($result)) {
    return $row['status'];
  } else {
    return -1;
  }
}

function update_current_submission_status($new_status) {
  $submission_id = current_submission_id();
  if ($submission_id < 0) {
    print "<p>submission_id = " . $submission_id . "</p>";
    return FALSE;
  }
  $user_id = current_user_id();
  if ($user_id < 0) {
    print "<p>user_id = " . $user_id . "</p>";
    return FALSE;
  }
  $query = "UPDATE submission SET status = " . $new_status .
    " WHERE submission_id = " . $submission_id . " AND user_id = " . $user_id;
  //print "<p>query = " . $query . "</p>";
  return mysql_query($query);
}

function submission_directory($submission_id) {
    global $server_info;
    $submission_id = intval($submission_id);
    return $server_info["uploads_path"]."/".strval((int)($submission_id/1000))."/".strval($submission_id);
}

?>

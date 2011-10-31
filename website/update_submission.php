<?php
require_once('session.php');
require_once('mysql_login.php');

if (!logged_in_with_valid_credentials())
  die ("Nice try.. but the evil robot WILL get you");

if (!array_key_exists('update_key', $_POST)) {
  die("Check key not found.");
}

$user_id = current_user_id();

$query = "SELECT email, activation_code FROM user WHERE user_id = ".$user_id;
$result = mysql_query($query);
if ($row = mysql_fetch_assoc($result)) {
  $sid = session_id();
  $update_key = $_POST['update_key'];
  $local_key = sha1($sid . $row['activation_code'] . $row['email']);
  if ($local_key != $update_key) {
    die("Bad update key found.");
  }
} else {
  die("User data query failed.");
}

if (array_key_exists('activate', $_POST)) {
    contest_query("update_user_shutdown_date_activate", $user_id);
    contest_query("activate_submission", $user_id);
} elseif (array_key_exists('deactivate', $_POST)) {
    contest_query("update_user_shutdown_date_deactivate", $user_id);
    contest_query("deactivate_submission", $user_id);
}

header('Location: profile.php?user='. $user_id);
?>

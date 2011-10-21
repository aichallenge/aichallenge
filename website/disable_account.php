<?php

require_once('session.php');
require_once('mysql_login.php');

function safe_str($str) {
    return mysql_real_escape_string(stripslashes($str));
}

if (!(logged_in_with_valid_credentials() && logged_in_as_admin()))
    die("Forget it, you must be logged in as admin.");

if (!isset($_POST['user_id']) || !isset($_POST['reason']))
    die("Did not receive user_id or reason");

$user_id = safe_str($_POST['user_id']);
$reason = $_POST['reason'];

$query = "SELECT * from user where user_id = ".$user_id;
$result = mysql_query($query);
if (!result || mysql_num_rows($result) != 1)
    die("Could not find the user account");
$user = mysql_fetch_assoc($result);

if ($user['password'] == "")
    die("This account is already disabled");

$admin = current_username();
$bio = safe_str($user['bio']." - ".$reason." by ".$admin);
$email = safe_str($user['email']." disabled");

$query = "UPDATE user SET email='$email', bio = '$bio', password = ''
    WHERE user_id = $user_id";
mysql_query($query);
$query = "UPDATE submission SET latest=0 WHERE user_id = $user_id";
mysql_query($query);

header("Location: profile.php?user=".$user_id);

?>

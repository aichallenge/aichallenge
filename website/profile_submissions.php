<?php
include 'header.php';
include_once 'profile_submissions_widget.php';

$user_id = $_GET["user_id"];
if(!filter_var($user_id, FILTER_VALIDATE_INT)) {
    $user_id = NULL;
}

$page = $_GET["page"];
if(!filter_var($page, FILTER_VALIDATE_INT)) {
    $page = 1;
}

$query = "select u.username from user u where u.user_id = '$user_id' limit 1";
$username = mysql_fetch_object(mysql_query($query))->username;

$username = htmlspecialchars($username);

echo "<h2><a href=\"profile.php?user=$user_id\">$username</a>'s contest submissions</h2>";
echo getSubmissionTableString($user_id, false, 25, "?user=$user_id&page=", $page);

include 'footer.php';
?>

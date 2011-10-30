<?php
require_once('profile_submissions_widget.php');
require_once('mysql_login.php');

include 'header.php';

$user_id = NULL;
if (isset($_GET['user'])) {
    $user_id = $_GET["user"];
    if(!filter_var($user_id, FILTER_VALIDATE_INT)) {
        $user_id = NULL;
    }
}

if(!isset($_GET["page"])
    || !filter_var($_GET["page"], FILTER_VALIDATE_INT)) {
    $page = 1;
} else {
    $page = $_GET["page"];
}

if ($user_id) {
    $query = "select u.username from user u where u.user_id = $user_id limit 1";
    $username = mysql_fetch_object(mysql_query($query))->username;
    
    $username = htmlentities($username, ENT_COMPAT, "UTF-8");
    
    $title=$username."'s Contest Submissions";
    echo "<h2><a href=\"profile.php?user=$user_id\">$username</a>'s contest submissions</h2>";
    echo getSubmissionTableString($user_id, false, 25, "?user=$user_id&page=", $page);
} else {
    echo "<p>User not found.</p>";
}

include 'footer.php';
?>

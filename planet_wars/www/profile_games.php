<?php
include 'header.php';
include_once 'profile_games_widget.php';

$user_id = $_GET["user_id"];
if(!filter_var($user_id, FILTER_VALIDATE_INT)) {
    $user_id = NULL;
} else {
    $user_id = intval($user_id);
}

$page = $_GET["page"];
if(!filter_var($page, FILTER_VALIDATE_INT)) {
    $page = 1;
} else {
    $page = intval($page);
}

// Fetch userid's username
$username_query = <<<EOT
select
    u.username
from
    contest_users u
where
    u.user_id = '$user_id'
EOT;

$username_data = mysql_query($username_query);
if ($username_data) {
    list ($username) = mysql_fetch_row($username_data);
} else {
    $username = "";
}

$username = htmlspecialchars($username);

echo "<h2><a href=\"profile.php?user_id=$user_id\">$username</a>'s latest submission's games</h2>";
echo getGamesTableString($user_id, false, 200, "?user_id=$user_id&page=", $page);

include 'footer.php';
?>

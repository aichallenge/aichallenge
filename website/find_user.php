<?php

require_once("mysql_login.php");

$username = mysql_real_escape_string(stripslashes($_GET['username']));
if (!isset($username)) {
    echo "No username given";
    die();
}

$username .= "%";

$query = "SELECT user_id, username FROM user
    WHERE username LIKE \"$username\" ORDER BY username";
$result = mysql_query($query);
if (!$result) {
    echo "Sorry, database query failed.";
    die();
}

$num_found = mysql_num_rows($result);
if ($num_found == 1) {
    $row = mysql_fetch_assoc($result);
    header("Location: profile.php?user=".$row['user_id']);
    die();
}

include "header.php";

if ($num_found > 0) {
    echo <<<EOT
<h2>Users</h2>
<ul>
EOT;
    while ($row = mysql_fetch_assoc($result)) {
        echo "<li><a href='profile.php?user=".$row['user_id']."'>".
            $row['username']."</a></li>";
    }
    echo "</ul>";
} else {
    echo "<h2>Sorry could not find any user with that name</h2>";
}

if (isset($_SERVER['HTTP_REFERER'])) {
    echo "<a href=\"".$_SERVER['HTTP_REFERER']."\">Back</a>";
}

include "footer.php";

?>

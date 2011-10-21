<?php

require_once("mysql_login.php");
require_once("lookup.php");

$username = mysql_real_escape_string(stripslashes($_GET['username']));
if (!isset($username) || !$username) {
    $users = NULL;
} else {
    $users = search_user_row($username);
    if (count($users) === 1) {
        header("Location: profile.php?user=".$users[0]['user_id']);
        die();
    }   
}

require_once("header.php");
require_once("nice.php");

if ($users === NULL) {
    echo "<p>No search string given.</p>";
} else {
    echo "<h2>Users with '$username'</h2>";
    if (count($users) > 0) {
        echo "<ul>";
        foreach ($users as $user) {
            $username = htmlentities($user['username'], ENT_COMPAT, "UTF-8");
            echo "<li>".nice_user($user['user_id'], $user['username'])."</li>";
        }
        echo "</ul>";
    } else {
        echo "<p>Sorry could not find any user with that name.</p>";
    } 
}

require_once("footer.php");

?>

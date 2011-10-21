<?php

require_once('header.php');
require_once('lookup.php');
require_once('nice.php');

echo "<h2>User List</h2>";

$result = search_user_row();
if (count($result) > 0) {
    echo "<ul>";
    foreach ($result as $row) {
        $username = htmlentities($row['username'], ENT_COMPAT, "UTF-8");
        echo "<li>".nice_user($row['user_id'], $row['username'])."</li>";
    }
    echo "</ul>";
} else {
    echo "<p>No users found.</p>";
}

require_once('footer.php');
?>

<?php

require_once('session.php');
require_once('mysql_query.php');
$title = "Duplicated E-Mails";
include 'header.php';

if (!logged_in_as_admin()) {
    die("Must be logged in as an admin to access this page");
}

$query = "SELECT u1.* FROM user AS u1 JOIN (SELECT email FROM user
        GROUP BY email HAVING COUNT(*) > 1
    ) AS u2 ON u1.email = u2.email WHERE password != ''
    ORDER BY email, user_id";
$result = mysql_query($query);
if (!$result) {
    echo "Could not query database";
} else {
    $num_accounts = mysql_num_rows($result);
    echo <<< EOT
<h3>Found $num_accounts accounts using duplicate email addresses</h3>
<table class="leaderboard">
  <thead><tr>
    <th>ID</th><th>User</th><th>Email</th><th>Activated</th</tr>
  </thead><tbody>
EOT;
    for ($i = 1; $row = mysql_fetch_assoc($result); $i += 1) {
        $tr_class = $i % 2 ? "even" : "odd";
        $user_id = $row['user_id'];
        $username = $row['username'];
        $email = $row['email'];
        $activated = $row['activated'];
        echo <<< EOT
<tr class="$tr_class">
    <td><a href="profile.php?user=$user_id">$user_id</a></td>
    <td><a href="profile.php?user=$user_id">$username</a></td>
    <td>$email</td><td>$activated</td>
</tr>
EOT;
    }
    echo "</tbody></table>";
}

include "footer.php";
?>


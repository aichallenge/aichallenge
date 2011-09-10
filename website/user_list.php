<?php
include 'header.php';
require_once('mysql_login.php');
echo "<h2>User List</h2>";
$query = "SELECT * FROM user";
$result = mysql_query($query);
if ($result) {
  echo "<ul>";
  while ($row = mysql_fetch_assoc($result)) {
    $username = htmlentities($row['username'], ENT_COMPAT, "UTF-8");
    echo "<li>$username</li>";
  }
  echo "</ul>";
} else {
  echo "<p>Could not query the database.</p>";
}
include 'footer.php';
?>

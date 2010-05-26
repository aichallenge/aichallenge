<?php
include 'header.php';
include 'mysql_login.php';
echo "<h2>User List</h2>";
$query = "SELECT * FROM users";
$result = mysql_query($query);
if ($result) {
  echo "<ul>";
  while ($row = mysql_fetch_assoc($result)) {
    $username = $row['username'];
    echo "<li>$username</li>";
  }
  echo "</ul>";
} else {
  echo "<p>Could not query the database.</p>";
}
include 'footer.php';
?>

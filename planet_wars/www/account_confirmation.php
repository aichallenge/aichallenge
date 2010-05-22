<?php
include 'session.php';
include 'mysql_login.php';
include 'header.php';

function get_username_from_confirmation_code($confirmation_code) {
  $query = "SELECT username FROM users WHERE " .
    "activation_code = '$confirmation_code'";
  $result = mysql_query($query);
  if ($row = mysql_fetch_assoc($result)) {
    $username = $row['username'];
    return $username;
  } else {
    return NULL;
  }
}

$confirmation_code = $_GET['confirmation_code'];
if ($confirmation_code == NULL || strlen($confirmation_code) <= 0) {
  $errors[] = "Failed to activate the account. (101)";
} else {
  $username = get_username_from_confirmation_code($confirmation_code);
  if ($username == NULL || strlen($username) <= 0) {
    $errors[] = "Failed to activate the account. (102)";
  } else {
    $result = activate_user($username);
    if (!$result) {
      $errors[] = "Failed to activate the account. (103)";
    }
  }
}
if (count($errors) > 0) {
?>

<h1>Account Activation Failed</h1>
<p>There was a problem while activating your account, and your account may
  not have been activated.</p>
<ul>

<?php
foreach ($errors as $key => $error) {
  print "<li>$error</li>";
}
?>

</ul>

<p>Try again in a few minutes.</p>

<?php
} else {
?>

<h1>Account Activation Success!</h1>
<p>You may now login to your account and begin competing!</p>
<p>To get your name up on the global leaderboard as quickly as possible, check
  out the <a href="quickstart.php">five-minute quickstart guide</a>.</p>
<p>Choose a programming language and download a starter package
  <a href="starter_packages.php">here</a>.</p>
<?php
}

include 'footer.php'
?>

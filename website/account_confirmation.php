<?php

require_once('session.php');
require_once('mysql_login.php');
$title = "Account Confirmed";
require_once('header.php');

function get_userid_from_confirmation_code($confirmation_code) {
    $confirmation_code = mysql_real_escape_string( stripslashes( $confirmation_code ) );
    $query = "
        SELECT user_id
        FROM user
        WHERE activation_code = '$confirmation_code'";
    $result = mysql_query($query);
    if (mysql_num_rows($result) > 0 ) {
        return mysql_result($result,0, 0);
    } else {
        return false;
    }
}

$errors = array();

$confirmation_code = $_GET['confirmation_code'];
if ($confirmation_code == NULL || strlen($confirmation_code) <= 0) {
    $errors[] = "Failed to activate the account. (101)";
} else {
    $user_id = get_userid_from_confirmation_code($confirmation_code);
    if (!$user_id) {
        $errors[] = "Failed to activate the account. (102)";
    } else {
        $result = activate_user($user_id);
        if (!$result) {
            echo mysql_error();
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

require 'footer.php';

<?php

require_once('session.php');

if (!logged_in_with_valid_credentials()) {
    header('index.php');
    die();
}

require_once('mysql_login.php');

// Log this login attempt
if (isset($_POST['old_password'])) {
    $old_password = $_POST['old_password'];
} else {
    $old_password = NULL;
}
$old_password = mysql_real_escape_string(stripslashes($old_password));
$new_password = mysql_real_escape_string(stripslashes($_POST['new_password']));
$confirm_password = mysql_real_escape_string(stripslashes($_POST['confirm_password']));

if ($new_password != $confirm_password) {
    $_SESSION['change_password_error'] = "Passwords do not match.";
    header("location:change_password.php");
} elseif ((isset($_SESSION['forgotten']) && $_SESSION['forgotten']) ||
          check_credentials(current_username(), $old_password)) {
    $_SESSION['forgotten'] = false;
    contest_query("change_password",
                  crypt($new_password, '$6$rounds=54321$' . salt() . '$'),
                  current_user_id());
    if (isset($_POST['remember_me'])) {
        create_user_cookie(current_user_id());
    }
    header("location:index.php");
    $_SESSION['change_password_error'] = false;
} else {
    $_SESSION['change_password_error'] = "Old password is incorrect.";
    header("location:change_password.php");
}

?>

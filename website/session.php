<?php

session_start();

function logged_in_with_valid_credentials() {
    return isset($_SESSION['user_id']);
}

/*
 * Checks if user can be logged in by cookie and logs in
 * @since 27 Oct 2011 bear@deepshiftlabs.com
 */
function try_logging_by_cookie() {
    if (!isset($_COOKIE['uid']) || !trim($_COOKIE['uid'])) {
        return false;
    }
    $parts = explode(":",$_COOKIE['uid'], 2);
    if (count($parts)<2 || !is_numeric($parts[0])) {
        return false;
    }

    $user_id = $parts[0];
    $login_cookie = $parts[1];

    global $server_info;
    require_once 'mysql_login.php';
    return check_credentials_cookie($user_id, $login_cookie);
}

function logged_in_as_admin() {
  return isset($_SESSION['user_id']) && isset($_SESSION['admin']) && $_SESSION['admin'] == 1;
}

function current_username() {
    if (isset($_SESSION['username'])) {
        return $_SESSION['username'];
    } else {
        return NULL;
    }
}

function current_user_id() {
  if( isset($_SESSION['user_id']) ) {
    return $_SESSION['user_id'];
  } else {
    return -1;
  }
}

function activate_user($user_id) {
  $query = "UPDATE user SET activated = 1 WHERE user_id = '$user_id'";
  return mysql_query($query);
}

$errors = array();
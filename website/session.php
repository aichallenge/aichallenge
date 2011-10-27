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
    if (!isset($_COOKIE['aich_login']) || !isset($_COOKIE['aich_remme']) ) {
        return false;
    }
    $username = trim($_COOKIE['aich_login']);
    $remme_value = trim($_COOKIE['aich_remme']);
    if (!$username || !$remme_value) {
        return false;
    }

    global $server_info;
    require_once 'mysql_login.php';
    return check_credentials($username, $remme_value, true);
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

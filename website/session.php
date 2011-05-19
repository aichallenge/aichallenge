<?php

session_start();

function logged_in_with_valid_credentials() {
    return isset($_SESSION['user_id']);
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

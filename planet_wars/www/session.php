<?php

// include guard
if (!isset($JPC_CONTEST_SESSION_PHP__)) {
$JPC_CONTEST_SESSION_PHP__ = 1;

session_start();
include 'mysql_login.php';

function logged_in_with_valid_credentials() {
  if (!isset($_SESSION['username']) || !isset($_SESSION['password'])) {
    return False;
  }
  $username = $_SESSION['username'];
  $password = $_SESSION['password'];
  $query = "SELECT * FROM users u " .
           "WHERE username='$username' " .
           "AND password='$password' " .
	   "AND activated = 1";
  $result = mysql_query($query);
  return mysql_num_rows($result) > 0;
}

function current_username() {
  if (logged_in_with_valid_credentials()) {
    return $_SESSION['username'];
  } else {
    return NULL;
  }
}

function current_user_id() {
  if (!isset($_SESSION['username']) || !isset($_SESSION['password'])) {
    return NULL;
  }
  $username = $_SESSION['username'];
  $password = $_SESSION['password'];
  $query = "SELECT user_id FROM contest_users " .
           "WHERE username='$username' " .
           "and password='$password'";
  // print "<p>" . $query . "<\p>";
  $result = mysql_query($query);
  if ($row = mysql_fetch_assoc($result)) {
    $user_id = $row['user_id'];
    // print "<p>user_id = " . $user_id . "</p>";
    return $user_id;
  } else {
    return -1;
  }
}

function has_permission($username, $permission_name) {
  if ($username == NULL) {
    return False;
  }
  $query = "SELECT DISTINCT u.user_id, p.permission_id FROM contest_users u " .
    "INNER JOIN contest_user_permissions up ON up.user_id = u.user_id " .
    "INNER JOIN contest_permissions p ON p.permission_id = up.permission_id " .
    "WHERE u.username = '$username' AND p.name = '$permission_name'";
  $result = mysql_query($query);
  return mysql_num_rows($result) > 0;
}

function current_user_has_permission($permission_name) {
  return has_permission(current_username(), $permission_name);
}

function permission_exists($permission_name) {
  $query = "SELECT * FROM contest_permissions WHERE name = '$permission_name'";
  $result = mysql_query($query);
  return mysql_num_rows($result) > 0;
}

// This function is deprecated, and doesn't do anything. We used to use this
// in the early contests when there was a list of permissions a user could have
// which each had a name. Now permissions are just boolean-valued fields in the
// users table in the database.
function grant_permission_to_user($username, $permission_name) {
  return False;
  //if ($username == NULL) {
  //  return False;
  //}
  //$query = "INSERT INTO contest_user_permissions (user_id, permission_id) " .
  //  "VALUES (" .
  //  "(SELECT user_id from contest_users WHERE username = '$username')" .
  //  ", " .
  //  "(SELECT permission_id FROM contest_permissions WHERE " .
  //  "name = '$permission_name')" .
  //  ")";
  //return mysql_query($query);
}

function activate_user($username) {
  if ($username == NULL) {
    return False;
  }
  $query = "UPDATE users SET activated = 1 WHERE username = '$username'";
  return mysql_query($query);
}

}
?>

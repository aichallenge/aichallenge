<?php

// include guard
if (!isset($JPC_CONTEST_CHECK_LOGIN_PHP__)) {
$JPC_CONTEST_CHECK_LOGIN_PHP__ = 1;

include 'mysql_login.php';
include 'session.php';

// A function I copied from teh internets that claims to get a person's "real"
// IP address. Not sure what that's all about, but let's log it anyways!
function getRealIpAddr()
{
    if (!empty($_SERVER['HTTP_CLIENT_IP']))
    {
      $ip=$_SERVER['HTTP_CLIENT_IP'];
    }
    elseif (!empty($_SERVER['HTTP_X_FORWARDED_FOR']))
    {
      $ip=$_SERVER['HTTP_X_FORWARDED_FOR'];
    }
    else
    {
      $ip=$_SERVER['REMOTE_ADDR'];
    }
    return $ip;
}

// Log this login attempt
$username = mysql_real_escape_string(stripslashes($_POST['username']));
$md5password = md5(mysql_real_escape_string(stripslashes($_POST['password'])));
$naive_ip = $_SERVER['REMOTE_ADDR'];
$real_ip = getRealIpAddr();
$query = "INSERT INTO login_attempts (timestamp,username,password,naive_ip," .
  "real_ip) VALUES (CURRENT_TIMESTAMP,'$username','$md5password','$naive_ip'," .
  "'$real_ip')";
$result = mysql_query($query);
if (!$result) {
  echo "<p>Could not write to log: " . htmlspecialchars(mysql_error()) . "</p>";
}

if (check_credentials($username, $md5password)) {
  header("location:index.php");
} else {
  unset($_SESSION['username']);
  unset($_SESSION['password']);
  unset($_SESSION['admin']);
  unset($_SESSION['user_id']);
  header("location:login_failed.php");
}

}  // include guard
?>

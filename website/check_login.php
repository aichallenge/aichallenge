<?php

require_once('session.php');
require_once('mysql_login.php');

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

/*
 * Sets cookies for auto login. User should be already logged in.
 * @since 27 Oct 2011 bear@deepshiftlabs.com
 */
function setRememberMeCookie()
{
    $days_to_be_logged_in = 5;
    $time_to_store = time()+ 60*60*24*$days_to_be_logged_in;
    $cookie_uid = get_cookie_uid($_SESSION['user_id']);
    if ($cookie_uid) {
        setcookie('uid', $cookie_uid, $time_to_store);
    }
}


// Log this login attempt
$username = mysql_real_escape_string(stripslashes($_POST['username']));
$password = mysql_real_escape_string(stripslashes($_POST['password']));
$naive_ip = mysql_real_escape_string($_SERVER['REMOTE_ADDR']);
$real_ip = mysql_real_escape_string(getRealIpAddr());
$query = "INSERT INTO login_attempt (timestamp,username,naive_ip," .
  "real_ip) VALUES (CURRENT_TIMESTAMP,'$username','$naive_ip'," .
  "'$real_ip')";
$result = mysql_query($query);
if (!$result) {
  echo "<p>Could not write to log: " . htmlentities(mysql_error(), ENT_COMPAT, "UTF-8") . "</p>";
}
if (check_credentials($username, $password)) {
    if (isset($_POST['remember_me'])) {
        setRememberMeCookie();
    }
    header("location:index.php");
} else {
    unset($_SESSION['username']);
    unset($_SESSION['password']);
    unset($_SESSION['admin']);
    unset($_SESSION['user_id']);
    header("location:login_failed.php");
}

?>

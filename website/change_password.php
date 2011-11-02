<?php

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

$title = "Change Password";

require_once('session.php');
require_once('mysql_login.php');

if (!logged_in_with_valid_credentials()) {
    // check for forgotten password url
    if (isset($_GET['user_id']) && isset($_GET['code'])) {
        // Log this login attempt
        $user_id = mysql_real_escape_string(stripslashes($_GET['user_id']));
        $forgot_code = mysql_real_escape_string(stripslashes($_GET['code']));
        $naive_ip = mysql_real_escape_string($_SERVER['REMOTE_ADDR']);
        $real_ip = mysql_real_escape_string(getRealIpAddr());
        
        $result = contest_query("log_login", $user_id, $naive_ip, $real_ip);
        if (!$result) {
          error_log("Could not write to log: " . mysql_error());
        }
        if (check_credentials_forgot($user_id, $forgot_code)) {
            $_SESSION['forgotten'] = true;
        } else {
            unset($_SESSION['username']);
            unset($_SESSION['password']);
            unset($_SESSION['admin']);
            unset($_SESSION['user_id']);            
            header('index.php');
            die();
        }
    } else {
        header('index.php');
        die();
    }
}

require_once('header.php');

?>

<form name="login_form" method="post" action="check_change_password.php">
  <table class="login">
  <caption><h2>Change Password</h2></caption>
<?php
if (isset($_SESSION['change_password_error']) && $_SESSION['change_password_error']) {
?>
  <tr>
    <th colspan="4" class="error"><?php echo $_SESSION['change_password_error']; ?>  Please try again.</th>
  </tr>
<?php
}
if (!isset($_SESSION['forgotten']) || $_SESSION['forgotten'] === false) {
?>
  <tr>
    <td>Old Password</td>
    <td>:</td>
    <td><input name="old_password" type="password" id="old_password"></td>
  </tr>
<?php
} 
?>
  <tr>
    <td>New Password</td>
    <td>:</td>
    <td><input name="new_password" type="password" id="new_password"></td>
  </tr>
  <tr>
    <td>Confirm Password</td>
    <td>:</td>
    <td><input name="confirm_password" type="password" id="confirm_password"></td>
  </tr>
  <tr>
    <td>&nbsp;</td>
    <td>&nbsp;</td>
    <td><label><input name="remember_me" type="checkbox" id="remember_me"> Remember me</label></td>
  </tr>
  <tr>
    <td>&nbsp;</td>
    <td>&nbsp;</td>
    <td><input type="submit" name="Submit" value="Change Password"></td>
  </tr>
  </table>
</form>

<?php

require_once('footer.php');

?>

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

if (isset($_GET['user_id']) && isset($_GET['code'])) {
    // Log this login attempt
    $user_id = mysql_real_escape_string(stripslashes($_GET['user_id']));
    $forgot_code = mysql_real_escape_string(stripslashes($_GET['code']));
    $naive_ip = mysql_real_escape_string($_SERVER['REMOTE_ADDR']);
    $real_ip = mysql_real_escape_string(getRealIpAddr());
    
    $result = contest_query("log_login", $user_id, $naive_ip, real_ip);
    if (!$result) {
      error_log("Could not write to log: " . mysql_error());
    }
    if (check_credentials_forgot($user_id, $forgot_code)) {
        $_SESSION['temp_access'] = true;
        header("location:change_password.php");
        die();
    }
}

unset($_SESSION['username']);
unset($_SESSION['password']);
unset($_SESSION['admin']);
unset($_SESSION['user_id']);

require_once('header.php');

?>

<p>
You should be receiving an email shortly with furthur instructions.  If you do
not receive an email with 5 mintues, or the instructions did not work, please
notify an admin.
</p>

<?php

include 'footer.php'

?>

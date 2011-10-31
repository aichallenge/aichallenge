<?php

require_once('session.php');
require_once('mysql_login.php');

// By default, send account confirmation emails.
$send_email = 1;
$debug = 0;

if (strcmp($server_info["mailer_address"], "donotsend") == 0) {
    $send_email = 0;
} else {
    require_once("email.php");
}

if (isset($_POST['username'])) {
    $username = mysql_real_escape_string(stripslashes($_POST['username']));
    if (list($user_id, $username, $user_email, $forgot_code) = create_user_forgot_code($username)) {
        $forgot_code = urlencode($forgot_code);
        $mail_subject = "AI Challenge Temporary Access";
        $mail_content = "Hello $username,
        
You are receiving this email because you have requested temporary access to the aichallenge.org website to reset your password.  If you did not request this access then please ignore it.  To proceed to the password reset page, click the following link:

http://aichallenge.org/change_password.php?user_id=$user_id&code=$forgot_code

If successful you will be logged in and able to change your password.  This access is only granted for 2 hours from the time you initiated the request.
        
Cheers, the aichallenge.org staff.";        
        if ($send_email == 1 && strcmp($user_email, "donotsend") != 0) {
            $mail_accepted = send_email($user_email, $mail_subject, $mail_content);
            if (!$mail_accepted) {
                die("Sorry email sending failed. Please try back later");
            }
            header("location:forgot_instructions.php");    
        } else {
            if ($debug == 1) {
                require_once('header.php');
                echo "<h3>".$mail_subject."</h3>";
                echo "<pre>".$mail_content."</pre>";
                $mail_accepted = true;
                require_once('footer.php');
            } else {
                $_SESSION['forgot_error'] = false;
                header("location:forgot.php");
            }
        }
        $_SESSION['forgot_error'] = false;
    } else {
        $_SESSION['forgot_error'] = true;
        header("location:forgot.php");
    }
}


?>

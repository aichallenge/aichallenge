<?php include 'header.php'; ?>
<?php include 'mysql_login.php'; ?>
<?php include 'bad_words.php'; ?>

<?php
function check_valid_user_status_code($code) {
  $query = "SELECT * FROM contest_user_status_codes";
  $result = mysql_query($query);
  while ($row = mysql_fetch_assoc($result)) {
    $status_id = $row['status_id'];
    if ($code == $status_id) {
      return True;
    }
  }
  return False;
}

function check_valid_organization($code) {
  if ($code == 999) {
    return False;
  }
  $query = "SELECT * FROM contest_organizations";
  $result = mysql_query($query);
  while ($row = mysql_fetch_assoc($result)) {
    $org_id = $row['org_id'];
    if ($code == $org_id) {
      return True;
    }
  }
  return False;
}

function valid_username($s) {
  return strspn($s, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.-") == strlen($s);
}

// By default, send account confirmation emails.
$send_email = 1;

// Gather the information entered by the user on the signup page.
$username = mysql_real_escape_string(stripslashes($_POST['username']));
$password1 = mysql_real_escape_string(stripslashes($_POST['password1']));
$password2 = mysql_real_escape_string(stripslashes($_POST['password2']));
$user_email = mysql_real_escape_string(stripslashes($_POST['user_email']));
$user_status = mysql_real_escape_string(stripslashes($_POST['user_status']));
$user_org = mysql_real_escape_string(stripslashes($_POST['user_organization']));
$bio = mysql_real_escape_string(stripslashes($_POST['bio']));
$country_id = mysql_real_escape_string(stripslashes($_POST['user_country']));

// Uncomment the following line to disable account creation
$errors[] = "Nuh-uh. The contest is over. No more accounts.";

// Check for bad words
if (contains_bad_word($username)) {
  $errors[] = "Your username contains a bad word. Keep it professional.";
}
if (contains_bad_word($bio)) {
  $errors[] = "Your bio contains a bad word. Keep it professional.";
}

// Check if email address is "donotsend". If so, don't send any confirmation mails. Display
// the confirmation code once the account creation finishes, and let them access the account
// activation page themselves.
if (strcmp($user_email, "donotsend") == 0) {
  $send_email = 0;
}

// Check if the username already exists.
$sql="SELECT * FROM contest_users WHERE username='$username'";
$result = mysql_query($sql);
if (mysql_num_rows($result) > 0) {
  $errors[] = "The username $username is already in use. Please choose a different username.";
}

// Check if the email is already in use (except by an admin account or a donotsend account).
if (strcmp($user_email, "donotsend") != 0) {
  $sql="select u.username, u.email, MIN(up.permission_id) from contest_users u INNER JOIN contest_user_permissions up ON up.user_id = u.user_id where u.email = '$user_email' GROUP BY u.email HAVING MIN(up.permission_id) > 1";
  $result = mysql_query($sql);
  if ($result && mysql_num_rows($result) > 0) {
    $errors[] = "The email $user_email is already in use. You are only allowed to have one account! It is easy for us to tell if you have two accounts, and you will be disqualified if you have two accounts! If there is some problem with your existing account, get in touch with the contest organizers by dropping by the Computer Science Club office and we will help you get up-and-running again!  :-)";
  }
}

// Check if the email is made up of the right kinds of characters
if (!valid_username($username)) {
  $errors[] = "Invalid username. Your username must be longer than 6 characters and composed only of the characters a-z, A-Z, 0-9, '-', '_', and '.'";
}

// Check that the username is between 8 and 16 characters long
if (strlen($username) < 6 || strlen($username) > 16) {
  $errors[] = "Your username must be between 6 and 16 characters long.";
}

// Check that the two passwords given match.
if ($password1 != $password2) {
  $errors[] = "You made a mistake while entering your password. "
            . "The two passwords that you give should match.";
}

// Check that the desired password is long enough.
if (strlen($password1) < 8) {
  $errors[] = "Your password must be at least 8 characters long.";
}

// Check that the email address is not blank.
if (strlen($user_email) <= 0) {
  $errors[] = "You must provide an email address. The email address that you specify will be used to activate your account.";
}

// Check that the user status code is valid.
if (!check_valid_user_status_code($user_status)) {
  $errors[] = "The status you selected is invalid. Please contact the contest staff.";
}

// Check that the user organziation code is valid.
if (!check_valid_organization($user_org)) {
  $errors[] = "The organization you selected is invalid. Please contact the contest staff.";
}

// Check that the country code is not empty.
if (strlen($country_id) <= 0) {
  $errors[] = "You did not select a valid country from the dropdown box.";
}

if (count($errors) <= 0) {
  // Add the user to the database, with no permissions.
  $confirmation_code =
    md5($username . "d3j7k4nh8dvs0" . $password1 . $username);
  $query = "SELECT org.name AS name, COUNT(u.user_id) AS peers FROM " .
    "contest_organizations org " .
    "LEFT OUTER JOIN contest_users u ON u.org_id = org.org_id GROUP BY " .
    "org.org_id HAVING org.org_id = " . $user_org;
  $result = mysql_query($query);
  $peer_message = "";
  $org_name = "";
  $num_peers = "";
  if ($result) {
    if ($row = mysql_fetch_assoc($result)) {
      $org_name = $row['name'];
      $num_peers = $row['peers'];
      if ($num_peers == 0) {
        $peer_message = "You are the first person from your school to sign up " .
          "for the Google AI Challenge. We would really appreciate it if you would " .
          "encourage your friends to sign up for the Challenge as well. The more, " .
          "the merrier!\n\n";
      } else {
        $peer_message = "" . $num_peers . " other people from " . $org_name .
          " have already signed up for the Google AI Challenge. When you look " .
          "at the rankings, you can see the global rankings for all schools, or " .
          "you can filter the list to only show other contestants from your school!\n\n";
      }
    }
  }
  $query = "INSERT INTO contest_users " .
    "(username,password,email,status_id,activation_code,org_id,bio,country_id,created) VALUES " .
    "('$username','" . md5($password1) . "','$user_email',$user_status," .
    "'$confirmation_code',$user_org,'$bio',$country_id,CURRENT_TIMESTAMP)";
  if (mysql_query($query)) {
    // Send confirmation mail to user.
    $mail_subject = "Google AI Challenge!";
    $mail_content = "Welcome to the contest! Click the link below in order " .
      "to activate your account.\n\n" .
      "http://csclub.uwaterloo.ca/contest/account_confirmation.php" .
      "?confirmation_code=" . $confirmation_code . "\n\n" .
      "After you activate your account by clicking the link above, you will " .
      "be able to sign in and start competing. Good luck!\n\n" .
      $peer_message . "Contest Staff\nUniversity of Waterloo Computer Science Club";
    if ($send_email == 1) {
      $mail_accepted = mail($user_email, $mail_subject, $mail_content);
    } else {
      $mail_accepted = true;
    }
    if (!$mail_accepted) {
      $errors[] = "Failed to send confirmation email. Try again in a few " .
        "minutes.";
      $query = "DELETE FROM contest_users WHERE username='$username' and " .
        "password='" . md5($password1) . "'";
      mysql_query($query);
    } else {
      // Send notification mail to contest admin.
      $mail_subject = "New Contest User";
      $mail_content = "username = " . $username . "\nOrganizationID = " . $user_org .
        "\nUser number " . ($num_peers + 1) . " from " . $org_name;
      if ($send_email == 1) {
        $mail_accepted = mail("cameron.jp@gmail.com", $mail_subject, $mail_content);
      } else {
        $mail_accepted = true;
      }
      if (!$mail_accepted) {
        $errors[] = "Failed to send confirmation email. Try again in a few " .
          "minutes.";
        $query = "DELETE FROM contest_users WHERE username='$username' and " .
          "password='" . md5($password1) . "'";
        mysql_query($query);
      } else {
        grant_permission_to_user($username, "exists");
      }
    }
  } else {
    $errors[] = "Failed to communicate with the registration database. Try " .
      "again in a few minutes. ($query : " . mysql_error() . ")";
  }
}
if (count($errors) > 0) {
?>

<h1>Registration Failed</h1>
<p>There was a problem with the information that you gave.</p>
<ul>

<?php
foreach ($errors as $key => $error) {
  print "<li>$error</li>";
}
?>

</ul>
<p>Go <a href="signup.php">back to the signup page</a> and try again.</p>

<?php
} else {
?>

<h1>Registration Successful!</h1>
<p>Thank you for registering for the contest! A confirmation
   message will be sent to the email address that you provided.
   You must click the link in that message in order to activate
   your account.</p>
<h2>Check Your Junk Mail Folder</h2>
<p>If you don't see it in five minutes, remember to check your
   junk mail folder. Some free email providers are known to
   mistake confirmation emails for junk mail. Before you even think
   of sending us mail asking for help, <strong>check your junk mail
   folder!</strong></p>
<p><a href="index.php">Back to the home page.</a></p>

<?php

if ($send_email == 0) {
  echo "<p>$confirmation_code</p>";
}

}  // end if
?>

<?php include 'footer.php'; ?>

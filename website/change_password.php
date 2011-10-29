<?php

$title = "Change Password";

require_once('session.php');

if (!logged_in_with_valid_credentials()) {
    header('index.php');
    die();
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
if (!isset($_SESSION['temp_access'])) {
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
    <td><input name="remember_me" type="checkbox" id="remember_me"> Remember me</td>
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

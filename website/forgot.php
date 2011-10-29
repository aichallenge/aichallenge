<?php

$title="Forgotten Password";
include 'header.php'

?>

<form name="login_form" method="post" action="check_forgot.php">
  <table class="login">
  <caption><h2>Forgotten Password?</h2></caption>
<?php
if (isset($_SESSION['forgot_error']) && $_SESSION['forgot_error']) {
?>
  <tr>
    <th colspan="4" class="error">The user name supplied was not found in our records.</th>
  </tr>
<?php
}
?>
  <tr>
    <td>Username</td>
    <td>:</td>
    <td><input name="username" type="text" id="username"></td>
    <td>To get temporary access to your account, type the user name you used to register your AI Challenge account.</td>
  </tr>
  <tr>
    <td>&nbsp;</td>
    <td>&nbsp;</td>
    <td><input type="submit" name="Submit" value="Request Temporary Access"></td>
    <td>&nbsp;</td>
  </tr>
  </table>
</form>

<?php include 'footer.php' ?>

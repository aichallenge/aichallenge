<?php

$title="Login";
include 'header.php'

?>

<form name="login_form" method="post" action="check_login.php">
  <table class="login">
  <caption><h2>Login</h2></caption>
  <tr>
    <td>Username</td>
    <td>:</td>
    <td><input name="username" type="text" id="username"></td>
  </tr>
  <tr>
    <td>Password</td>
    <td>:</td>
    <td><input name="password" type="password" id="password"></td>
  </tr>
  <tr>
    <td>&nbsp;</td>
    <td>&nbsp;</td>
    <td><label><input name="remember_me" type="checkbox" id="remember_me"> Remember me</label></td>
  </tr>
  <tr>
    <td>&nbsp;</td>
    <td>&nbsp;</td>
    <td><input type="submit" name="Submit" value="Login"></td>
  </tr>
  <tr>
    <th colspan="3"><a href="forgot.php">Forgot your password?</a></th>
  </tr>
  <tr>
    <td colspan="3"><h3>-or-<br /><br /><a href="register.php">Create an account</a></h3></td>
  </tr>
  </table>
</form>

<?php include 'footer.php' ?>

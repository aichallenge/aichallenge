<?php include 'header.php' ?>

<h2>Login</h2>
<form name="login_form" method="post" action="check_login.php">
  <table width="100%" border="0" cellpadding="3" cellspacing="5">
  <tr>
    <td width="78">Username</td>
    <td width="6">:</td>
    <td width="294"><input name="username" type="text" id="username"></td>
  </tr>
  <tr>
    <td>Password</td>
    <td>:</td>
    <td><input name="password" type="password" id="password"></td>
  </tr>
  <tr>
    <td>&nbsp;</td>
    <td>&nbsp;</td>
    <td><input type="submit" name="Submit" value="Login"></td>
  </tr>
  <tr>
    <td>&nbsp;</td>
    <td>&nbsp;</td>
    <td><a href="register.php">Create an account</a></td>
  </tr>
  </table>
</form>

<?php include 'footer.php' ?>

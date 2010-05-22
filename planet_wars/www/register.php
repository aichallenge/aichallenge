<?php include 'header.php' ?>
<?php include 'mysql_login.php'; ?>

<h2>Create Your Account</h2>
<form name="create_account_form" method="post"
  action="process_registration.php">
<table border="0" width="100%" cellspacing="10">
  <!-- Username -->
  <tr>
    <td>Username</td>
    <td>&nbsp;</td>
    <td><input name="username" type="text" id="username"></td>
    <td>Your username must be at least 6 characters and composed only of the
        characters a-z, A-Z, 0-9, '-', '_', and '.'</td>
  </tr>
  <!-- Password -->
  <tr>
    <td>Password</td>
    <td>&nbsp;</td>
    <td><input name="password1" type="password" id="password1"></td>
    <td>Must be at least 8 characters long. Must contain at least one number
        and one letter.</td>
  </tr>
  <!-- Confirm Password -->
  <tr>
    <td>Confirm Password</td>
    <td>&nbsp;</td>
    <td><input name="password2" type="password" id="password2"></td>
    <td>&nbsp;</td>
  </tr>
  <!-- Email -->
  <tr>
    <td>Email Address</td>
    <td>&nbsp;</td>
    <td><input name="user_email" type="text" id="user_email"></td>
    <td>You can use any email address. You will use this address to confirm
        the creation of your account.</td>
  </tr>
  <!-- Status -->
  <tr>
    <td>Status</td>
    <td>&nbsp;</td>
    <td>
      <select name="user_status">
<?php
  $query = "SELECT * FROM user_status_codes";
  $result = mysql_query($query);
  while ($row = mysql_fetch_assoc($result)) {
    $status_id = $row['status_id'];
    $status_name = $row['name'];
    echo "<option value=$status_id>$status_name</option>";
  }
?>
      </select>
    </td>
    <td>Choose the option that best describes your academic status.
      First-year students may be eligible for special prizes.</td>
  <!-- School -->
  <tr>
    <td>Organization</td>
    <td>&nbsp;</td>
    <td>
      <select name="user_organization" style="width:210px">
      <option value="0">Other</option>
      <option value="1">University of Waterloo</option>
      <option value="999">---</option>
<?php
  $query = "SELECT * FROM organizations WHERE org_id > 1 ORDER BY name";
  $result = mysql_query($query);
  while ($row = mysql_fetch_assoc($result)) {
    $status_id = $row['org_id'];
    $org_name = $row['name'];
    echo "<option value=$status_id>$org_name</option>";
  }
?>
      </select>
    </td>
    <td>If you are a student, select your school. If you are an
      employee, select your employer. Otherwise, select 'Other'.</td>
  </tr>
  <!-- Country -->
  <tr>
    <td>Country</td>
    <td>&nbsp;</td>
    <td>
      <select name="user_country" style="width:210px">
      <option value="">&nbsp;</option>
<?php
  $query = "SELECT * FROM countries ORDER BY country_id";
  $result = mysql_query($query);
  while ($row = mysql_fetch_assoc($result)) {
    $country_id = $row['country_id'];
    $country_name = $row['name'];
    echo "<option value=$country_id>$country_name</option>";
    if ($country_id == 11) {
      echo "<option value=999>---</option>";
    }
  }
?>
      </select>
    </td>
    <td>Choose the country where you currently reside.</td>
  </tr>
  <!-- Biographical Information -->
  <tr>
    <td>About You</td>
    <td>&nbsp;</td>
    <td><textarea name="bio" rows="3" cols="20"></textarea></td>
    <td>Introduce yourself. Other contestants will be able to see this on
      your profile. Optional</td>
  </tr>
  </tr>
  <tr><td colspan="4" style="color:red">By submitting this form, you implicity agree to <a href="rules.php">these rules</a>.</td></tr>
  <tr><td colspan="4" style="color:red">We take security very seriously. Any attempt to compromise the integrity of the contest or CSC servers will result in the immediate involvement of law enforcement officials.</td></tr>
  <!-- Submit -->
  <tr>
    <td>&nbsp;</td>
    <td>&nbsp;</td>
    <td><input type="submit" name="Submit" value="Create My Account!"></td>
    <td>&nbsp;</td>
  </tr>
</table></form>

<?php include 'footer.php' ?>

<?php

$title="Create Your Account";
require_once 'header.php';
require_once 'mysql_login.php';
require_once 'server_info.php';

if($server_info["submissions_open"]) { ?>
<script>
    function onClickOrganization(element) {
        var otherOrg = document.getElementById('user_organization_other');
        if(element.selectedIndex === 1) {
            otherOrg.disabled = false;
            otherOrg.focus();
        } else {
            otherOrg.disabled = true;
            otherOrg.value = '';
        }
    }
</script>
<h2>Create Your Account</h2>
<?php if (count($errors) > 0) { ?>

<p>There was a problem with the information that you gave.</p>
<ul>

<?php
    foreach ($errors as $key => $error) {
      print "<li>$error</li>";
    }
}
?>


<form name="create_account_form" method="post"
  action="process_registration.php">
<table border="0" width="100%" cellspacing="10">
  <!-- Username -->
  <tr>
    <td>Username</td>
    <td>&nbsp;</td>
    <td><input name="username" type="text" id="username" value="<?php echo isset($_POST['username'])?htmlentities($_POST['username'], ENT_COMPAT, "UTF-8"):'' ?>" /></td>
    <td>Your username must be at least 6 characters and composed only of the
        characters a-z, A-Z, 0-9, '-', '_', and '.'</td>
  </tr>
  <!-- Password -->
  <tr>
    <td>Password</td>
    <td>&nbsp;</td>
    <td><input name="password1" type="password" id="password1" /></td>
    <td>Should be at least 8 characters long and contain at least one number
        and one letter.</td>
  </tr>
  <!-- Confirm Password -->
  <tr>
    <td>Confirm Password</td>
    <td>&nbsp;</td>
    <td><input name="password2" type="password" id="password2" /></td>
    <td>&nbsp;</td>
  </tr>
  <!-- Email -->
  <tr>
    <td>Email Address</td>
    <td>&nbsp;</td>
    <td><input name="user_email" type="text" id="user_email" value="<?php echo isset($_POST['user_email'])?htmlentities($_POST['user_email'], ENT_COMPAT, "UTF-8"):'' ?>" /></td>
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
  $query = "SELECT * FROM user_status_code";
  $result = mysql_query($query);
  while ($row = mysql_fetch_assoc($result)) {
    $status_id = $row['status_id'];
    $status_name = $row['name'];
    if ( isset( $_POST['user_status'] ) && $_POST['user_status'] == $status_id ) $selected = ' selected="selected"';
    else $selected = '';
    echo "<option value=\"$status_id\"$selected>$status_name</option>";
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
      <select name="user_organization" style="width:210px" onClick="onClickOrganization(this)" onChange="onClickOrganization(this)">
<?php
    $organizations = array(
        array( 'org_id' => '0', 'name' => 'None' ),
        array( 'org_id' => '-1', 'name' => 'Other' ),
        array( 'org_id' => '1', 'name' => 'University of Waterloo' ),
        array( 'org_id' => '999', 'name' => '---' ),

    );
  $query = "SELECT * FROM organization WHERE org_id > 1 ORDER BY name";
  $result = mysql_query($query);
  while ($row = mysql_fetch_assoc($result)) {
      $organizations []= $row;
  }

  foreach ( $organizations as $row ) {
    $org_id = $row['org_id'];
    $org_name = htmlentities($row['name'], ENT_COMPAT, "UTF-8");
    if ( isset( $_POST['user_organization'] ) && $_POST['user_organization'] == $org_id ) $selected = ' selected="selected"';
    else $selected = '';

    echo "<option value=\"$org_id\"$selected>$org_name</option>";
  }
?>
      </select>
    </td>
    <td>If you are a student, select your school. If you are an
      employee, select your employer. Otherwise, select 'None'.</td>
  </tr>
  <tr>
      <td>Other</td>
        <td>&nbsp;</td>
        <td>
            <input name="user_organization_other" id="user_organization_other" <?php if (empty($_POST['user_organization']) || $_POST['user_organization'] !== '-1') {
                ?> disabled="true"<?php } else { ?> value="<?php echo isset($_POST['user_organization_other'])?htmlentities($_POST['user_organization_other'], ENT_COMPAT, "UTF-8"):'' ?>"<?php } ?>  />
        </td>
        <td>If your organization isn't present in drowdown, select 'Other' and enter your organization.</td>
  </tr>
  <!-- Country -->
  <tr>
    <td>Country</td>
    <td>&nbsp;</td>
    <td>
      <select name="user_country" style="width:210px">
      <option value="">&nbsp;</option>
<?php
  $query = "SELECT * FROM country ORDER BY country_id";
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
    <td><textarea name="bio" rows="3" cols="20"><?php echo isset($_POST['bio'])?htmlentities($_POST['bio'], ENT_COMPAT, "UTF-8"):'' ?></textarea></td>
    <td>Introduce yourself. Other contestants will be able to see this on
      your profile. Optional</td>
  </tr>
  </tr>
  <tr><td colspan="4" style="color:red">By submitting this form, you implicity agree to <a href="rules.php">these rules</a>.</td></tr>
  <!-- Submit -->
  <tr>
    <td>&nbsp;</td>
    <td>&nbsp;</td>
    <td><input type="submit" name="Submit" value="Create My Account!"></td>
    <td>&nbsp;</td>
  </tr>
</table></form>

<?php } else { ?>

<h2>Account Creation Closed</h2>

<?php } ?>

<?php require_once 'footer.php' ?>

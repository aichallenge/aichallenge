<?php include 'session.php'; ?>
<?php
function check_valid_organization($id) {
  if ($id == 999) {
    return False;
  }
  $query = "SELECT count(*) FROM organizations where org_id=". $id;
  $result = mysql_query($query);
  $row = mysql_fetch_assoc($result);
  if ($row['count(*)'] > 0)
    return True;
  return False;
}

function check_valid_country($id) {
  if ($id == 999) {
    return False;
  }
  $query = "SELECT count(*) from countries where country_id=". $id;
  $result = mysql_query($query);
  $row = mysql_fetch_assoc($result);
  if ($row['count(*)'] > 0)
    return True;
  return False;
}

if (!logged_in_with_valid_credentials())
  die ("Nice try.. but the bad robot WILL get you");

if (!array_key_exists('chk', $_POST)
    || !array_key_exists('update_key', $_POST)) {
  die("Check keys not found.");
}

$user_id = current_user_id();

$query = "SELECT email, activation_code FROM users WHERE user_id=".$user_id;
$result = mysql_query($query);
if ($row = mysql_fetch_assoc($result)) {
  $chk = $_POST['chk'];
  $update_key = $_POST['update_key'];
  $local_key = sha1($chk . $row['activation_code'] . $row['email']);
  if ($local_key != $update_key) {
    die("Bad update key found.");
  }
} else {
  die("User data query failed.");
}

if (array_key_exists('user_country', $_POST)) {
  $country_id = $_POST['user_country'];
  if (!check_valid_country($country_id))
    die("Invalid country id: ". $country_id);
  $query = "UPDATE users SET country_id='". $country_id ."' WHERE user_id='".
      $user_id ."';";
  if (!mysql_query($query))
    die("Sorry, database update failed.");
}

if (array_key_exists('user_organization', $_POST)) {
  $code = $_POST['user_organization'];
  if (!check_valid_organization($code))
    die("Invalid organization id: ". $code);
  $query = "UPDATE users SET org_id='". $code ."' WHERE user_id='".
      $user_id ."';";
  if (!mysql_query($query))
    die("Sorry, database update failed.");
}

header('Location: profile.php?user_id='. $user_id);
?>

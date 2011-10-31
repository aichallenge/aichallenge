<?php
require_once('session.php');
require_once('mysql_login.php');
require_once('file_system_util.php');
require_once('submission.php');

$title="Upload Verification";
include 'header.php';

function ends_with($str, $sub) {
  return preg_match('/\Q' . $sub . '\E$/', $str);
}

function upload_errors($errors) {
  $post_max_size = intval(str_replace('M', '', ini_get('post_max_size'))) * 1024 * 1024;
  $content_length = intval($_SERVER['CONTENT_LENGTH']);
  if ($content_length > $post_max_size) {
    $errors[] = "Your zip file may be larger than the maximum allowed " .
      "size, ".ini_get('upload_max_filesize').". " .
      "You probably have some executables or other larger " .
      "files in your zip file. Re-zip your submission, being sure to " .
      "include only the source code.";
  } elseif (count($_FILES) == 0 || count($_FILES['uploadedfile']) == 0
      || strlen($_FILES['uploadedfile']['name']) == 0
      || $_FILES['uploadedfile']['error'] == UPLOAD_ERR_NO_FILE) {
    $errors[] = "Somehow you forgot to upload a file!";
  } elseif ($_FILES['uploadedfile']['error'] > 0) {
    if ($_FILES['uploadedfile']['error'] == UPLOAD_ERR_FORM_SIZE or
        $_FILES['uploadedfile']['error'] == UPLOAD_ERR_INI_SIZE) {
      $errors[] = "Your zip file may be larger than the maximum allowed " .
        "size, ".ini_get('upload_max_filesize').". " .
        "You probably have some executables or other larger " .
        "files in your zip file. Re-zip your submission, being sure to " .
        "include only the source code.";
    } else {
      $errors[] = "General upload error: " . $_FILES['uploadedfile']['error'];
    }
  } else {
    $filename = basename($_FILES['uploadedfile']['name']);
    if (!ends_with($filename, ".zip") &&
        !ends_with($filename, ".tar.xz") &&
        !ends_with($filename, ".tar.bz2") &&
        !ends_with($filename, ".txz") &&
        !ends_with($filename, ".tbz") &&
        !ends_with($filename, ".tgz") &&
        !ends_with($filename, ".tar.gz")) {
      $errors[] = "Invalid file type. Must be zip, tgz, tar.gz, tbz, tar.bz2, txz, or tar.xz";
    }
  }
  return $errors;
}

if (!logged_in_with_valid_credentials()) {
  header('Location: index.php');
  die();
}

$result = mysql_query("SELECT * FROM user WHERE user_id = ".current_user_id());
$userdata = mysql_fetch_assoc($result);
$sid = session_id();
$local_key = sha1($sid . $userdata['activation_code'] . $userdata['email']);
if (!isset($_POST['submit_key']) || $local_key != $_POST['submit_key']) {
  die('Bad submission key found.');
}

if(!$server_info["submissions_open"])
    $errors[] = "Nuh-uh. The contest is over. No more submissions.";

if (count($errors) == 0) {
  if (has_recent_submission()) {
    $errors[] = "Sorry your last submission was too recent.";
  } else {
    $errors = upload_errors($errors);
  }
}

if (count($errors) == 0) {
  if (!create_new_submission_for_current_user()) {
    $errors[] = "Problem while creating submission entry in database. ".mysql_error();
  }
}

if (count($errors) == 0) {
  $submission_id = current_submission_id();
  $destination_folder = submission_directory($submission_id);
  $filename = basename($_FILES['uploadedfile']['name']);
  if (ends_with($filename, ".zip")) { $filename = "entry.zip"; }
  if (ends_with($filename, ".tar.gz")) { $filename = "entry.tar.gz"; }
  if (ends_with($filename, ".tgz")) { $filename = "entry.tgz"; }
  if (ends_with($filename, ".tar.xz")) { $filename = "entry.tar.xz"; }
  if (ends_with($filename, ".txz")) { $filename = "entry.txz"; }
  if (ends_with($filename, ".tar.bz2")) { $filename = "entry.tar.bz2"; }
  if (ends_with($filename, ".tbz")) { $filename = "entry.tbz"; }
  $target_path = $destination_folder . '/' . $filename;
  delete_directory($destination_folder);
  if (!mkdir($destination_folder, 0775, true)) {
      update_current_submission_status(90);
      $errors[] = "Problem while creating submission directory.";
  } else {
    if (!move_uploaded_file($_FILES['uploadedfile']['tmp_name'], $target_path)) {
      update_current_submission_status(90);
      $errors[] = "Failed to move file from temporary to permanent " .
                  "location.";
      update_current_submission_status(30);
    } else {
      chmod($destination_folder, 0775);
      chmod($target_path, 0664);
      if (!update_current_submission_status(20)) {
        $errors[] = "Failed to update the submission status in the " .
                    "database.";
      }
    }
  }
}

if (count($errors) == 0) {
?>

<h1>Success!</h1>
<p>Your entry was successfully uploaded to the contest server. Check
  your profile in a few minutes to see if your code compiled successfully.
  If your code compiled, then
  it will begin playing in a virtual tournament against other people's entries.
  Be sure to check the leaderboard in a while to see how your entry ranks!</p>
<p>If your code does not compile successfully, then you can see the compiler
  errors on your profile.  Fix as many of the errors as
  you can, and then resubmit your code!</p>
<p><em>Note: Submitting a new bot will not change your order in the queue to play next.  A new bot will have its skill reset to 0.</em>

<?php
} else {
?>

<h1>Failure</h1>
<p>There was a problem with your submission.</p>
<ul>

<?php
foreach ($errors as $key => $error) {
  print "<li>$error</li>";
}
?>

</ul>
<p>Go <a href="submit.php">back to the upload page</a> and try again.</p>

<?php
}

include 'footer.php';
?>

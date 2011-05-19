<?php
require_once('session.php');
require_once('mysql_login.php');
require_once('file_system_util.php');
require_once('submission.php');

include 'header.php';

function ends_with($str, $sub) {
  return preg_match('/\Q' . $sub . '\E$/', $str);
}

function upload_errors($errors) {
  if (count($_FILES) == 0 || count($_FILES['uploadedfile']) == 0
      || strlen($_FILES['uploadedfile']['name']) == 0
      || $_FILES['uploadedfile']['error'] == UPLOAD_ERR_NO_FILE) {
    $errors[] = "Somehow you forgot to upload a file!";
  } elseif ($_FILES['uploadedfile']['error'] > 0) {
    $errors[] = "General upload error: " . $_FILES['uploadedfile']['error'];
    if ($_FILES['uploadedfile']['error'] == UPLOAD_ERR_FORM_SIZE) {
      $errors[] = "Your zip file may be larger than the maximum allowed " .
        "size, 1 MB. You probably have some executables or other larger " .
        "files in your zip file. Re-zip your submission, being sure to " .
        "include only the source code.";
    }
  } else {
    $file_size = $_FILES['uploadedfile']['size'];
    if ($file_size > 2000000) {
      $errors[] = "File is too big. Maximum size is 1 KB. Make sure that " .
                  "your zip file contains only your code files.";
    } else {
      $filename = basename($_FILES['uploadedfile']['name']);
      if (!ends_with($filename, ".zip") &&
          !ends_with($filename, ".tgz") &&
          !ends_with($filename, ".tar.gz")) {
        $errors[] = "Invalid file type. Must be zip, tgz, or tar.gz";
      }
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
if ($local_key != $_POST['submit_key']) {
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
<p>Your entry was successfully uploaded to the contest server. Some time in the
  next five minutes you should receive an email letting you know whether or not
  we successfully compiled your code. If your code compiles successfully, then
  it will begin playing in a virtual tournament against other people's entries.
  Be sure to check the leaderboard in a while to see how your entry ranks!</p>
<p>If your code does not compile successfully, then you will receive an email
  with the list of errors. In this case, you should fix as many of the errors as
  you can, and then resubmit your code.</p>
  
<p>If your new submission doesn't show up on your profile page, under submissions, in about ten minutes, you can try resubmitting. We are working on fixing this.</p>

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

<?php
include 'session.php';
include 'file_system_util.php';
include 'submission.php';

function ends_with($str, $sub) {
  return preg_match('/\Q' . $sub . '\E$/', $str);
}

$submission_directory = "/home/contest/ai-contest/planet_wars/submissions/";
if (!logged_in_with_valid_credentials()) {
  header('index.php');
}
if (!setup_submission_directory($submission_diectory)) {
  $errors[] = "Problem while creating submission directory.";
} else {
  $destination_folder = $submission_directory . current_submission_id();
  $filename = basename($_FILES['uploadedfile']['name']);
  if (ends_with($filename, ".zip")) { $filename = "entry.zip"; }
  if (ends_with($filename, ".tar.gz")) { $filename = "entry.tar.gz"; }
  if (ends_with($filename, ".tgz")) { $filename = "entry.tgz"; }
  $target_path = $destination_folder . '/' . $filename;
  delete_directory($destination_folder);
  mkdir($destination_folder);
  if ($_FILES['uploadedfile']['error'] > 0) {
    $errors[] = "General upload error: " . $_FILES['uploadedfile']['error'];
    if ($_FILES['uploadedfile']['error'] == UPLOAD_ERR_FORM_SIZE) {
      $errors[] = "Your zip file may be larger than the maximum allowed " .
        "size, 1 MB. You probably have some executables or other larger " .
        "files in your zip file. Re-zip your submission, being sure to " .
        "include only the source code.";
    }
    update_current_submission_status(30);
  } else {
    $file_size = $_FILES['uploadedfile']['size'];
    if ($file_size > 1000000) {
      $errors[] = "File is too big. Maximum size is 1 KB. Make sure that " .
      "your zip file contains only your code files, run.sh, and compile.sh.";
      update_current_submission_status(30);
    } else {
      if (!ends_with($filename, ".zip") &&
          !ends_with($filename, ".tgz") &&
          !ends_with($filename, ".tar.gz")) {
        $errors[] = "Invalid file type. Must be zip, tgz, or tar.gz";
        update_current_submission_status(30);
      } else {
        if (!move_uploaded_file($_FILES['uploadedfile']['tmp_name'],
                                $target_path)) {
          $errors[] = "Failed to move file from temporary to permanent " .
            "location.";
          update_current_submission_status(30);
        } else {
          chmod($destination_folder, 0777);
          chmod($target_path, 0777);
          if (!update_current_submission_status(20)) {
            $errors[] = "Failed to update the submission status in the " .
              "database.";
          }
        }
      }
    }
  }
}
// Uncomment the following line to turn off new submissions.
//$errors[] = "Nuh-uh. The contest is over. No more submissions.";
include 'header.php';
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

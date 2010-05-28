<?php

include 'mysql_login.php';

$submission_id = $argv[1];
$query = "SELECT u.user_id, u.username, u.email FROM contest_users u INNER JOIN contest_submissions s ON s.user_id = u.user_id WHERE s.submission_id = " . $submission_id;
$result = mysql_query($query);
if ($row = mysql_fetch_assoc($result)) {
  $username = $row['username'];
  $user_id = $row['user_id'];
  $email_address = $row['email'];
} else {
  die();
}
if (strcmp($email_address, "donotsend") == 0) {
   die(); // donotsend is a dummy email address for test accounts. Never actually send mail.
}
$contest_user_directory = "/users/contest2/tron/";
$entries_directory = "compile/";
$compiler_output_directory =
  $contest_user_directory . $entries_directory . $submission_id;
$compile_output = file_get_contents(
  $compiler_output_directory . "/compile_output.txt");
$compile_error = file_get_contents(
  $compiler_output_directory . "/compile_error.txt");
$submission_error = file_get_contents(
  $compiler_output_directory . "/submission_error.txt");
// Hide the real entries directory from users.
$compile_output = str_replace($entries_directory,
                              "all_entries/submit/a34g/",
                              $compile_output);
$compile_error = str_replace($entries_directory,
                              "all_entries/submit/a34g/",
                              $compile_error);
$submission_error = str_replace($entries_directory,
                              "all_entries/submit/a34g/",
			      $submission_error);
if (strlen(trim($compile_output)) == 0) {
  $compile_output = "There was no compiler output. This is probably a good thing!";
}
if (strlen(trim($compile_error)) == 0) {
  $compile_error = "There were no compiler errors. You must be feeling lucky!";
}
if (strlen(trim($submission_error)) == 0) {
  $submission_error = "The system did not produce any general errors or comments about your submission. Fortune has smiled on you!";
}
$body = "Hello, " . $username . "! This is an automated email from the University of Waterloo Computer Science Club, letting you know that your latest code submission compiled successfully. You should be able to see your entry's ranking on the leaderboard within a few minutes or hours.\n\n";
$body = $body . "If you're not quite sure what to do, or you feel as though you're stuck or discouraged, or if you have any questions at all, you can get help! Log in to the contest webpage at http://csclub.uwaterloo.ca/contest and click the Help link on the left of the screen.\n\n";
$body = $body . "GENERAL ERRORS:\n\n";
$body = $body . $submission_error . "\n\n";
$body = $body . "COMPILER ERRORS:\n\n";
$body = $body . $compile_error . "\n\n";
$body = $body . "COMPILER OUTPUT:\n\n";
$body = $body . $compile_output . "\n\n";
$body = $body . "Sincerely,\nYour Friendly Contest Organizers\nThe University of Waterloo Computer Science Club";
$subject = "[Contest] Submission Success";
$user_mail_status = mail($email_address, $subject, $body);
$admin_body = "Submission " . $submission_id . " compiled successfully.\nusername: " . $username . "\nemail: " . $email_address . "\nreturn_value_from_php_mail_function: " . $user_mail_status;
mail("cameron.jp@gmail.com", "[Contest] Successful Compilation", $admin_body);
?>

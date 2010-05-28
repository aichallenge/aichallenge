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
  mail("cameron.jp@gmail.com", "[Contest] Important Error!", "send_compile_fail_mail.php script experienced a critical failure! This means that users are going to be very angry and confused!");
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
  $submission_error = "The system did not produce any general errors or comments about your submission. This is probably a good thing!";
}
$body = "Hello, " . $username . "! This is an automated email from the University of Waterloo Computer Science Club, letting you know that the system failed to compile your latest submission. In this email, I have included as much information as I can about what went wrong, to help you fix the problem. After you have fixed as many problems as you can, try submitting your code again.\n\n";
$body = $body . "If you're not quite sure what to do, or you feel as though you're stuck or discouraged, or if you have any questions at all, you can get help! Log in to the contest webpage at http://csclub.uwaterloo.ca/contest and click the Help link on the left of the screen. If an organizer is awake, you will likely get help right away. If all the organizers are asleep because it's 3:00 AM, then you might have to wait a few hours for a reply.\n\n";
$body = $body . "GENERAL ERRORS:\n\n";
$body = $body . $submission_error . "\n\n";
$body = $body . "COMPILER ERRORS:\n\n";
$body = $body . $compile_error . "\n\n";
$body = $body . "COMPILER OUTPUT:\n\n";
$body = $body . $compile_output . "\n\n";
$body = $body . "Sincerely,\nYour Friendly Contest Organizers\nThe University of Waterloo Computer Science Club";
$subject = "[Contest] Submission Failure";
$user_mail_status = mail($email_address, $subject, $body);
$admin_body = "Submission " . $submission_id . " failed to compile.\nusername: " . $username . "\nemail: " . $email_address . "\nreturn_value_from_php_mail_function: " . $user_mail_status;
mail("cameron.jp@gmail.com", "[Contest] Failed Compilation", $admin_body);

?>

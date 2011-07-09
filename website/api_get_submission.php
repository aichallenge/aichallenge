<?php
require_once('api_functions.php');
require_once('server_info.php');
require_once('submission.php');

/*
  Sends a bot's upload to a game server.
 */

$submission_id = $_GET['submission_id'];
if(!filter_var($submission_id, FILTER_VALIDATE_INT) || $submission_id == '') {
  die();
}

/*
 * switch to the submission's directory making sure it's actually there
 */
if (!chdir(submission_directory($submission_id))) {
    api_log(submission_directory($submission_id));
    die(submission_directory($submission_id));
}

// set correct content headers based on compression type
if (file_exists("entry.zip")) {
	header("Content-disposition: attachment; filename=entry.zip");
	header("Content-type: application/zip");
	$file = getcwd() . "/entry.zip";
} else if (file_exists("entry.tgz")) {
	header("Content-disposition: attachment; filename=entry.tgz");
	header("Content-type: application/x-gtar");
	$file = getcwd() . "/entry.tgz";
} else if (file_exists("entry.tar.gz")) {
	header("Content-disposition: attachment; filename=entry.tgz");
	header("Content-type: application/x-gtar");
	$file = getcwd() . "/entry.tar.gz";
} else if (file_exists("entry.tar.xz")) {
	header("Content-disposition: attachment; filename=entry.txz");
	header("Content-type: application/x-gtar");
	$file = getcwd() . "/entry.tar.xz";
} else if (file_exists("entry.txz")) {
	header("Content-disposition: attachment; filename=entry.txz");
	header("Content-type: application/x-gtar");
	$file = getcwd() . "/entry.txz";
} else if (file_exists("entry.tar.bz2")) {
	header("Content-disposition: attachment; filename=entry.tbz");
	header("Content-type: application/x-gtar");
	$file = getcwd() . "/entry.tar.bz2";
} else if (file_exists("entry.tbz")) {
	header("Content-disposition: attachment; filename=entry.tbz");
	header("Content-type: application/x-gtar");
	$file = getcwd() . "/entry.tbz";
} else {
	header("HTTP/1.0 404 Not Found");
	die();
}

ob_clean();
flush();
readfile($file);
exit;

?>

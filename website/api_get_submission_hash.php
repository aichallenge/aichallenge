<?php
require_once('api_functions.php');
require_once('server_info.php');

$submission_id = $_GET['submission_id'];
if(!filter_var($submission_id, FILTER_VALIDATE_INT) || $submission_id == '') {
  die();
}

/*
 * switch to the submission's directory making sure it's actually there
 */
if (!chdir($server_info["submissions_path"].$submission_id)) {
  die();
}

// set correct content headers based on compression type
if (file_exists("entry.zip")) {
	$file = getcwd() . "/entry.zip";
} else if (file_exists("entry.tgz")) {
	$file = getcwd() . "/entry.tgz";
} else if (file_exists("entry.tar.gz")) {
	$file = getcwd() . "/entry.tar.gz";
} else {
	header("HTTP/1.0 404 Not Found");
	die();
}

header("Content-type: application/json");
echo "{ 'hash': '";
passthru($server_info["manager_path"] . 'submission_hash.py ' . $file);
echo "' }\n";

?>

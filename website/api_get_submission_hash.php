<?php

require_once('api_functions.php');
require_once('submission.php');

header("Content-type: application/json");

$submission_id = $_GET['submission_id'];
if(!filter_var($submission_id, FILTER_VALIDATE_INT) || $submission_id == '') {
  die();
}

/*
 * switch to the submission's directory making sure it's actually there
 */
if (!chdir(submission_directory($submission_id))) {
  die();
}

// set correct content headers based on compression type
if (file_exists("entry.zip")) {
	$file = getcwd() . "/entry.zip";
} else if (file_exists("entry.tgz")) {
	$file = getcwd() . "/entry.tgz";
} else if (file_exists("entry.tar.gz")) {
	$file = getcwd() . "/entry.tar.gz";
} else if (file_exists("entry.tar.xz")) {
	$file = getcwd() . "/entry.tar.xz";
} else if (file_exists("entry.txz")) {
	$file = getcwd() . "/entry.txz";
} else if (file_exists("entry.tar.bz2")) {
	$file = getcwd() . "/entry.tar.bz2";
} else if (file_exists("entry.tbz")) {
	$file = getcwd() . "/entry.tbz";
} else {
	header("HTTP/1.0 404 Not Found");
	die();
}

header("Content-type: application/json");
echo "{ \"hash\": \"".md5_file($file)."\" }\n";

?>

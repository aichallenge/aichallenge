<?
require_once('api_functions.php');

$submission_directory = "/home/contest/ai-contest/planet_wars/submissions/";
$submission_id = $_GET['submission_id'];
if(!filter_var($submission_id, FILTER_VALIDATE_INT) || $submission_id == '') {
  die();
}

if (!file_exists($submission_directory.$submission_id)) {
  die();
}

passthru('/home/contest/ai-contest/planet_wars/backend/submission_hash.py '.
    $submission_id);

?>

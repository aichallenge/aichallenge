<?
require_once('api_functions.php');



/*
  Sends a bot's files to a game server. Tries to strip unneeded code.
 */


$submission_directory = "/home/contest/ai-contest/planet_wars/submissions/";
$submission_id = $_GET['submission_id'];
if(!filter_var($submission_id, FILTER_VALIDATE_INT) || $submission_id == '') {
  die();
}

header("Content-disposition: attachment; filename=$submission_id.tgz");
header("Content-type: application/x-compressed");
passthru('cd "'.$submission_directory.$submission_id.'"; tar -czf - . \
              --exclude=\'*.zip\' \
              --exclude=\'*.tgz\' \
              --exclude=PlayGame.jar \
              --exclude=ShowGame.jar \
              --exclude=.DS_Store \
              --exclude=Icon
');






?>
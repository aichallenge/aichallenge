<?php
// ini_set('error_reporting', E_ALL);
// ini_set('display_errors', true);

require_once('game_list.php');
require_once('session.php');
header("Content-type: application/json");

$user_id = get_type_or_else("user_id", FILTER_VALIDATE_INT);
$submission_id = get_type_or_else("submission_id", FILTER_VALIDATE_INT);
$map_id = get_type_or_else("map_id", FILTER_VALIDATE_INT);
$page = get_type_or_else("page", FILTER_VALIDATE_INT, 0);

echo get_game_list_json($page, $user_id, $submission_id, $map_id);

?>

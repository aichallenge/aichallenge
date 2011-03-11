<?php
// ini_set('error_reporting', E_ALL);
// ini_set('display_errors', true);

require_once('api_functions.php');

header("Content-type: application/json");

$json_string = file_get_contents('php://input');
$json_hash = md5($json_string);
$compiledata = json_decode($json_string);

if ($compiledata->status_id == 40) {
	if (mysql_query(sprintf($sql["update_submission_success"],
	                        $worker["worker_id"],
	                        $compiledata->submission_id))) {
		echo json_encode(array( "hash" => $json_hash ));
	} else {
		api_log(sprintf("Error updating successful compile: %s", mysql_error()));
	}
} else {
	if (mysql_query(sprintf($sql["update_compile_result"],
	                        $compiledata->status_id,
	                        $worker["worker_id"],
	                        $compiledata->submission_id))) {
		echo json_encode(array( "hash" => $json_hash ));
	} else {
		api_log(sprintf("Error updating errored compile: %s", mysql_error()));
	}
}

?>
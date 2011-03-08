<?php

require_once('api_functions.php');

$json_string = file_get_contents('php://input');
$json_hash = md5($json_string);
$compiledata = json_decode($json_string);

if ($compiledata->status_id == 40) {
	if (mysql_query(sprintf($sql["update_submission_success"],
	                        $worker["worker_id"],
	                        $compiledata->submission_id))) {
		echo json_decode(array( "hash" => $json_hash ));
	}
} else {
	if (mysql_query(sprintf($sql["update_submission_failure"],
	                        $compiledata->status_id,
	                        $worker["worker_id"],
	                        $compiledata->submission_id))) {
		echo json_decode(array( "hash" => $json_hash ));
	}
}

?>
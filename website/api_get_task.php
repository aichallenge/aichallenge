<?php
// ini_set('error_reporting', E_ALL);
// ini_set('display_errors', true);

require_once('api_functions.php');

header("Content-type: application/json");

// look for compile task first
$result = mysql_query($sql["select_next_compile"]);
while ($row = mysql_fetch_assoc($result)) {
	if (mysql_query(sprintf($sql["update_submission_compiling"],
	                        $worker["worker_id"],
	                        $row["submission_id"]))) {
		echo json_encode(array( "task" => "compile", "submission_id" => $row["submission_id"]));
		die();
	}
}

?>
<?php
// ini_set('error_reporting', E_ALL);
// ini_set('display_errors', true);

require_once('api_functions.php');

header("Content-type: application/json");

// look for compile task first
$result = mysql_query($sql["select_languages"]);
$languages = array();
while ($row = mysql_fetch_assoc($result)) {
	$languages[] = $row;
}

echo json_encode($languages);

?>
<?php

require_once('ranking.php');
header("Content-type: application/json");

$org_id = get_type_or_else("org_id", FILTER_VALIDATE_INT);
$country_id = get_type_or_else("country_id", FILTER_VALIDATE_INT);
$language_id = get_type_or_else("language_id", FILTER_VALIDATE_INT);
$page = get_type_or_else("page", FILTER_VALIDATE_INT, 0);

echo get_ranking_json($page, $org_id, $country_id, $language_id);

?>
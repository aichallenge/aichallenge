<?php
include 'header.php';
require_once('mysql_login.php');
require_once('ranking.php');
require_once('lookup.php');

$country_row = get_country_row(get_type_or_else('country'));

if (!$country_row) {
    echo "<p>Invalid Country</p>";
} else {
    $country_name = htmlentities($country_row['name']);
    echo "<h2><span>$country_name's User Rankings</span><div class=\"divider\" /></h2>";

    $page = get_type_or_else("page", FILTER_VALIDATE_INT, 1);
    echo get_country_ranking($country_row['country_id'], $page);
}

include 'footer.php';
?>

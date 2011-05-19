<?php
include 'header.php';
require_once('mysql_login.php');
require_once('ranking.php');

$lang_row = get_org_row(get_type_or_else('org'));

if (!$lang_row) {
    echo "<p>Invalid Organization</p>";
} else {
    $org_name = htmlentities($lang_row['name']);
    echo "<h2><span>$org_name's User Rankings</span><div class=\"divider\" /></h2>";

    $page = get_type_or_else("page", FILTER_VALIDATE_INT, 1);
    echo get_org_ranking($lang_row['org_id'], $page);
}

include 'footer.php';
?>

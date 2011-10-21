<?php
require_once('mysql_login.php');
require_once('ranking.php');
require_once('lookup.php');

$lang_row = get_language_row(get_type_or_else('language'));

if (!$lang_row) {
    $title="Invalid Language";
    include 'header.php';
    echo "<p>Invalid Language</p>";
} else {
    $language_name = htmlentities($lang_row['name'], ENT_COMPAT, 'UTF-8');
    $title=$language_name."'s User Rankings";
    include 'header.php';
    echo "<h2><span>$language_name's User Rankings</span><div class=\"divider\" /></h2>";

    $page = get_type_or_else("page", FILTER_VALIDATE_INT, 1);
    echo get_language_ranking($lang_row['language_id'], $page);
}

include 'footer.php';
?>

<?php

$title="All Games";
require_once("header.php");
require_once("game_list.php");

echo "<h2>All games by time played</h2>";

$page = get_type_or_else("page", FILTER_VALIDATE_INT, 1);
echo get_game_list_table($page);

require_once("footer.php");
?>

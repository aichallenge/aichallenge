<?php

require_once("visualizer_widget.php");
require_once("game_list.php");
require_once("lookup.php");

$map = str_replace("%2F", "/", filter_input(INPUT_GET, 'map', FILTER_SANITIZE_ENCODED));
$map_row = get_map_row($map);
$page = get_type_or_else("page", FILTER_VALIDATE_INT, 1);

if ($map_row !== NULL) {
    $title="Showing ".$map_row['filename'];
    require_once("header.php");
    echo "<a href=\"map/".$map_row['filename']."\">".$map_row['filename']."</a> (download link)";
    visualize_map($map_row['filename']);    
    echo get_game_list_table($page, NULL, NULL, $map_row['map_id'], FALSE, 'map.php');
} else {
    $map = htmlentities($map);
    require_once("header.php");
    echo "<p>Map $map does not exist</p>";
}


require_once("footer.php");

?>

<?php

include "header.php";

include "visualizer_widget.php";
$map = str_replace("%2F", "/", filter_input(INPUT_GET, 'map', FILTER_SANITIZE_ENCODED));
if ($map !== FALSE and $map !== NULL) {
    echo $map;
    visualize_map($map);    
} else {
    echo "<p>Incorrect Map $map</p>";
}

include "footer.php";

?>

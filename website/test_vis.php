<?php

$title="Visualizer Test";
require_once("header.php");
require_once("visualizer_widget.php");

?>
<pre id="map1">
# [ true, 100, 100, {} ]
rows 4
cols 4
players 2
attackradius2 5
spawnradius2 1
viewradius2 75
m ....
m .a..
m ..b.
m ....
</pre>
<pre id="map2">
# [false, 200, 200]
rows 4
cols 4
players 2
attackradius2 5
spawnradius2 1
viewradius2 75
m ....
m .a..
m ..b.
m ....
</pre>

<?php

visualize_pre();

require_once("footer.php");

?>

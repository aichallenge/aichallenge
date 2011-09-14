<?php

require_once("header.php");
require_once("visualizer_widget.php");

?>
<div id="map1"></div>
<div id="map2"></div>
<script src="visualizer/js/visualizer-min.js"></script>
<script>
    window.isFullscreenSupported = function () { return false; };
    var data = 'rows 4\ncols 4\nplayers 2\nm .a..\nm ..%.\nm ...b\nm %...\n';
    var visualizer1 = new Visualizer(document.getElementById('map1'), 'visualizer/', false, 100, 100);
    visualizer1.loadReplayData(data);

    var visualizer2 = new Visualizer(document.getElementById('map2'), 'visualizer/', false, 100, 100);
    visualizer2.loadReplayData(data);
</script>
<?php
require_once("footer.php");
?>

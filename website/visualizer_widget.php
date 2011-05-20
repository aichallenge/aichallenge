<?php
function visualizer_widget($game_id, $interactive=true, $width=690, $height=700) 
{
	$match = preg_match('/MSIE ([0-9]\.[0-9])/', $_SERVER['HTTP_USER_AGENT'], $reg);
	if ($match != 0 && floatval($reg[1]) < 9 || isset ($_GET["java"]) && $_GET["java"] == "true") {
		// we have IE < 9 or explicitly want to use Java
		if (file_exists(dirname(__FILE__)."/visualizer/java")) {
			$java = 'codebase="visualizer/java/"';
		} else {
			$java = 'archive="visualizer/visualizer.jar"';
		}
	}
    echo '<div id="visualizerDiv">';

    $replay = "game/" . $game_id;
    // Write applet tag if we use Java
    if (isset ($java)) {
        ?>
            <applet <?php echo $java; ?> code="com.aicontest.visualizer.VisualizerApplet" width="<?php echo $width; ?>" height="<?php echo $height; ?>">
            <param name="replay" value="<?php echo $replay; ?>">
            <param name="interactive" value="<?php echo $interactive; ?>">
        <?php
        if ($_GET["debug"] == "true") {
            ?>
                <param name="debug" value="true">
                <param name="separate_jvm" value="true">
                <param name="classloader_cache" value="false">
            <?php
        }
        ?>
            </applet>
        <?php
    }

    ?>
        </div>
    <?php

    // Write script tags if we use the canvas visualizer
    if (!isset ($java)) { 
        if (file_exists(dirname(__FILE__)."/visualizer/js/visualizer.js")) {
            $js = "visualizer/js/visualizer.js";
        } else {
            $js = "visualizer/js/visualizer-min.js";
        }
        ?>
            <script type="text/javascript" src="<?php echo $js; ?>"></script>
            <script type="text/javascript">
                visualizer = new Visualizer(document.getElementById('visualizerDiv'), 'visualizer/', <?php echo ($interactive) ? 'true' : 'false'; ?>, <?php echo $width; ?>, <?php echo $height; if (!$interactive) echo ', {fullscreen:false}'?>);
                visualizer.loadReplayDataFromURI('<?php echo $replay; ?>');
            </script>
        <?php
        }
    }
?>

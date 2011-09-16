<?php

function visualizer_activate() {
    $java = get_java_codebase();
    if (!isset ($java)) {
        if (file_exists(dirname(__FILE__)."/visualizer/js/visualizer.js")) {
            $js = "visualizer/js/visualizer.js";
        } else {
            $js = "visualizer/js/visualizer-min.js";
        }
?>
        <script type="text/javascript" src="<?php echo $js; ?>"></script>
<?php
    }
?>
<script>
// window.isFullscreenSupported = function () { return false; };
// function requires jQuery
var visualize = function (i) {
    var data = $(this).text();
    var options = data.split('\n')[0];
    if (options[0] === '#') {
        try {
            options = JSON.parse(options.slice(1));
        } catch (err) {
            options = [];
        }
    }
    var interactive = options[0] || false;
    var width = options[1] || 100;
    var height = options[2] || 100;
    var config = options[3] || {};
    if (typeof java_codebase !== 'undefined') {
        this.innerHTML = '';
        var applet = document.createElement('applet');
        var idx = java_codebase.indexOf('=');
        var codeBaseAttribute = java_codebase.substring(0, idx); 
        var codeBaseValue = java_codebase.substring(idx + 2, java_codebase.length - 1); 
        applet.setAttribute(codeBaseAttribute, codeBaseValue);
        applet.setAttribute('code', 'com.aicontest.visualizer.VisualizerApplet');
        applet.setAttribute('width', width);
        applet.setAttribute('height', height);
        var addParam = function(name, value) {
            var param = document.createElement('param');
            param.setAttribute('name', name);
            param.setAttribute('value', value);
            applet.appendChild(param);
        };
        data = data.replace(/\n/g, '\\n');
        addParam('replay_string', data);
        addParam('interactive', interactive);
        if (typeof java_debug !== 'undefined' && java_debug) {
            addParam('debug', 'true');
            addParam('separate_jvm', 'true');
            addParam('classloader_cache', 'false');
        }
        this.appendChild(applet);
    } else if (typeof Visualizer !== 'undefined') {
        var vis = new Visualizer(this, 'visualizer/', interactive, width, height, config);
        vis.loadReplayData(data);
    }
};
</script>
<?php
}

function get_java_codebase() {
	$match = preg_match('/MSIE ([0-9]\.[0-9])/', $_SERVER['HTTP_USER_AGENT'], $reg);
	if ($match != 0 && floatval($reg[1]) < 9 || isset ($_GET["java"]) && $_GET["java"] == "true") {
		// we have IE < 9 or explicitly want to use Java
		if (file_exists(dirname(__FILE__)."/visualizer/java")) {
			return 'codebase="visualizer/java/"';
		} else {
			return 'archive="visualizer/visualizer.jar"';
		}
	}
}

function visualize_pre() {
    visualizer_activate();
    $java = get_java_codebase();
    if (isset ($java)) {
?>
        <script>
<?php
        echo "java_codebase = '$java';";
        if (isset ($_GET['debug']) && $_GET['debug'] == "true") {
            echo "java_debug = 'true';";
        }
?>
        </script>
<?php
    }
?>
    <script>
    $(function () {
        $('pre').each(visualize);
    });
    </script>
<?php
}

function visualize_game($game_id, $interactive=true, $width=690, $height=700) {
    visualizer_widget("game/" . $game_id, $interactive, $width, $height);
}

function visualize_map($map, $width=690, $height=700) {
    visualizer_widget("map/" . $map, true, $width, $height);
}

function visualizer_widget($replay, $interactive=true, $width=690, $height=700) {
    echo '<div id="visualizerDiv">';

    // Write applet tag if we use Java
    $java = get_java_codebase();
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
        visualizer_activate();
        ?>
            <script type="text/javascript">
                visualizer = new Visualizer(document.getElementById('visualizerDiv'), 'visualizer/', <?php echo ($interactive) ? 'true' : 'false'; ?>, <?php echo $width; ?>, <?php echo $height; if (!$interactive) echo ', {fullscreen:false}'?>);
                visualizer.loadReplayDataFromURI('<?php echo $replay; ?>');
            </script>
        <?php
    }
}

?>

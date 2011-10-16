<?php

function visualizer_activate($extended) {
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
    if ($extended) {
?>
<script>
// function requires jQuery
var visualize = function (i) {
	var pre = $(this);
    var data = pre.text();
    var setup = data.split('\n')[0];
    if (setup[0] === '#') {
        try {
            // option is a list of 4 or 5 values, [ options, width, height, config, uri ]
            // if the uri is not there, the remaining data will be considered a map
            // otherwise the uri is loaded
            setup = $.parseJSON(setup.slice(1));
        } catch (err) {
            setup = [];
        }
    	// remove styles
    	pre.css('border','none').css('background','none');
        var width = setup[1] || 100;
        var height = setup[2] || 100;
        var config = setup[3] || {};
        var uri = null;
        if (setup.length === 5 && typeof setup[4] === 'string') {
            uri = setup[4];
        }
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
        	for (var key in setup[0]) {
	            addParam(key, setup[0][key]);
        	}
            if (uri) {
                addParam('replay', uri);
            } else {
	            data = data.replace(/\n/g, '\\n');
	            addParam('replay_string', data);
            }
            if (typeof java_debug !== 'undefined' && java_debug) {
                addParam('debug', 'true');
                addParam('separate_jvm', 'true');
                addParam('classloader_cache', 'false');
            }
            addParam('configOverrides', JSON.stringify(config));
            this.appendChild(applet);
        } else if (typeof Visualizer !== 'undefined') {
            // initialize options with defaults and override them
            var options = new Options();
            options.data_dir = 'visualizer/';
            if (setup[0]) {
            	for (key in setup[0]) {
            		if (!options.hasOwnProperty(key) || (typeof options[key] === 'function')) {
            			throw new Error("Cannot override '" + key + "', because it is not a known option.");
            		}
            		options[key] = setup[0][key];
            	}
            }
            var vis = new Visualizer(this, options, width, height, config);
            if (uri) {
            	vis.loadReplayDataFromURI(uri);
            } else {
            	vis.loadReplayData(data);
            }
        }
    }
};
</script>
<?php
    }
}

function get_java_codebase() {
    if (isset($_SERVER['HTTP_USER_AGENT'])) {
    	$match = preg_match('/MSIE ([0-9]\.[0-9])/', $_SERVER['HTTP_USER_AGENT'], $reg);
    } else {
        $match = 0;
    }
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
    visualizer_activate(true);
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

function visualize_game($game_id, $interactive=true, $width=710, $height=700) {
    visualizer_widget("game/" . $game_id, $interactive, $width, $height);
}

function visualize_map($map, $width=710, $height=700) {
    visualizer_widget("map/" . $map, true, $width, $height);
}

function visualizer_widget($replay, $interactive=true, $width=690, $height=700) {
    ?>
        <div id="visualizerDiv">
    <?php

    // Write applet tag if we use Java
    $java = get_java_codebase();
    if (isset ($java)) {
        ?>
            <applet <?php echo $java; ?> code="com.aicontest.visualizer.VisualizerApplet" width="<?php echo $width; ?>" height="<?php echo $height; ?>">
            <param name="replay" value="<?php echo $replay; ?>">
            <param name="interactive" value="<?php echo $interactive; ?>">
        <?php
        if (isset($_GET["DEBUG"]) && $_GET["debug"] == "true") {
            ?>
                <param name="debug" value="true">
                <param name="separate_jvm" value="true">
                <param name="classloader_cache" value="false">
            <?php
        }
        if (!$interactive) {
            ?>
                <param name="configOverrides" value="{&quot;zoom&quot;:1}">
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
        visualizer_activate(false);
        ?>
            <script type="text/javascript">
                var options = new Options();
                options.data_dir = 'visualizer/';
                options.interactive = <?php echo ($interactive) ? 'true' : 'false'; ?>;
                options.embedded = <?php echo ($interactive) ? 'false' : 'true'; ?>;
                var config = {};
                if (!options.interactive) {
                    // typically we want no zoom in non-interactive visualizers
                    config.zoom = 1;
                }
                visualizer = new Visualizer(document.getElementById('visualizerDiv'), options, <?php echo $width; ?>, <?php echo $height; ?>, config);
                visualizer.loadReplayDataFromURI('<?php echo $replay; ?>');
            </script>
        <?php
    }
}

?>

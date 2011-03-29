<?php
include "header.php";

$match = preg_match('/MSIE ([0-9]\.[0-9])/', $_SERVER['HTTP_USER_AGENT'], $reg);
if ($match != 0 && floatval($reg[1]) < 9 || isset ($_GET["java"]) && $_GET["java"] == true) {
	// we have IE < 9 or explicitly want to use Java
	if (file_exists(dirname(__FILE__)."/visualizer/java")) {
		$java = 'codebase="visualizer/java/"';
	} else {
		$java = 'archive="visualizer/visualizer.jar"';
	}
}
?>
    <div id="visualizerDiv">
<?php
$replay = "games/" . (int)($_GET["game_id"] / 10000) . "/" . $_GET["game_id"] . ".replay";
// Write applet tag if we use Java
if (isset ($java)) {
?>
      <applet <?php echo $java; ?> code="com.aicontest.visualizer.VisualizerApplet" width="655" height="655">
        <param name="replay" value="<?php echo $replay; ?>">
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
include "footer.php";

// Write script tags if we use the canvas visualizer
if (!isset ($java)) { 
	if (file_exists(dirname(__FILE__)."/visualizer/js/visualizer.js")) {
		$js = "visualizer/js/visualizer.js";
	} else {
		$js = "visualizer/js/visualizer-min.js";
	} ?>
<script type="text/javascript" src="<?php echo $js; ?>"></script>
<script type="text/javascript">
	visualizer = new Visualizer(document.getElementById('visualizerDiv'), 'visualizer/', 655, undefined);
	visualizer.loadReplayDataFromURI('<?php echo $replay; ?>');
</script>
<?php
} ?>

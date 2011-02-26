<?php include "header.php"; ?>

<div id="visualizer" />

<?php include "footer.php"; ?>

<script type="text/javascript" src="visualizer/js/visualizer.js"></script>
<script type="text/javascript">
<?php
ob_start();
include('game_info.php');
$data = ob_get_clean();
$cutA = substr($data, 0, 16) == "playback_string=";
$cutB = substr($data, strlen($data) - 21, 21) == "\nusing_compression=1\n";
$start = $cutA ? 16 : 0;
$length = strlen($data) - $start - ($cutB ? 21 : 0);
$data = substr($data, $start, $length);
$data = htmlspecialchars($data);			// encode '<', '>' and '&'
$data = str_replace("\\", "\\\\", $data);	// escape eixsting back slashes
$data = str_replace("\n", "\\n", $data);	// escape line breaks (must be Unix!)
?>
visualizer.loadReplay("<?php echo $data ?>", true);
visualizer.attach('visualizer', 640);
</script>

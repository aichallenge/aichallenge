<?php include "header.php"; ?>

<div id="visualizerDiv"></div>

<?php include "footer.php"; ?>

<script type="text/javascript" src="visualizer/js/visualizer.js"></script>
<script type="text/javascript">
<?php
$replay = "games/" . (int)($_GET["game_id"] / 10000) . "/" . $_GET["game_id"] . ".replay";
?>
visualizer = new Visualizer(document.getElementById('visualizerDiv'), 'visualizer/', 'visualizer/java/', 640, undefined);
visualizer.loadReplayDataFromURI('<?php echo $replay; ?>');
</script>

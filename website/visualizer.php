<?php include "header.php"; ?>
<?php
    include "visualizer_widget.php";
    $game_id = filter_input(INPUT_GET, 'game', FILTER_VALIDATE_INT);
    if ($game_id) {
        visualizer_widget($game_id);
    } else {
        echo '<p>Incorrect Game Number</p>';
    }
?>
<?php include "footer.php"; ?>

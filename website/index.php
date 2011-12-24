<?php

$title = "Home";
include('header.php');
require_once('memcache.php');

if(file_exists('server_message.html')) {
    //Used to convey a message on the front page
    include('server_message.html');
}

?>

<!--<MarkdownReplacement with="competition.md">--><!--</MarkdownReplacement>-->

<p>Computer Programs Duking it Out with Ants:</p>
<?php
    $last_game_id = 0;
    if ($memcache)
        $last_game_id = $memcache->get('l:splash');
    if (!$last_game_id) {
        $last_game_id = 0;
    }
    include 'visualizer_widget.php';
    visualize_game($game_id=strval($last_game_id),false,700,700);
?>

<p>Browse other <a href="games.php">recent games here</a>.</p>

<?php include 'footer.php'; ?>

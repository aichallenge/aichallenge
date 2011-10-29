<?php

$title = "Home";
include('header.php');
require_once('memcache.php');

if(file_exists('server_message.html')) {
    //Used to convey a message on the front page
    include('server_message.html');
}

?>

<!--<MarkdownReplacement with="competition.md">--><h1>Introducing Ants</h1>

<p>The Google AI Challenge is all about creating artificial intelligence, whether you are a beginning programmer or an expert. Using one of the easy-to-use starter kits, you will create a computer program (in any language) that controls a colony of ants which fight against other colonies for domination.</p>

<p>It only takes 5 minutes to submit one of the starter kits to the website and watch your ant colony fight for domination against colonies created by other people from around the world. From there check out the tutorials on how to locally run your bot and begin programming!</p>
<!--</MarkdownReplacement>-->

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

<h2>Contest Schedule</h2>

<p>The current phase of the contest will end December 18th at 11:59pm EST. At that time submissions will be closed. Shortly thereafter the final tournament will be started. The length of the final tournament has not yet been determined but is expected to last less than one week. Upon completion the contest winner will be announced and all results will be publically available.</p>

<?php include 'footer.php'; ?>

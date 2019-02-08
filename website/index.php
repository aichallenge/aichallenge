<?php

$title = "Home";
include('header.php');
require_once('memcache.php');

if(file_exists('server_message.html')) {
    //Used to convey a message on the front page
    include('server_message.html');
}

?>

<!--<MarkdownReplacement with="competition.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h1 id="final-rankings">Final Rankings</h1>
<p>Congratulations the ants contest winner, Mathis Lichtenberger. He is a second year computer science student at the University of Lubeck. His winning bot (<a href="http://xathis.com/posts/ai-challenge-2011-ants.html">post mortem</a>) is called <a href="/profile.php?user=4513">xathis</a>.</p>
<p>A close second place is  Evgeniy Voronyuk. He graduated from the Dnipropetrovsk National University. He works at ISD (a satellite of SCC Soft Computer). His entry is called <a href="/profile.php?user=398">GreenTea</a>.</p>
<p>Third place is taken by <a href="/profile.php?user=5916">protocolocon</a>, a bot from Spain.</p>
<p>Big congratulations are also in order for the top 100 contestants. Through hard work and determination they overcame the other 7900 submissions.</p>
<p>On behalf of the contest organizers I would like to thank everyone taking part in this term's competition.</p>
<p>Most of the <a href="https://github.com/aichallenge/aichallenge">code for this contest</a> was done by generous volunteers. A lot of people took part in helping. Thank you to all developers.</p>
<p>A lot of the bot authors have written <a href="http://forums.aichallenge.org/viewforum.php?f=24">post-mortems</a> and they have released their <a href="http://forums.aichallenge.org/viewtopic.php?f=24&amp;t=2161">source code</a> on the forums.</p>
<p>The rankings are available <a href="/rankings.php">here</a> showing the full results of the finals.</p>
<h1 id="introducing-ants">Introducing Ants</h1>
<p>The AI Challenge is all about creating artificial intelligence, whether you are a beginning programmer or an expert. Using one of the easy-to-use starter kits, you will create a computer program (in any language) that controls a colony of ants which fight against other colonies for domination.</p>
<p>It only takes <code>5 minutes</code> to submit one of the starter kits to the website and watch your ant colony fight for domination against colonies created by other people from around the world. From there check out the tutorials on how to locally run your bot and begin programming!</p>
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

<?php include 'footer.php'; ?>

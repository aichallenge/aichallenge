<?php
include('header.php');
require_once('memcache.php');

if(file_exists('server_message.html')) {
    //Used to convey a message on the front page
    include('server_message.html');
}

?>

<!--<MarkdownReplacement with="competition.md">--><h1>Missing wiki content.</h1><p>Please run `./setup.py` in the website folder.</p><!--</MarkdownReplacement>-->

<p>Computer Programs Duking it Out with Ants:</p>
<?php
    $last_game_id = 0;
    if ($memcache)
        $last_game_id = $memcache->get('last_game_id');
    if (!$last_game_id) {
        $last_game_id = 0;
    }
    include 'visualizer_widget.php';
    visualizer_widget($game_id=strval($last_game_id),false,550,550);
?>

<h2>Planet Wars Final Rankings</h2>
<p>Congratulations to this term's winner, <a href="http://quotenil.com/">
  Gábor Melis</a>! Whereas the Google AI Challenge has traditionally been
  dominated by entries written in languages like
  <a href="http://ai-contest.com/language_profile.php?language=C%2B%2B">C++</a>,
  Gábor has solidly taken the top spot with an innovative algorithm. His
  entry is one of just 33 submissions written in
  <a href="http://ai-contest.com/language_profile.php?language=Lisp">Lisp</a>,
  and is entitled <a href="http://ai-contest.com/profile.php?user=8565">
  bocsimacko</a>. Congratulations Gábor!</p>
<p>A big congratulations also goes out to the runners up,
  <a href="http://ai-contest.com/profile.php?user=7026">_iouri_</a> and
  <a href="http://ai-contest.com/profile.php?user=11248">Slin-.-</a>,
  as well as all the finalists in the top 100. With over 4600 submissions
  received, securing a spot in the top 100 was no easy task!</p>
<p>On behalf of <a href="http://www.google.com">Google</a> and the
  <a href="http://csclub.uwaterloo.ca">University of Waterloo Computer
  Science Club</a>, we would like to thank everybody who took part in the
  Google AI Challenge. This term was the best yet, with more than 4600 working
  entries submitted by people from 112 different countries. Thank you!</p>
<p>We would also like to thank all the <a href="thanks.php">people</a>
  who generously volunteered their time and mad skills to make the Google
  AI Challenge happen.</p>
<p>Final rankings can be found <a href="rankings.php">here</a>. These
  rankings are based on the
  <a href="http://en.wikipedia.org/wiki/Elo_rating_system">Elo rating
  system</a>. Click on any player's name to see their game history. If
  you crave more interesting statistics and rankings, check out the
  <a href="country_rankings.php">country rankings</a> and
  <a href="organization_rankings.php">organization rankings</a>. Also check
  out <a href="http://ai-contest.com/forum/viewtopic.php?f=19&t=1184">
  this cool forum thread</a> for tons of the most interesting game replays!</p>

<?php include 'footer.php'; ?>

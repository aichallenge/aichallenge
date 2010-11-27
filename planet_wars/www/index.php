<?php include 'header.php';

$deadline = new DateTime("2010-11-27 23:59");
$deadline = floatval($deadline->format("U"));
$now = new DateTime();
$now = floatval($now->format("U"));
$diff = max(($deadline - $now) / 60, 0);
if ($diff > 60) {
    $timeleft = number_format($diff/60) ." hours ".
        number_format($diff%60) ." minutes";
} else {
    $timeleft = number_format($diff) ." minutes";
}

?>
<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"></script>
<script type="text/javascript" src="js/jquery.countdown.min.js"></script>
<script>
function serverTime() { 
    var time = null; 
    $.ajax({url: 'server_time.php', 
        async: false, dataType: 'text', 
        success: function(text) { 
            time = new Date(text); 
        }, error: function(http, message, exc) { 
            time = new Date(); 
    }}); 
    return time; 
}

$(function() {
$('#countdown_timer').countdown({until: new Date("2010-11-27T23:59"),
    serverSync: serverTime, timezone: -6, format: "HMS"});
});
</script>

<div style="text-align: center; padding-bottom: 2em">
  <h2>Submissions closing in approximately</h2>
  <div id="countdown_timer" style="width: 150px; height: 45px; margin: auto">
    <h3><?=$timeleft?></h3></div>
</div>

<h2>Google AI Challenge!</h2>
<p>That&#39;s right, the Google AI Challenge is back! Are you excited to get
  started? So are we! Here is the timeline:</p>
<ul>
  <li><strong>September 1, 2010:</strong> programming materials released to the
    public.</li>
  <li><strong>Friday Sept 10, 2010:</strong> official start date. You will be
    able to create an account and see your ranking.</li>
  <li><strong>Saturday Nov 27, 2010:</strong> submission deadline. No
    submissions will be accepted after 11:59 PM CST (i.e. server time as used in profile game listings). Don't wait till the last second to send in your final submission though, leaving yourself a few hours buffer to make sure it submits correctly is best.</li>
  <li><strong>Wednesday Dec 1, 2010:</strong> official results and final
    rankings will be released. Who will be the winner? Will it be <strong>
    you</strong>?!</li>
</ul>

<p>Ready to get started? Download a <a href="starter_packages.php">starter pack</a>, and then follow the <a href="quickstart.php">five minute tutorial</a>.</p>

<p>Got questions? Ask them in our <a href="forum/">forum</a> or join us on <a href="http://webchat.freenode.net/?channels=aichallenge&uio=d4">#aichallenge</a> on irc.freenode.net</p>


<p></p>
<center><object width="500" height="405"><param name="movie" value="http://www.youtube.com/v/O0uxXZY-t-s&amp;hl=en_US&amp;fs=1?border=1"></param><param name="allowFullScreen" value="true"></param><param name="allowscriptaccess" value="always"></param><embed src="http://www.youtube.com/v/O0uxXZY-t-s&amp;hl=en_US&amp;fs=1?border=1" type="application/x-shockwave-flash" allowscriptaccess="always" allowfullscreen="true" width="500" height="405"></embed></object></center>
<p></p>
<h2>Planet Wars is Based on Galcon</h2>
<p>Planet Wars is inspired by Galcon, a popular iPhone and desktop strategy
  game. A huge thank-you goes out to the creator of Galcon,
  <a href="http://www.philhassey.com">Phil Hassey</a>, for
  letting us use his idea. We wanted to show Phil some love for being so
  awesome by saying a few words about Galcon here.</p>
<p>You can play Galcon online against other people for free
  <a href="http://www.galcon.com/flash/">here</a>. The Galcon homepage is
  <a href="http://www.galcon.com/">here</a>. You can play Galcon on your
  <a href="http://www.galcon.com/fusion/">desktop</a>,
  <a href="http://www.galcon.com/iphone/">phone</a>, or
  <a href="http://www.galcon.com/flash/">on the web</a>. If you love Galcon
  as much as we do, then consider buying it!</p>
<center><a href="http://www.galcon.com">
  <img src="galcon_fusion_logo.png">
</a></center>
<?php include 'footer.php'; ?>

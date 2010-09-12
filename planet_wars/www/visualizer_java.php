<?php include "header.php"; ?>
<h2>Planet Wars Game Viewer</h2>
<center>
  <APPLET code="Viewer.class"
          width="500"
          height="500"
          archive="Visualizer.jar">
    <param name="game_id" value="<?php echo htmlspecialchars($_GET["game_id"]); ?>"/>
    Java applet support required.
  </APPLET> 
</center>
<p>&nbsp;</p>
<p>The visualizer can take up to a minute to load. If it does not appear
  after one minute, then you probably do not have Java applets enabled in
  your browser. Enable Java applets, then restart your browser before trying
  again.</p>
<h3>Frequently Asked Questions</h3>
<p><strong>Why do some games end suddenly?</strong> In order to allow as many
  games as possible, games that go longer than 200 turns are ended. The game
  is won by whichever player has the most ships on the map at the end of
  turn 200. In the rare case when the bots have the same number of ships, the
  game is a draw. The ship count includes ships that are on planets as well as
  ships that are in flight. We hope that this restriction will not impact
  the rankings, since most games are basically settled fairly early on.</p>
<p><strong>Why does this playback have less than two players?</strong> Usually
  this happens when one player or the other crashes at the beginning of the
  game. This results in a game of length zero that contains only one player.
  If a bot crashes, it loses the game and a win is awarded to the opponent.</p>
<?php include "footer.php"; ?>

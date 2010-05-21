<?php
include 'header.php';
include_once 'rankings_widget.php';

echo <<<EOT
<h2>Final Rankings</h2>
<p>Congratulations to this year's winner, Andy Sloane! Andy is
  originally from Milwaukee, Wisconsin, and currently works as a software
  engineer at Yahoo!. His winning entry is titled a1k0n_. Congratulations!</p>
<p>Congratulations also goes out to this year's finalists (top 100). The
  finalists this year come from all around the world, with 27 different
  countries represented. You should all be very proud!</p>
<p>Here are the complete final rankings. These rankings are based on the
  <a href="http://en.wikipedia.org/wiki/Elo_rating_system">Elo rating system
  </a>. Click on any player's name to see their game history.</p>
<p>For technical information about the exact procedure that was used to
  calculate these rankings, <a href="final_tournament_faq.php">click here</a>.
  </p>
EOT;

echo getRankingsTableString(1, false, 100,"");

include 'footer.php';
?>

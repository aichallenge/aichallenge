<?php include 'header.php'; ?>

<h2>Problem Description</h2>
<p>The objective is to create a computer program that plays the game of Planet
  Wars as intelligently as possible. It's recommended that you use one of the
  <a href="starter_packages.php">starter packages</a> as a starting point. If
  you're looking to get up and running as quickly as possible, check out the
  <a href="quickstart.php">Five Minute Quickstart Guide</a>.</p>
<p>Planet Wars is a strategy game set in outer space. The objective is to
  take over all the planets on the map, or altenatively eliminate all of
  your opponents' ships.</p>
<center><object width="500" height="405"><param name="movie" value="http://www.youtube.com/v/O0uxXZY-t-s&amp;hl=en_US&amp;fs=1?border=1"></param><param name="allowFullScreen" value="true"></param><param name="allowscriptaccess" value="always"></param><embed src="http://www.youtube.com/v/O0uxXZY-t-s&amp;hl=en_US&amp;fs=1?border=1" type="application/x-shockwave-flash" allowscriptaccess="always" allowfullscreen="true" width="500" height="405"></embed></object></center>
<p></p>
<p>The game is turn-based. Your bot is a function that takes a list of planets
  and a list of fleets, and outputs some orders. Each planet has the following
  fields/properties:</p>
<ul>
  <li>X-coordinate</li>
  <li>Y-coordinate</li>
  <li>Owner's PlayerID</li>
  <li>Number of Ships</li>
  <li>Growth Rate</li>
</ul>
<p>Neutral planets have a PlayerID of zero, planets owned by your bot have a
  PlayerID of 1, and planets owned by the enemy have a PlayerID of 2. The
  number of ships and the growth rate are both whole numbers. Each turn, the
  number of ships on non-neutral planets increases according to the growth
  rate for that planet.</p>
<p>Fleets are the colored numbers that fly between planets. When a fleet
  arrives at its destination planet, one of two things can happen. If the
  destination planet belongs to your bot, the ships from the fleet land on
  the planet as reinforcements. If your bot does not own the destination
  planet, the ships from the fleet are subtracted from the ships that
  currently occupy the planet. If the result is less than zero, then your
  bot gains control of the planet. If the result is exactly zero, then
  control of the planet does not change hands. A fleet has the following
  properties:</p>
<ul>
  <li>Owner's PlayerID</li>
  <li>Number of Ships</li>
  <li>Source PlanetID</li>
  <li>Destination PlanetID</li>
  <li>Total Trip Length</li>
  <li>Number of turns remaining until arrival</li>
</ul>
<p>Your bot can issue as many orders as it wants during one turn. Each order
  specifies a source planet, a destination planet, and a number of ships.
  Once the order is executed, the given number of ships leave the source
  planet, to go towards the destination planet. Your bot issues orders using
  the IssueOrder(src, dest, num_ships) function. See the starter packages
  for an example that you can use as a starting point.</p>
<p>The game ends when only one player remains, or if the game goes past a
  certain number of turns.</p>

<?php include 'footer.php'; ?>

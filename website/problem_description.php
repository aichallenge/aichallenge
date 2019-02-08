<?php

$title="Problem Description";
require_once('header.php');
require_once('visualizer_widget.php');
visualize_pre();

?>

<!--<MarkdownReplacement with="competition-Problem-Description.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h1 id="ants-problem-description">Ants Problem Description</h1>
<p>Ants is a multi-player strategy game set on a plot of dirt with water for obstacles and food that randomly drops. Each player has one or more hills where ants will spawn.  The objective is for players to seek and destroy the most enemy ant hills while defending their own hills.  Players must also gather food to spawn more ants, however, if all of a player's hills are destroyed they can't spawn any more ants.</p>
<div class="codehilite"><pre><span></span># [ { &quot;embedded&quot;: true, &quot;decorated&quot;: false, &quot;loop&quot;: true }, 570, 170, { &quot;speedFactor&quot;: 0, &quot;speedFastest&quot;: 2, &quot;speedSlowest&quot;: 2, &quot;zoom&quot;: 1 }, &quot;example_games/1.replay&quot; ]
</pre></div>


<p>The objective is to create a computer program (a bot) that plays the game of Ants as intelligently as possible. It is recommended that you use one of the starter packages as a starting point. If you are looking to get up and running as quickly as possible, check out the Five Minute Quickstart Guide. For more details about Ants beyond this introductory document, see the Game Specification.</p>
<hr />
<p><img src="http://aichallengebeta.hypertriangle.com/images/wrap.png" align="left" alt="the map wraps around like a torus" />
The game is turn-based. The map is a grid of squares that wraps around at the edges (a torus).  This means if an ant walks up across the top of the map they appear at the bottom, or walking to the right they appear at the left.  A bot is a program that reads input about the squares it can currently see and outputs orders to move its ants around the map.</p>
<hr />
<p><img src="http://aichallengebeta.hypertriangle.com/images/direction.png" align="right" alt="ants move in 4 directions" />
Each ant can only see the area around it, so bots will not start with a full view of the map.  Each turn the bot will be given the following information for all squares that are visible to its ants:</p>
<ul>
<li>a list of <strong>water</strong> squares, that have not been seen before</li>
<li>a list of <strong>ants</strong>, including the owner</li>
<li>a list of <strong>food</strong></li>
<li>a list of <strong>hills</strong>, including the owner</li>
<li>a list of <strong>dead</strong> ants (from the last attack phase), including the owner</li>
</ul>
<p>A bot can issue up to one order for each ant during a turn. Each order specifies an ant by location and the direction to move it: North, South, East or West. Once the order is executed, ants move one square in the given direction.</p>
<hr />
<p>The game then goes through 5 phases:</p>
<ul>
<li><strong>move</strong> all ants (ants that collide in the same square are killed)</li>
<li><strong>attack</strong> enemy ants if within range</li>
<li><strong>raze</strong> ant hills with enemy ants positioned on them</li>
<li><strong>spawn</strong> more ants at hills that are not razed or blocked</li>
<li><strong>gather</strong> food next to ants (food disappears if 2 enemies are both next to it)</li>
</ul>
<p>After the phases, the bot will receive the next game state and issue more moves.</p>
<hr />
<p>Sometimes bots crash or timeout (don't let your bot do this!) and are removed from the game.  The ants will just stay where they are and must still be attacked and killed to get their territory.  It's good to control most of the map, because then the bot will be able to gather more food to create more ants giving it a better chance to raze enemy ant hills while defending its own, which is how to get the highest score and win!</p>
<p>The game ends when only one active player has ants, or only one player's ant hills remain, or if the game goes past a certain number of turns.  (The game can also be cut short if no progress is being made by any bot, or if the rank of bots will not change.  The game will continue if the only bot with a hill isn't winning yet.  See our cutoff rules.)</p>
<hr />
<p>Want to know how to program a bot?  First read the detailed Game Specification, and then check out the Five Minute Quickstart Guide!</p>
<!--</MarkdownReplacement>-->

<?php

require_once('footer.php');

?>

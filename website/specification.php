<?php

require_once('header.php');

?>

<h1>Ants Game Specification</h1>

<p>Contents:</p>

<ol>
<li><a href="#Turns-and-Phases">Turns and Phases</a></li>
<li><a href="#Scoring">Scoring</a></li>
<li><a href="#Cutoff-Rules">Cutoff Rules</a></li>
<li><a href="#Food-Harvesting">Food Harvesting</a></li>
<li><a href="#Ant-Spawning">Ant Spawning</a></li>
<li><a href="#Food-Spawning">Food Spawning</a></li>
<li><a href="#Battle-Resolution">Battle Resolution</a></li>
<ol>
<li><a href="specification_battle.php">Focus Battle Resolution</a></li>
</ol>
<li><a href="#Hill-Razing">Battle Resolution</a></li>
<li><a href="#Bot-Input">Bot Input</a></li>
<li><a href="#Bot-Output">Bot Output</a></li>
<li><a href="#Map-Format">Map Format</a></li>
<li><a href="#Replay-Format">Replay Format</a></li>
</ol>

<div id="Turns-and-Phases">
<!--<MarkdownReplacement with="competition-Turns-and-Phases.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="turns-and-phases">Turns and Phases</h2>
<h3 id="setup">Setup</h3>
<p>Each bot is sent some starting information for the game, including map size, max turns and turn timings.  After the bot has processed this data, it should return a 'go'.  After all bots are ready, the game will start.</p>
<h3 id="turns">Turns</h3>
<p>Once each of the bots has indicated it has finished setting up, the game engine performs the following steps repeatedly:</p>
<ol>
<li>Send the game state to the players</li>
<li>Receive orders from the players</li>
<li>Perform the phases and update the game state</li>
<li>Check for endgame conditions</li>
</ol>
<p>There is a specified maximum turn limit for each map.  This will be adjusted continuously during the contest.</p>
<p>A <em>turn</em> is defined as the above steps. They are performed up to the maximum number of turns times and then the game stops. </p>
<h3 id="phases">Phases</h3>
<p>After receiving complete orders from the players, the engine then updates the game state, advancing the game to the next turn. This happens in 6 phases:</p>
<ul>
<li>move (execute orders)</li>
<li>attack</li>
<li>raze hills</li>
<li>spawn ants</li>
<li>gather food</li>
<li>spawn food</li>
</ul>
<h3 id="endbot-conditions">Endbot Conditions</h3>
<p>Any of the following conditions will cause a player to finish participating in a game:</p>
<ul>
<li>The player has no live ants left remaining on the map.</li>
<li>The bot crashed.</li>
<li>The bot exceeded the time limit without completing its orders.</li>
<li>A bot attempts to do something that the tournament manager deems a security issue and is disqualified.</li>
</ul>
<p>If a bot stops participating due to a crash or timeout, their ants remain on the board and can still collide and battle with other ants. Their ants just do not make any future moves and opponents are not explicitly told those ants' owners are no longer participating.</p>
<p>If a bot crashes or times out on a given turn then none of the moves received from that bot will be executed for that turn. </p>
<h3 id="ranking">Ranking</h3>
<p>At the end of the game, each player is ranked based off the final scores, with a tie resulting in each player having the same ranking. The difference in scores is not reflected in the general ranking of bots, only the relative positions for each game.</p>
<!--</MarkdownReplacement>-->
</div>

<div id="Scoring">
<!--<MarkdownReplacement with="competition-Scoring.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="scoring">Scoring</h2>
<p>The objective of the game is to get the highest score.  Points are awarded by attacking and defending hills.</p>
<ul>
<li>Each bot starts with 1 point per hill</li>
<li>Razing an enemy hill is 2 points</li>
<li>Losing a hill is -1 points</li>
</ul>
<p>This means if you don't attack and lose all your hills, you will end up with 0 points.</p>
<p>If the game ends with only 1 bot remaining, any enemy hills not razed will be awarded to the remaining bot.  This is done so that if another bot crashes or times out the remaining bot isn't denied the points for attacking.  These are called bonus points.</p>
<ul>
<li>2 bonus points per remaining hill to the remaining bot</li>
<li>-1 point per remaining hill to the owner</li>
</ul>
<!--</MarkdownReplacement>-->
</div>

<div id="Cutoff-Rules">
<!--<MarkdownReplacement with="competition-Cutoff-Rules.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="cutoff-rules">Cutoff Rules</h2>
<p>To ensure meaningful games are being played on the server, there are several rules in place to cut games short for various reasons.</p>
<ul>
<li>Food Not Being Gathered</li>
</ul>
<p>If a game consists of bots that aren't capable of gathering food, then the game is cutoff.  It is assumed that these are starter bots or very unsophisticated bots.  If the total amount of food is 90% of the count of food and ants for 150 turns then the cutoff is invoked.</p>
<ul>
<li>Ants Not Razing Hills</li>
</ul>
<p>If a game consists of a dominant bot that isn't razing enemy hills, then the game is cutoff.  It is assumed that the bot would probably not lose the lead and just isn't sophisticated enough to go in for the kill.  If the total amount of live ants for the dominant bot is 90% of the count of food and ants for 150 turns then the cutoff is invoked.  <strong>Update</strong>: Because a bot with a large hive count cannot be taken if they do not move off of the hill, we will stall this cutoff if there is any dead ant on top of a hill not owned by the dominant bot.  This gives the dominant bot time to drain the hive count to 0 and score for razing the hill.</p>
<ul>
<li>Lone Survivor</li>
</ul>
<p>If there is only 1 bot left alive in the game, then the game is cutoff.  All other bots have been completely eliminated (no ants on the map) or have crashed or timed out.  Remaining enemy hills are awarded to the last bot and points subtracted from the hill owners.</p>
<ul>
<li>Rank Stabilized</li>
</ul>
<p>If there is no bot with hills left that can gain enough points to gain in rank, then the game is cutoff.  Even though bots without hills left could still possibly gain in rank, the game is not extended them, only those with hills.  For each bot with a hill, it's maximum score (calculated assuming it could capture all remaining enemy hills) is compared to each opponents minimum score (calculated assuming it would lose all remaining hills).  If any score difference can overtake or break a tie then the game continues.  If no bot meets this criteria, the game is stopped.</p>
<p>(e.g. For a 4 player game, if bot A razes the hills of bot B and C, the scores are A=5, B=0, C=0 and D=1.  Even if bot D razes the hill of bot A the score would be A=4 and D=3, so D can't possible do better than 2nd place and the game ends.)</p>
<p>(e.g. For the same 4 player game, if bot A razes the hill of bot B and bot B still has ants, it is free to attempt to gain points.  But after bot A razes the hill of bot C it is no longer given the opportunity even though it could end with a score of A=4, B=2, C=0, D=0.)</p>
<ul>
<li>Turn Limit Reached</li>
</ul>
<p>There is a maximum turn limit for each map.  Each bot is given the limit.  The game ends at this point.  The limit will be adjusted so about 90%-95% of the games will be ended by this cutoff.</p>
<!--</MarkdownReplacement>-->
</div>

<div id="Food-Harvesting">
<!--<MarkdownReplacement with="competition-Food-Harvesting.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="food-harvesting">Food Harvesting</h2>
<p>Harvesting of food occurs each turn after the battle resolution process. If there are ants located within the spawn radius of a food location one of two things will occur:</p>
<ul>
<li>If there exist ants within the spawn radius belonging to more than one distinct bot then the food is destroyed and disappears from the game. </li>
<li>If the ants within the spawn radius all belong to the same bot then the food is harvested and placed into the hive for that bot.</li>
</ul>
<!--</MarkdownReplacement>-->
</div>

<div id="Ant-Spawning">
<!--<MarkdownReplacement with="competition-Ant-Spawning.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="ant-spawning">Ant Spawning</h2>
<p>As food is harvested, it is placed in the "hive".  Each food will spawn 1 ant.  Ants are only spawned at hills.</p>
<ul>
<li>The hill must not have been razed.</li>
<li>The hill must not be occupied by an ant.</li>
</ul>
<p>Only 1 ant can be spawned on a hill each turn.</p>
<p>For maps with multiple hills, 1 ant can be spawned at each hill if there is enough food in the hive.  If there is less food in the hive than there are hills, each hill is given a priority.  The last hill to have an ant on top is chosen last or the hill to have been touched the longest ago is chosen first.  In case of a tie, a hill is chosen at random.</p>
<p>This means that if you always move ants off of the hill right away, the spawned ants should be evenly spread between the hills.</p>
<p>You can control which hill to spawn at by keeping an ant nearby to block the hill when you don't want it to spawn ants.</p>
<!--</MarkdownReplacement>-->
</div>

<div id="Food-Spawning">
<!--<MarkdownReplacement with="competition-Food-Spawning.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="food-spawning">Food spawning</h2>
<p>Food spawning is done symmetrically.  Every map is symmetric, meaning each bot's starting position looks like every other bots starting position.</p>
<ul>
<li>Each game will start with a few food items placed within the bots starting vision, about 2-5.</li>
<li>Starting food will be placed randomly on the map as well, symmetrically.</li>
<li>Each game has a hidden food rate that will increase the amount of food in the game.  Then the amount to be spawned is divisible by the number of players, then that amount of food will spawn symmetrically.</li>
<li>The entire map is divided into sets of squares that are symmetric.  The sets are shuffled into a random order. When food is spawned, the next set is chosen.  When all the sets have been chosen, they are shuffled again.</li>
<li>Every set will spawn at least once before a set spawns a second time.  This means if you see food spawn, it may be awhile before it spawns again, unless it was the last set of the random order and was then shuffled to be the first set of the next random order.</li>
<li>Sometimes squares are equidistant to 2 bots.  This makes for a set that is smaller than normal.  The food rate takes this into account when spawning food.</li>
<li>Some maps have mirror symmetry so that a set of symmetric squares are touching.  It would be unfair to spawn so much food in one place, so these sets are not used.  If you can find a mirror symmetry after exploring the map, then you can avoid those spots when looking for food.</li>
</ul>
<p>The best way to gather the most food is to explore the map and cover the most area.</p>
<!--</MarkdownReplacement>-->
</div>

<div id="Battle-Resolution">
<!--<MarkdownReplacement with="competition-Battle-Resolution.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="battle-resolution">Battle Resolution</h2>
<div class="codehilite"><pre><span></span>    // how to check if an ant dies
    for every ant:
        for each enemy in range of ant (using attackadius2):
            if (enemies(of ant) in range of ant) &amp;gt;= (enemies(of enemy) in range of enemy) then
                the ant is marked dead (actual removal is done after all battles are resolved)
                break out of enemy loop
</pre></div>


<ul>
<li>Ants within the attack radius of each other kill each other (sometimes).</li>
<li>If you have more ants than another bot in the area, you won't die (usually).</li>
<li>The battle resolution is locally deterministic, meaning<ul>
<li>you only need to know an ants surroundings</li>
<li>and it is easy for the computer to solve.</li>
</ul>
</li>
<li>The battle resolution is:<ul>
<li>fun!</li>
<li>means killing your foe without taking loses</li>
<li>enables defending your hill with a few ants and inflicting massive losses to the enemy (Sparta!)</li>
<li>allows for awesome formations yet to be discovered</li>
<li>gets really weird with 3 or more bots in the fight (wait for the other guys to kill each other?)</li>
<li>doesn't need to concern you if you use Ender's method of winning the game (the ant hill is down)</li>
<li>is the most fun part of this game!</li>
</ul>
</li>
</ul>
<p>You should read more about it on the <a href="specification_battle.php">Focus Battle Resolution Page</a></p>
<!--</MarkdownReplacement>-->
</div>

<div id="Hill-Razing">
<!--<MarkdownReplacement with="competition-Hill-Razing.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="hill-razing">Hill Razing</h2>
<p>The objective of the game is to raze your opponents hills and defend your own hill.</p>
<p>A hill is razed (destroyed) when:</p>
<ul>
<li>An enemy ant is at the same location as the hill after the attack phase.</li>
</ul>
<p>Razed hills do not spawn ants anymore.  If all your hills have been razed, but you still have ants, your bot is still alive and your ants can still move, attack, gather food and raze hills.</p>
<!--</MarkdownReplacement>-->
</div>

<div id="Bot-Input">
<!--<MarkdownReplacement with="competition-Bot-Input.md">--><!--</MarkdownReplacement>-->
</div>

<div id="Bot-Output">
<!--<MarkdownReplacement with="competition-Bot-Output.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="bot-output">Bot Output</h2>
<p>Once the bot has been passed the parameters at the start of the game and it has finished setting up, it is to indicate this to the engine by outputting "go" on its own line.</p>
<p>Each bot may move any number of their ants each turn either north, east, south or west, provided the destination square is not water. Each move should be on a separate line with the following format:</p>
<div class="codehilite"><pre><span></span>o row col direction
</pre></div>


<p>Grid indexes start from zero and directions are to be passed as either 'N', 'E', 'S' or 'W'. At the end of each turn, bots are to output "go" to indicate to the engine that it has finished issuing moves. </p>
<p><strong>Sample Output:</strong>  </p>
<div class="codehilite"><pre><span></span># [ { &quot;embedded&quot;: true, &quot;decorated&quot;: false }, 200, 200, {} ]
rows 20 
cols 20 
players 2
m ....................
m ....................
m ....................
m ....................
m ....................
m ....................
m .....*..............
m ......%..b..........
m ....................
m ....................
m ........aa..........
m ....................
m ....................
m ....................
m ....................
m ....................
m ....................
m ....................
m ....................
m ....................
</pre></div>


<p>Below is sample output from player 'a' in the above game:</p>
<div class="codehilite"><pre><span></span>go  
o 10 8 N 
o 10 9 N
go
</pre></div>


<p>Below is sample output from player 'b' in the above game:</p>
<div class="codehilite"><pre><span></span>go  
o 7 9 W 
go
</pre></div>


<h3 id="blocking">Blocking</h3>
<p>Only movement is blocked by water. Ants can see and attack over water. If a bot issues a move for an ant onto water, it is considered an invalid move and therefore ignored.</p>
<p>Food will also block an ants movement.  This can happen if food spawns next to an ant.  Don't move the ant and it will be gathered the next turn.</p>
<h3 id="collisions">Collisions</h3>
<p>You can order 2 ants to the same space.  If you do this, both ants will die.  You can order your ant to go to the same space as an enemy ant, both ants will die before the attack radius is considered.  This can happen if an ant spawns near an enemy.  (You might also be close to losing your hill if enemies are right next to where you spawn!)</p>
<!--</MarkdownReplacement>-->
</div>

<div id="Map-Format">
<!--<MarkdownReplacement with="competition-Map-Format.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="map-format">Map Format</h2>
<p>A map consists of a rectangular grid. Each square can contain land, water, food, a live ant, multiple dead ants or an ant hill.  Ants can also be on top of their own hill. The edges of the map are wrapped, meaning if you walk off the top of the map, you will appear at the bottom, or if you walk off the right, you will appear on the left, assuming water doesn't block your path.</p>
<p>A map file is like an ordinary text file, except the extension is ".map" rather than ".txt", with the following format:</p>
<div class="codehilite"><pre><span></span>rows noRows
cols noCols
players noPlayers
score s1 s2 ...
hive h1 h2 ...
m [.%*!?a-jA-J0-9]
</pre></div>


<p>The symbols used have the following meaning:</p>
<div class="codehilite"><pre><span></span>.   = land
%   = water
*   = food
!   = dead ant or ants
?   = unseen territory
a-j = ant
A-J = ant on its own hill
0-9 = hill
</pre></div>


<p>Maps can almost describe the start of any turn of the game, except for multiple dead ants on the same square and who the owner(s) are.</p>
<p>For running games, the maps generated only need describe the start of the game and use only a subset of the full map specification.  Food, ants, dead ants are thrown out.  No square should be unseen.  No score or hive amounts should be included.  Maps for games should be symmetric.</p>
<p><strong>Sample Map:</strong></p>
<div class="codehilite"><pre><span></span>rows 20 
cols 20 
players 2
m ....................
m ....................
m ....................
m ....................
m ....................
m ....................
m .....*..............
m ......%..b..........
m ....................
m ....................
m ........aa..........
m ....................
m ....................
m ....................
m ....................
m ....................
m ....................
m ....................
m ....................
m ....................
</pre></div>


<h2 id="map-generators">Map Generators</h2>
<p>Maps are generated randomly using a program which is designed to try and result in interesting games while not allowing for people to hard code strategies.</p>
<ul>
<li>Maps are limited to 2 to 10 players</li>
<li>Maps must be symmetric</li>
<li>Hills must be between 20 and 150 steps away from other enemy hills (friendly hills can be closer)</li>
<li>Hills may not be within close range, euclidean distance no less than 6</li>
<li>Must be a path through all hills traversable by a 3x3 block</li>
<li>Maps must not contain islands</li>
<li>Maps are limited to at most 200 in each direction</li>
<li>Maps are limited in area to 900 to 5000 area per player, with a total area limit of 25,000.</li>
</ul>
<p>There are currently no further restrictions on what form the map generator for the final contest will take.  New map generators will be welcomed by anyone who wishes to write one.  Generators should do the following:</p>
<ul>
<li>Produce a map with the above qualities</li>
<li>Can be written in any language</li>
</ul>
<!--</MarkdownReplacement>-->
</div>

<div id="Replay-Format">
<!--<MarkdownReplacement with="competition-Replay-Format.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="formats-in-use">Formats in use</h2>
<p>There are two formats the engine can write. The first format is a streaming format that outputs data turn by turn. It is about ten times larger, but can be used to view games in progress (see 'Streaming format' below). The other format is used to store replays on the disk (see 'Storage format' below).</p>
<h3 id="storage-format">Storage format</h3>
<p>Replays you can download from servers are likely the storage format which is replay data and meta data in JavaScript object notation (JSON). Here is an example file (with the replay data truncated):</p>
<div class="codehilite"><pre><span></span>{
    &quot;challenge&quot;: &quot;ants&quot;,
    &quot;date&quot;: &quot;11-11-1111&quot;,
    &quot;playernames&quot;: [&quot;amstan&quot;, &quot;a1k0n&quot;, &quot;mega1&quot;],
    &quot;playerstatus&quot;: [&quot;timeout&quot;, &quot;crash&quot;, &quot;Some other message&quot;],
    &quot;submitids&quot;: [6, 3, 7],
    &quot;user_ids&quot;: [94, 813, 39],
    &quot;user_url&quot;: &quot;http://aichallenge.org/profile.php?user_id=~&quot;,
    &quot;game_id&quot;: &quot;12345&quot;,
    &quot;game_url&quot;: &quot;http://aichallenge.org/ants/visualizer.php?game_id=~&quot;,
    &quot;replayformat&quot;: &quot;json&quot;,
    &quot;replaydata&quot;: {
        &quot;revision&quot;: 2,
        &quot;players&quot;: 3,
        &quot;turns&quot;: 200,
        &amp;lt;...&amp;gt;
    }
}
</pre></div>


<h3 id="meta-data">Meta data</h3>
<p>This format was designed to be future proof, so it contains additional information to distinguish it from other replays as well as meta data</p>
<ul>
<li><em>challenge</em> is the name of the challenge or game that this replay is for. If it doesn't read "ants" you don't need to parse anything else.</li>
<li><em>replayformat</em> for ants should be "json"</li>
<li><em>replaydata</em> is the replay in the format set by "replayformat". As only "json" is in use, the replay data will automatically be parsed by your JSON library of choice.</li>
<li><em>user_url</em> and <em>user_ids</em> can be used to get a link to each user's page on the respective server. The ~ is replaced by the id.</li>
<li><em>game_url</em> and <em>game_id</em> can be used to find the original game.</li>
<li><em>playerstatus</em> explains what happened to a player after its last turn. The status could be any string with spaces, but the following predefined strings should be used it appropriate: 'timeout' (could be displayed as '... did not respond in time' or '... timed out') = the bot did not respond in time and was disqualified, 'crash' = the bot program crashed, 'eliminated' = no ants survived. 'survived' = the bot survived to the end of the game. Other status messages could also be used and must be displayed literally. E.g.: 'The bot tried to install a root kit'.</li>
<li><em>submitids</em> Each ant bot code submitted to the contest is assigned a unique id. For reference this can be included in the metadata. In case player's submissions are made available after the contest, the id from a downloaded replay can be used to find the code.</li>
<li><em>playercolors \&lt;array of html color codes></em> The replay file can set player colors in html notation. This is either #rrggbb or #rgb where r, g and b are upper- or lowercase hexadecimal digits. A visualizer must be prepared to select default colors for the players if the replay lacks them. An example can be found here [[http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/]]
same color.</li>
<li><em>antvalue \&lt;bounty></em> The score value for a dying ant.</li>
</ul>
<p>A visualizer is required to understand and check the "challenge" key, then check the "replayformat" and finally read the "replaydata". The other keys are optional information.</p>
<h3 id="replay-data">Replay data</h3>
<p>The "replaydata" field is an object made up of a list of all ants in the game, the map, the scores for each player and turn and some game settings.</p>
<ul>
<li><em>revision 2</em> The revision of this specification that was used to generate the replay.</li>
<li><em>players \&lt;number></em> This sets up the number of players in the replay. It can be anywhere from 1 to 26.</li>
<li><em>loadtime \&lt;time in ms></em> This time was given to the bot executables to load up before the match started.</li>
<li><em>turntime \&lt;time in ms></em> This time was given to the bot for each turn.</li>
<li><em>turns \&lt;number></em> The turn limit that was set for the match, which may have ended earlier.</li>
<li><em>viewradius2 \&lt;number></em> The squared ant view radius.</li>
<li><em>attackradius2 \&lt;number></em> The squared attack radius.</li>
<li><em>spawnradius2 \&lt;number></em> The squared spawn radius.</li>
<li><em>\&lt;parameter name> \&lt;value></em> Other parameters can have arbitrary names and are meant for an easy extension of the format.</li>
<li><em>map &lt;object&gt;</em> This object contains the map.<ul>
<li><em>rows \&lt;number></em> The number of rows in the map.</li>
<li><em>cols \&lt;number></em> The number of columns in the map.</li>
<li><em>data \&lt;array></em> An array of strings, one for each row. Every string in the array must contain as many characters as there are columns. The used characters in maps are: . = land, % = water, * = food. If a player has a starting ant, instead of '.' the map will show a letter from 'a' to 'z' = player 1 to 26. These spawns and the food locations are not authorative for the visualizer, but can be used as a preview in a replay browser application. That said a visualizer should assume all squares to be land if their character is not '%'.</li>
</ul>
</li>
<li><em>ants \&lt;array></em> An array of all ants in the game. Food and the ant it is eventually converted into is also a single element of this array. Food can be considered an ant which doesn't have an owner yet. There are some cases to be considered: Some food is never converted, some ants are there from the beginning of the game, some ants survive, some food could be removed from the game if the rules allow two enemy ants to come close enough to the food item. Each element of the ants array is itself an array with either 4 elements (food, which is not converted) or 7 elements: <em>\&lt;row> \&lt;col> \&lt;start turn> \&lt;conversion turn> \&lt;end turn> \&lt;player> \&lt;moves></em> Each object has an initial \&lt;row> and \&lt;col> on the map as well as the \&lt;start turn> in which they first appear on the map. For the \&lt;conversion turn> and the \&lt;end turn> there is a special rule that makes parsing easier: The turn number is the total number of turns +1 if the food or ant survives the game. The \&lt;conversion turn> tells the visualizer when the food is converted into an ant or disappears from the map. The parameter list may end here if the food is not converted to a player (i.e. it disappears or survives the whole game). If the food is converted, the following parameters exist. \&lt;end turn> is basically the same as \&lt;conversion turn>. It is either total turns +1 or the turn in which the ant is dead. \&lt;player> is the 0 based player number and \&lt;moves> is a string of commands issued by the bot for the ant. Each character is either '-' = do nothing or a move order ('n', 'e', 's', 'w') for a turn starting with \&lt;conversion turn>. Note: For ants that are there from the beginning of the game both \&lt;start turn> and &lt;\conversion turn> are 0!</li>
<li><em>scores \&lt;array></em> There will be exactly as elements in this array as there are <em>players</em>. The first element is for player 1, the second for player 2 and so on. Each element is an array of floating point score values, where each value represents the player's score for the start of a turn. If a player crashed before completing its first turn there will be one score entry (usually 0). If a player survived a 200 turns game, there will be 201 values including the end game score. A reason why a certain player did not make it to the end may be given in the meta data.</li>
<li><em>bonus \&lt;array></em> An optional field that holds the "food bonus" value for each player. Any number which is not 0 is added to the score after the last turn in the game.</li>
</ul>
<h3 id="streaming-format">Streaming format</h3>
<p>The streaming format can be used to visualize games in progress. It produces a lot more data than the storage format, but has the benefit of being turn based. That means that each turn can be visualized as soon as the engine completed it.
Details of the format follow...</p>
<!--</MarkdownReplacement>-->
</div>

<!--<MarkdownReplacement with="competition-Game-Specification.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="miscellaneous">Miscellaneous</h2>
<p>In order to ensure that people are able to see how well their bot performs relative to the other people who decide to participate in the contest, we encourage people not to share bot code that goes beyond the functionality of a starter bot until final submissions are closed.</p>
<p>It is still alright, in fact highly encouraged, for people to share strategy ideas and general tools that people can use, both to improve their bot or enable easier development and debugging.</p>
<h3 id="more-information">More Information</h3>
<p>If you have a question about the game mechanics, you may ask in the <a href="http://forums.aichallenge.org/">forums</a>, in the #aichallenge IRC channel on Freenode, or go straight to the contest's <a href="https://github.com/aichallenge/aichallenge">source code</a>.</p>
<!--</MarkdownReplacement>-->

<?php

require_once('visualizer_widget.php');
visualize_pre();
require_once('footer.php');

?>

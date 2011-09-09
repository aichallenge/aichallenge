<?php include 'header.php'; ?>

<!--<MarkdownReplacement with="competition-Game-Specification.md">--><h1>Ants Game Specification</h1>

<p>Ants is a multi-player strategy game set on a rectangular grid with the edges wrapped. The objective is to create a computer program in a language of your choice that plays the game of Ants as intelligently as possible, which consists of eliminating the enemy colonies while taking over all of the resources.</p>

<p>Your bot is a program that inputs information about squares it can currently see and outputs orders to move any number of their ants either north, east, south or west. Each order specifies an ant location and direction to move it. Once all the bots have issued their orders, ants move one square in the given direction. Enemy ants that land too close to each other are automatically engaged in battle. Similarly, if an ant lands close enough to a food square, it spawns into a new ant for that bot.</p>

<p>The game ends when only one player remains, or if the game goes past a certain number of turns.</p>

<p>In order to ensure that people are able to see how well their bot performs relative to the other people who decide to participate in the contest, we encourage people not to share code that specifically implements anything related to the moves a bot would make until final submissions are closed. </p>

<p>It is still alright, in fact highly encouraged, for people to share strategy ideas and general tools that people can use, both to improve their bot or enable easier development and debugging.</p>

<h1>Maps</h1>

<p>A map consists of a rectangular grid. Each square can contain land, water, food, a live ant or dead ants. The edges of the map are wrapped, meaning if you walk off the top of the map, you will appear at the bottom, or if you walk off the right, you will appear on the left, assuming water doesn't block your path.</p>

<p>A map file is like an ordinary text file, except the extension is ".map" rather than ".txt", with the following format:</p>

<pre><code>rows noRows
cols noCols
players noPlayers
m [.%*abc]
</code></pre>

<p><strong>Sample Map:</strong></p>

<pre><code>rows 20 
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
</code></pre>

<h2>Map Generator</h2>

<p>Maps are generated randomly using a program which is designed to try and result in interesting games while not allowing for people to hard code strategies.</p>

<p>There are currently no further restrictions on what form the map generator for the final contest will take, more information will be made available as it is known.</p>

<h2>Bot Input</h2>

<p><strong>Parameter Information:</strong> <br />
At the start of a game, each bot is passed general parameters about the game, this begins with "turn 0" on its own line. Parameters will then be passed on separate lines with the following format:</p>

<pre><code>type value
</code></pre>

<p>All values are integers and the possible parameter types are:</p>

<pre><code>"loadtime"       # in milliseconds
"turntime"       # in milliseconds
"rows"           # number of rows in the map
"cols"           # number of columns in the map
"turns"          # maximum number of turns in the game
"viewradius2"    # view radius squared
"attackradius2"  # battle radius squared
"spawnradius2"   # spawn radius squared
</code></pre>

<p>Once all parameters have been passed you will receive "ready" on a separate line, at which point you are free to set up for as long as the loadtime specifies.</p>

<p><strong>Turn Information:</strong> <br />
Each following turn begins with one of the following lines:</p>

<pre><code>turn turnNo
end
</code></pre>

<p>"end" indicates that the game is over, the winner of the game will receive information for the final state of the game following this, should they wish to use it for local testing.</p>

<p>If the game is over, bots will then receive two lines giving the number of players and scores in the following format:</p>

<pre><code>players noPlayers
score p1Score ... pnScore
</code></pre>

<p>You are then passed information about the squares you can currently see with the following format:</p>

<pre><code>w row col                            # water
f row col                            # food
r row col                            # food removed
a row col owner                      # live ant
d row col owner                      # dead ant
</code></pre>

<p>The end of input for a turn is indicated by receiving "go" on its own line.</p>

<p>You are always passed information as though you are player zero, the first enemy you see always appears as player one, and so on. This helps to ensure that you do not know how many players started in the game.</p>

<p>Information about a water square will only be sent the first turn in which it is visible by one of your live ants (to reduce the amount of data transferred). You will be given information about removing food for a visible square that contained food the last time you saw it and that currently does not contain food.</p>

<p>You will be passed information for live ants and food squares every turn they are within sight of one of your live ants. </p>

<p>Information is given for ants that died during the collision or battle resolution of the previous turn if it is in a square currently visible by one of your live ants. These squares are merely for your information if you wish to use them, they can otherwise be thought of as land and moved into that turn.</p>

<p><strong>Sample Input:</strong> <br />
Below is sample input for player 'a' in the above game:</p>

<pre><code>turn 0
loadtime 2000  
turntime 2000  
rows 20  
cols 20  
turns 500  
viewradius2 93  
attackradius2 6  
spawnradius2 6  
ready  

turn 1
f 6 5
w 7 6
a 7 9 1 
a 10 8 0
a 10 9 0
go  

end
players 2
score 1 0
f 6 5
d 7 8 1 
a 9 8 0
a 9 9 0
go
</code></pre>

<p>Below is sample input for player 'b' in the above game, starting from the first turn:</p>

<pre><code>turn 1
f 6 5
w 7 6
a 7 9 0 
a 10 8 1
a 10 9 1
go  

end
players 2
score 1 0
go
</code></pre>

<h2>Bot Output</h2>

<p>Once the bot has been passed the parameters at the start of the game and it has finished setting up, it is to indicate this to the engine by outputting "go" on its own line.</p>

<p>Each bot may move any number of their ants each turn either north, east, south or west, provided the destination square is not water. Each move should be on a separate line with the following format:</p>

<pre><code>o row col direction
</code></pre>

<p>Grid indexes start from zero and directions are to be passed as either 'N', 'E', 'S' or 'W'. At the end of each turn, bots are to output "go" to indicate to the engine that it has finished issuing moves. </p>

<p><strong>Sample Output:</strong> <br />
Below is sample output from player 'a' in the above game:</p>

<pre><code>go  
o 10 8 N 
o 10 9 N
go
</code></pre>

<p>Below is sample output from player 'b' in the above game:</p>

<pre><code>go  
o 7 9 W 
go
</code></pre>

<h2>Distance</h2>

<p>Distances are calculated using the Euclidean metric, which gives the straight line distance between two points. For two locations (a) and (b), this can be calculated as follows:</p>

<p>[ dr = min(abs(a.row-b.row), \text{ rows}-abs(a.row-b.row)) ]
[ dc = min(abs(a.col-b.col), \text{ cols}-abs(a.col-b.col)) ]
[ distance(a, b) = \sqrt{{dr}^2 + {dc}^2} ]</p>

<p>Two locations (a) and (b) are considered to be within a distance of (r) if (distance(a, b) \leq r).</p>

<h2>Fog of War</h2>

<p>Each bot is passed a parameter at the start of the game indicating the square of each ants visibility, which is how far each ant can see around them. At the moment this is set at 93, giving a view radius of approximately 9.6. </p>

<p>Each turn you are only given current information for the squares that your live ants can currently see.</p>

<h2>Water Blocking</h2>

<p>Only movement is blocked by water. Ants can see, battle and spawn over water. If a bot issues a move for an ant onto water, it is considered an invalid move and therefore ignored.</p>

<h2>Turns</h2>

<p>Once each of the bots has indicated it has finished setting up, the game engine performs the following steps repeatedly:</p>

<ol>
<li>Send the game state to the players.</li>
<li>Receive orders from the players.</li>
<li>Update the game state.</li>
<li>Check for endgame conditions.</li>
</ol>

<p>There is a specified maximum turn limit. At the time of this writing, the maximum is undecided, but will be available when it is. The intent is to have this number nailed down later in the contest period.</p>

<p>A <em>turn</em> is defined as the above steps. They are performed up to the maximum number of turns times and then the game stops. </p>

<h1>Game State Update</h1>

<p>After receiving complete orders from the players, the engine then updates the game state, advancing the game to the next turn. This happens in four phases: movement, collision resolution, battle resolution, and spawn resolution.</p>

<p>If a bot issues an invalid move, it is just ignored. Attempting to walk an ant onto water is considered to be an invalid move.</p>

<h3>Collision Resolution</h3>

<p>If more than one ant is in a square once all moves have been made, each of these ants automatically dies.</p>

<h3>Battle Resolution</h3>

<p>If there are opponent ants within the attack radius of an ant, the ants automatically engage in a battle, even over water. The attack radius is currently set at (\small \sqrt{6}). The battles are resolved with the following steps:</p>

<ol>
<li>Ants with enemies in range distribute 1 point of damage, split evenly among those enemies. </li>
<li>Ants with at least 1 full point of damage from that turn die. </li>
</ol>

<h3>Spawn Resolution</h3>

<p>If a food square has ants from more than one player within the spawn radius of it, it disappears. If a food square has ants from a single player within the spawn radius of it, the food square spawns into an ant for that player. The spawn radius is currently set at (\small \sqrt{2}).</p>

<h2>Endbot Conditions</h2>

<p>Any of the following conditions will cause a player to finish participating in a game:</p>

<ul>
<li>The player has no live ants left remaining on the map.</li>
<li>The bot crashed.</li>
<li>The bot exceeded the time limit without completing its orders.</li>
<li>A bot attempts to do something that the tournament manager deems a security issue and is disqualified.</li>
</ul>

<p>If a player stops participating due to a crash or timeout, their ants remain on the board, can still collide and battle with other ants, and spawn new ones with food squares. Their ants just do not make any future moves and opponents are not explicitly told those ants owner are no longer participating.</p>

<h2>Endgame Conditions</h2>

<p>Any of the following conditions will cause the game to end:</p>

<ul>
<li>The turn limit is reached.</li>
<li>No more than one of the players is still participating in the game.</li>
</ul>

<h2>Scoring</h2>

<p>You get one point for every ant spawned, including any ants you have on the first turn. Each dying ant is worth one point. At the end of each turn, each ant that died distributes an equal proportion of these points to each of the enemy ants within the attack radius, including other dying ants, which are then given to the ants owner.</p>

<p>If an ant belonging to a bot that is no longer participating in the game contributes to the death of an enemy ant, that ant is assigned its fair share of points, but those points are not passed to its owner, they disappear from the game. If two friend ants collide with no enemy ants in range, those points also disappear from the game.</p>

<p>If the game ends before the maximum number of turns are reached, the last surviving bot gets a food bonus, which consists of the number of food squares currently on the map, the number of food squares that would have spawned in the remaining turns and the number of enemy ants left on the map from a bot that is no longer participating.</p>

<p>At the end of the game, each player is ranked based off the final scores, with a tie resulting in each player having the same ranking. The difference in scores is not reflected in the general ranking of bots, only your relative position for each game.</p>

<h2>More Information</h2>

<p>If you have a question about the game mechanics, you may ask in the <a href="http://ai-contest.com/forum/">forums</a>, in the #aichallenge IRC channel on Freenode, or go straight to the contest's <a href="https://github.com/aichallenge/aichallenge">source code</a>.</p>
<!--</MarkdownReplacement>-->

<?php include 'footer.php'; ?>

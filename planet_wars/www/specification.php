<?php include 'header.php'; ?>

<h2>Planet Wars Specification</h2>

<div id="TOC"
><ul
  ><li
    ><a href="#overview"
      >Overview</a
      ></li
    ><li
    ><a href="#the-map"
      >The Map</a
      ><ul
      ><li
	><a href="#planets"
	  >Planets</a
	  ></li
	><li
	><a href="#fleets"
	  >Fleets</a
	  ></li
	><li
	><a href="#file-format"
	  >File Format</a
	  ></li
	><li
	><a href="#about-the-current-maps"
	  >About the Current Maps</a
	  ></li
	></ul
      ></li
    ><li
    ><a href="#turns"
      >Turns</a
      ><ul
      ><li
	><a href="#bot-io"
	  >Bot I/O</a
	  ><ul
	  ><li
	    ><a href="#bot-orders"
	      >Bot Orders</a
	      ></li
	    ></ul
	  ></li
	><li
	><a href="#game-state-update"
	  >Game State Update</a
	  ><ul
	  ><li
	    ><a href="#departure"
	      >Departure</a
	      ></li
	    ><li
	    ><a href="#advancement"
	      >Advancement</a
	      ></li
	    ><li
	    ><a href="#arrival"
	      >Arrival</a
	      ></li
	    ></ul
	  ></li
	></ul
      ></li
    ><li
    ><a href="#endgame-conditions"
      >Endgame Conditions</a
      ></li
    ><li
    ><a href="#more-information"
      >More Information</a
      ></li
    ></ul
  ></div
>
<h3 id="overview"
><a href="#TOC"
  >Overview</a
  ></h3
><p
>Planet Wars is a game based on <a href="http://www.galcon.com"
  >Galcon</a
  >, but is designed to be a simpler target for bots. The contest version of the game is for two players.</p
><p
>A game of Planet Wars takes place on a map which contains several planets, each of which has some number of ships on it. Each planet may have a different number of ships. The planets may belong to one of three different owners: you, your opponent, or neutral. The game has a certain maximum number of turns. At the time of this writing, the maximum number of turns on the official server is 200, but it is not yet a part of the specification. Provided that neither player performs an invalid action, the player with the most ships at the end of the game wins. The game may also end earlier if one of the players loses all his ships, in which case the player that has ships remaining wins instantly. If both players have the same number of ships when the game ends, it’s a draw.</p
><p
>On each turn, the player may choose to send fleets of ships from any planet he owns to any other planet on the map. He may send as many fleets as he wishes on a single turn as long as he has enough ships to supply them. After sending fleets, each planet owned by a player (not owned by neutral) will increase the forces there according to that planet’s “growth” rate. Different planets have different growth rates. The fleets will then take some number of turns to reach their destination planets, where they will then fight any opposing forces there and, if they win, take ownership of the planet. Fleets cannot be redirected during travel. Players may continue to send more fleets on later turns even while older fleets are in transit.</p
><h3 id="the-map"
><a href="#TOC"
  >The Map</a
  ></h3
><p
>Maps have no particular dimensions and are defined completely in terms of the planets and fleets in them.</p
><h4 id="planets"
><a href="#TOC"
  >Planets</a
  ></h4
><p
>Planet <em
  >positions</em
  > are specified relative to a common origin in Euclidean space. The coordinates are given as floating point numbers. Planets never move and are never added or removed as the game progresses. Planets are not allowed to occupy the exact same position on the map.</p
><p
>The <em
  >owner</em
  > of a planet can be neutral, player 1, or player 2. Players always see themselves as player 1 and their opponents as player 2. The engine works out how to display the world differently to each player. The owner of a planet can change throughout the game. The owner is given as an integer with the following mapping:</p
><ul
><li
  >0 means neutral</li
  ><li
  >1 means player 1 (yourself, from your point of view)</li
  ><li
  >2 means player 2 (your opponent, from your point of view)</li
  ></ul
><p
>The <em
  >number of ships</em
  > is given as an integer, and it may change throughout the game.</p
><p
>The <em
  >growth rate</em
  > of the planet is the number of ships added to the planet after each turn. If the planet is currently owned by neutral, the growth rate is not applied. Only players can get new ships through growth. The growth rate of a planet will never change. It is given as an integer.</p
><p
>Each planet is also implicitly assigned an <em
  >ID</em
  >. These are assigned as integers according to the order in which the planets are specified in the map, starting from 0. A planet’s ID will never change throughout the game.</p
><h4 id="fleets"
><a href="#TOC"
  >Fleets</a
  ></h4
><p
>The <em
  >owner</em
  > is represented in the same way as for planets, and the <em
  >number of ships</em
  > is again an integer.</p
><p
>The <em
  >source planet</em
  > and <em
  >destination planet</em
  > are specified according to the planets’ IDs as specified above.</p
><p
>The <em
  >total trip length</em
  > is given as an integer, representing the total number of turns required to make the trip from source to destination. The <em
  >turns remaining</em
  > is also an integer, representing the number of turns left from the current turn to arrive at the destination. Trip lengths are determined at the time of departure by taking the Euclidean distance to the destination <IMG SRC="eqn008.png" WIDTH=56 HEIGHT=20 STYLE="vertical-align: -5px; margin: 0;"> from the source <IMG SRC="eqn005.png" WIDTH=51 HEIGHT=20 STYLE="vertical-align: -5px; margin: 0;"> and rounding up. That is, <IMG SRC="eqn010.png" WIDTH=196 HEIGHT=23 STYLE="vertical-align: -5px; margin: 0;">.</p
><h4 id="file-format"
><a href="#TOC"
  >File Format</a
  ></h4
><p
>The map file format is fairly simple. Each line may be blank, a planet, or a fleet, and each line is separated by Unix style line breaks (LF, not CR or CRLF). The <code
  >#</code
  > character and everything on the same line after it are treated as white space (comments), so a line beginning with a <code
  >#</code
  > character is considered blank, too.</p
><p
>Planet lines have the following format:</p
><pre
><code
  >P &lt;x:float&gt; &lt;y:float&gt; &lt;owner:int&gt; &lt;ships:int&gt; &lt;growth:int&gt;
</code
  ></pre
><p
>Fleet lines have the following format:</p
><pre
><code
  >F &lt;owner:int&gt; &lt;ships:int&gt; &lt;source:int&gt; &lt;destination:int&gt; &lt;total_turns:int&gt; &lt;remaining_turns:int&gt;
</code
  ></pre
><p
>Here is an example of a valid map:</p
><pre
><code
  ># Example map

P 0    0    1 34 2  # Player one's home planet.
P 7    9    2 34 2  # Player two's home planet.
P 3.14 2.71 0 15 5  # A neutral planet with real-number coordinates.

F 1 15 0 1 12 2     # Player one has sent some ships to attack player two.
F 2 28 1 2  8 4     # Player two has sent some ships to take over the neutral planet.
</code
  ></pre
><p
>In the above example, player 1’s planet has ID 0, player 2’s planet has ID 1, and the neutral planet has ID 2. In addition to the forces owned by each of the two players on the planets themselves, the two players each have fleets in transit. Player 1 has a fleet of 15 ships that is about to arrive at player 2’s home planet. Player 2 has a fleet of 28 ships that is half way to the neutral planet.</p
><p
>Map files should not contain fleet lines, but this format is also used for game state updates, as will be described below, so the fleet lines are included in the specification here.</p
><h4 id="about-the-current-maps"
><a href="#TOC"
  >About the Current Maps</a
  ></h4
><p
>Most maps on the server and in the starter packs were generated programmatically by a <a href="http://code.google.com/p/ai-contest/source/browse/trunk/planet_wars/backend/map_generator.py"
  >Python script</a
  >. You may also create your own to use for testing.</p
><h3 id="turns"
><a href="#TOC"
  >Turns</a
  ></h3
><p
>The game engine performs the following steps repeatedly:</p
><ol style="list-style-type: decimal;"
><li
  >Send the game state to the players.</li
  ><li
  >Receive orders from both players.</li
  ><li
  >Update the game state.</li
  ><li
  >Check for endgame conditions.</li
  ></ol
><p
>There is an unspecified maximum turn limit. At the time of this writing, the maximum is 200 turns, but this may change. The intent is to have this number nailed down later in the contest period.</p
><p
>A <em
  >turn</em
  > is defined as the above four steps. They are performed up to 200 times and then the game stops. This means that the players receive the game state up to 200 times and send sets of orders up to 200 times.</p
><h4 id="bot-io"
><a href="#TOC"
  >Bot I/O</a
  ></h4
><p
>The engine on the official server launches your bot in a sandbox environment and communicates with it via stdin and stdout. It silently absorbs your stderr stream, and your bot is prohibited from writing to files. A single game is a single instance of your bot process. That is, the same bot process is used from turn to turn in a single game, but an entirely different process is used for a new game. The unofficial engines may have their own ways of handling these things, so if you are testing with a different engine you are responsible for being aware of these details on a per case basis.</p
><p
>At the beginning of each turn, the engine sends the map state to both bots. The format of this map is the same as the map file format specified under “The Map” section above. The end of the game state is denoted by a single line containing the text “<code
  >go</code
  >”. For example, the same map example from earlier might look like this:</p
><pre
><code
  ># Example map

P 0    0    1 34 2  # Player one's home planet.
P 7    9    2 34 2  # Player two's home planet.
P 3.14 2.71 0 15 5  # A neutral planet with real-number coordinates.

F 1 15 0 1 12 2     # Player one has sent some ships to attack player two.
F 2 28 1 2  8 4     # Player two has sent some ships to take over the neutral planet.
go
</code
  ></pre
><p
>It’s unlikely that the engine will include comments in this output, but I wouldn’t rely on it not to. Note also that because planet IDs never change, the planets in the bot input will be in the same order from turn to turn.</p
><p
>Once the engine starts sending the game state to the bots, the bots each have 1 second of wall-clock time to receive the game state, process it, and send their orders. (The first turn is an exception, in which case the bots have 3 seconds of wall-clock time. This is to help bots written in languages whose VMs need some “warm-up” time.) Both bots perform these operations concurrently, and they are each unaware of what the other bot is doing. The choice of using wall-clock time rather than CPU time is primarily due to technical difficulties in measuring child processes’ CPU times in real time. If a simple way to monitor CPU time instead is discovered, this part of the spec may change.</p
><h5 id="bot-orders"
><a href="#TOC"
  >Bot Orders</a
  ></h5
><p
>An “order” is a line of text that the bot sends to the engine to make a fleet depart from a planet on the next game state update. The format of this line is as follows:</p
><pre
><code
  >&lt;source:int&gt; &lt;destination:int&gt; &lt;ships:int&gt;
</code
  ></pre
><p
>If the source and destination planets are the same, the bot instantly loses the game. If the number of ships is greater than is available at the source planet, the bot instantly loses the game. If the bot doesn’t own the source planet, the bot instantly loses the game. The bot may issue as many orders in a single turn as it likes so long as the sum of all ships in fleets leaving a planet is not greater than the ships residing on the planet.</p
><p
>When the bot is done issuing commands to the engine, it sends a single “<code
  >go</code
  >” line. Here is an example of the output from a bot for a single turn:</p
><pre
><code
  >1 17 50
4 17 50
go
</code
  ></pre
><p
>That means the bot wants to send 50 ships from planet 1 to planet 17 and 50 ships from planet 4 to planet 17.</p
><h4 id="game-state-update"
><a href="#TOC"
  >Game State Update</a
  ></h4
><p
>After receiving complete lists of commands from the players, the engine then updates the game state, advancing the game to the next turn. This happens in three phases: departure, advancement, and arrival.</p
><h5 id="departure"
><a href="#TOC"
  >Departure</a
  ></h5
><p
>In this phase, the players’ commands are carried out. This consists of creating new fleets and removing the appropriate numbers of ships from each planet. Fleet trip lengths are determined by taking the Euclidean distance to the destination from the source and rounding up.</p
><h5 id="advancement"
><a href="#TOC"
  >Advancement</a
  ></h5
><p
>This phase advances fleets and grows populations. Fleets are advanced by simply decrementing their “turns remaining” values.</p
><p
>For each planet, if it is owned by a player, its growth rate is added to its number of ships. For example, if the planet is owned by player 1, has 4 ships, and has a growth rate of 2, the planet will next have 6 ships, still having its growth rate of 2 and still being owned by player 1. However, had the planet been owned by neutral instead, its population would not have grown.</p
><h5 id="arrival"
><a href="#TOC"
  >Arrival</a
  ></h5
><p
>This phase handles fleets whose “turns remaining” became zero during the advancement phase. It does so by considering each destination planet at a time.</p
><p
>For each planet, consider its owner and ship count along with the owners and ship counts of each fleet. We will call each of these groups a “force.” Combine the forces according to their owners. For example, if the planet is owner by player 1 with a population of 5 ships, two of player 1’s fleets are arriving with 3 ships each, and two of player 2’s fleets are arriving with 5 ships each, the result of this operation gives us a force owned by player 1 consisting of 11 ships and a force owned by player 2 consisting of 10 ships.</p
><p
>If there is only one force, that is the new occupation of the planet.</p
><p
>If there are two forces, the new owner of the planet is the owner of the largest force, and the losing player’s ship count is subtracted from the winning player’s ship count as the new population. However, if both forces are the same size, then the winner is the original owner of the planet, and the planet’s new ship count is zero.</p
><p
>If the original owner of the planet was neutral, then it is possible for there to be three forces fighting for one planet. In this case, the owner of the largest force is the new owner of the planet, and his ship count is reduced by the number of ships in the second largest force. If the top two forces are the same size, the original owner retains ownership of the planet but the forces are set to zero.</p
><p
>There is an intuitive way to understand these rules. Let’s look at the rules with an example. Say player 1 has a force of 5 ships, player 2 has a force of 4 ships, and neutral has a force of 3 ships. We create groups of units that will battle, each group consisting of two ships from different forces or of three ships all from different forces. In this case, the division is as follows:</p
><ul
><li
  >P1 vs. P2 vs. N</li
  ><li
  >P1 vs. P2 vs. N</li
  ><li
  >P1 vs. P2 vs. N</li
  ><li
  >P1 vs. P2</li
  ><li
  >P1</li
  ></ul
><p
>Each group of two or three fighting ships cancels out. In this battle, player 1 wins with a single ship left.</p
><p
>If we apply the original procedure to the above battle, we use the three-way rules. The two largest forces are player 1’s and player 2’s. The winner must be player 1 because he has the most ships, and we subtract player 2’s ships from player 1’s to obtain the new ship count, <IMG SRC="eqn007.png" WIDTH=68 HEIGHT=16 STYLE="vertical-align: -2px; margin: 0;">.</p
><p
>In actuality, all three cases for one, two and three forces follow the same procedure. In all cases, the largest force wins, and the second largest force is subtracted from the first, and in the case of a tie the original owner keeps the planet with zero ships remaining. A critical detail is that if forces completely cancel out then the original owner retains the planet.</p
><h3 id="endgame-conditions"
><a href="#TOC"
  >Endgame Conditions</a
  ></h3
><p
>The following conditions will cause the game to end:</p
><ul
><li
  >The turn limit is reached. The winner is the player with the most ships, both on planets and in fleets. If both players have the same number of ships, it’s a draw.</li
  ><li
  >One player runs out of ships entirely. The winner is the other player.</li
  ><li
  >Both players run out of ships at the same time. The game is a draw.</li
  ><li
  >A bot issues a command with the same source and destination planet and forfeits the game.</li
  ><li
  >A bot sends invalid data and forfeits the game.</li
  ><li
  >A bot crashes and forfeits the game.</li
  ><li
  >A bot exceeds the time limit without completing its orders (it never sends a line that says “<code
    >go</code
    >”) and is disqualified. This is perhaps overly harsh, but is the way it currently works. It may change in the future to simply be a forfeit.</li
  ><li
  >A bot attempts to do something that the tournament manager deems a security issue and is disqualified.</li
  ></ul
><h3 id="more-information"
><a href="#TOC"
  >More Information</a
  ></h3
><p
>If you have a question about the game mechanics, you may ask in the <a href="http://ai-contest.com/forum/"
  >forums</a
  >, ask in the #aichallenge IRC channel on Freenode, or go straight to the contest’s <a href="http://code.google.com/p/ai-contest/source/browse/#svn/trunk/planet_wars"
  >source code</a
  >.</p
>

<?php include 'footer.php'; ?>

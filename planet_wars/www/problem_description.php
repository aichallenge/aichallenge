<?php include 'header.php'; ?>

<h2>Problem Description</h2>
<p>Who can survive the longest in the game of Tron!?</p>
<p>The objective is to create an AI to play the classic game Tron, Surround, Snafu, Snake, etc. You begin with a start location on an m by n grid and are asked to move in a corresponding direction (north, south, east, west). When players, move their previous location remains as a "jet wall". A player must try to avoid running into their opponents "jet wall", their own "jet wall", and outside the "game grid" in order to successfully beat their opposition.</p>
<p>Check out the diagram below for a visual of the jet walls, or 'tails'.</p>
<center><img src="vis.png"></center>
<p>In order to win, you must complete a move without crashing (into the wall or a jet wall) when your opposition fails to do so. Note that it is possible to have a tie in the case that both players attempt to enter the same location in the grid during the same move as moves are done simultaneously.</p>
<p>Build an efficient algorithm to beat out your opposition by cutting them off, blocking them in, taking them into the maze, or making them fall out of the grid. Remember, there will only be one survivor, all other programs will be terminated.</p>

<?php include 'footer.php'; ?>

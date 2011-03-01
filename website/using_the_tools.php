<?php include 'header.php'; ?>
<h2>Using the Tools</h2>
<p>So you completed the <a href="quickstart.php">Five-Minute Quickstart Guide
  </a>, and you see your name on the leaderboard. Now what? In this guide,
  you will use the Planet Wars game visualizer to watch two of the sample
  bots play against each other.</p>
<p>If you have any trouble with this tutorial, please complain on the
  <a href="forums/">forums</a> so that we know to fix it. We'll do whatever
  it takes to get you off the ground!</p>
<h3>Getting the Tools</h3>
<p>If you haven't done so already, download a <a href="starter_packages.php">
  starter package</a>. Unzip the starter package in a location of your choice.
  Since we will be using a command-line terminal in this tutorial, it is better
  to choose a location that is easy to find using a terminal. I chose to
  unzip my starter package in C:\planetwars\. This creates a new folder
  C:\planetwars\JavaStarterPackage\ which contains all the materials that we
  need for this tutorial.</p>
<h3>Watch Two Bots Fight</h3>
<p>Now it's time to watch two bots fight. For this step, we will have to use
  a command-line terminal. But don't worry, it's really easy! To open a
  command-line terminal in Windows, open the "run" dialog, type "cmd", then
  press the Run button. You should get a black window with a flashing white
  cursor.</p>
<p>First, go to the starter package
  that you just unzipped using the following command. If you unzipped your
  starter package in a location different from C:\planetwars\, then modify
  the command accordingly. If you chose a language other than Java, you
  will also have to modify this command.</p>
<p class="code">cd C:\planetwars\JavaStarterPackage\</p>
<p>Next, we will invoke the game visualizer. Don't worry if you don't
  understand this long command. All you have to do is copy and paste it.</p>
<p class="code">java -jar tools/PlayGame.jar maps/map7.txt 1000 1000 log.txt
  "java -jar example_bots/RandomBot.jar" "java -jar example_bots/DualBot.jar"
  | java -jar tools/ShowGame.jar</p>
<p>The above command plays a game between two of the sample strategies. Once
  the game is complete, the visualizer window pops up to play back the game
  graphically. Hit the play button to watch the game.</p>
<p>In the playback you just saw, player 1 is called RandomBot, and player 2 is
  called DualBot. RandomBot basically works by choosing random legal moves.
  It's not very smart. DualBot uses a slightly more intelligent strategy, and
  changes its level of agressiveness depending on whether it is winning or
  losing. There are other sample strategies that come with the starter package:
  </p>
<ul>
  <li>BullyBot</li>
  <li>DualBot</li>
  <li>ProspectorBot</li>
  <li>RageBot</li>
  <li>RandomBot</li>
</ul>
<p>You can also choose from a large variety of maps. The starter packages each
  come with 100 different maps. For example, to watch RageBot play against
  ProspectorBot on map number 43, use the following command.</p>
<p class="code">java -jar tools/PlayGame.jar maps/map43.txt 1000 1000 log.txt
  "java -jar example_bots/RageBot.jar" "java -jar
  example_bots/ProspectorBot.jar" | java -jar tools/ShowGame.jar</p>
<p>Play around with different combinations of bots on different maps. Have fun
  laughing at how much RandomBot and RageBot suck. In the next tutorial, you
  will create a simple bot of your own and watch it play against the sample
  bots. You can even play your strategy against itself!</p>
<p>Note for people using Linux: if you are SSH-ed into some server other than
  your local machine, then you must use X-forwarding in order to see the
  graphical playback interface. Alternatively, you can just follow this
  tutorial on your own local machine to solve this problem.</p>
<h3>Next Steps</h3>
<p>So you have had enough of playing with the tools that come with the starter
  package? In the next tutorial, you will make one little change to the
  starter package which should give you a big bump in the rankings. Here are
  some places to look for guidance as you move forward on your quest to the
  top of the leaderboard!</p>
<ul>
  <li><a href="starting_your_own.php">Starting Your Own Bot</a></li>
  <li><a href="simple_strategy_guide.php">Climbing the Rankings</a>:
    make some simple improvements to the starter package to climb the
    rankings.</li>
  <li><a href="resources.php">More Articles and Tutorials</a></li>
  <li><a href="forums/">Forums</a> (See especially the Strategy section)</li>
</ul>
<?php include 'footer.php'; ?>

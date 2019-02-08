<?php

$title = "Ants Tutorial";
require_once('header.php');

?>

<!--<MarkdownReplacement with="competition-Tutorial.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h2 id="ants-tutorial">Ants Tutorial</h2>
<div class="toc">

*   <a href="ants_tutorial.php">Setting Up</a>
*   <a href="ants_tutorial_step_1.php">Step 1: Avoiding Collisions</a>
*   <a href="ants_tutorial_step_2.php">Step 2: Gathering Food</a>
*   <a href="ants_tutorial_step_3.php">Step 3: Not Blocking Hills</a>
*   <a href="ants_tutorial_step_4.php">Step 4: Explore the Map</a>
*   <a href="ants_tutorial_step_5.php">Step 5: Attack the Enemy Hills</a>

</div>

<p>The strategies implemented in the starter packages are only meant to serve as a starting point for making your bot.  In fact, it's almost the worst strategy to use.  The starter packages also come with some useful functions to help you develop a smarter strategy.  This page will walk you through a series of improvements.  With each step that you complete, your bot will get smarter and you should notice your ranking start to rise.</p>
<h3 id="prerequisites">Prerequisites</h3>
<p>For this tutorial, we will be using the Python starter package.  In order to use python, you must have a python interpreter downloaded and installed on your machine.  See <a href="getting_started_with_python.php">Getting Started with Python</a>.</p>
<p><strong>Note: The tools come with the game engine written in python.  You will need to install a python interpreter to run the game engine regardless of which language you are programming in.</strong></p>
<p>You'll also want to download the <a href="using_the_tools.php">tools</a> and install them on your machine.</p>
<h3 id="setting-up">Setting Up</h3>
<p>Create a folder to put both the tools and the starter bot and unzip both files in that location.  You should have something that looks like this:</p>
<div class="codehilite"><pre><span></span><span class="nt">C</span><span class="o">:</span><span class="err">\</span><span class="nt">aichallenge</span><span class="o">&amp;</span><span class="nt">gt</span><span class="o">;</span><span class="nt">tree</span>
<span class="nt">Folder</span> <span class="nt">PATH</span> <span class="nt">listing</span>
<span class="nt">C</span><span class="o">:.</span>
<span class="o">+</span><span class="nt">----tools</span>
    <span class="o">+</span><span class="nt">---mapgen</span>
    <span class="o">+</span><span class="nt">---maps</span>
    <span class="o">|</span>   <span class="o">+</span><span class="nt">---example</span>
    <span class="o">|</span>   <span class="o">+</span><span class="nt">---maze</span>
    <span class="o">|</span>   <span class="o">+</span><span class="nt">---multi_hill_maze</span>
    <span class="o">|</span>   <span class="o">+</span><span class="nt">---symmetric_random_walk</span>
    <span class="o">+</span><span class="nt">---sample_bots</span>
    <span class="o">|</span>   <span class="o">+</span><span class="nt">---csharp</span>
    <span class="o">|</span>   <span class="o">+</span><span class="nt">---java</span>
    <span class="o">|</span>   <span class="o">+</span><span class="nt">---php</span>
    <span class="o">|</span>   <span class="o">|</span>   <span class="o">+</span><span class="nt">---tests</span>
    <span class="o">|</span>   <span class="o">+</span><span class="nt">---python</span>
    <span class="o">+</span><span class="nt">---submission_test</span>
    <span class="o">+</span><span class="nt">---visualizer</span>
        <span class="o">+</span><span class="nt">---data</span>
        <span class="o">|</span>   <span class="o">+</span><span class="nt">---img</span>
        <span class="o">+</span><span class="nt">---js</span>
</pre></div>


<div class="tab_sync">    
<div class="tab_content" title="Python">

    C:\aichallenge>dir /b
    ants.py
    MyBot.py
    tools

</div>
<div class="tab_content" title="Java">

    C:\aichallenge>dir /b
    AbstractSystemInputParser.java
    AbstractSystemInputReader.java
    Aim.java
    Ants.java
    Bot.java
    Ilk.java
    make.cmd
    Makefile
    Manifest.txt
    MyBot.java
    Order.java
    Tile.java
    tools

</div>
</div>

<h3 id="testing">Testing</h3>
<p>Now lets make sure everything is working by running a test game.  The tools comes with a utility called "playgame.py" that will help up test our bot.  It also comes with an example script called "play_one_game.cmd" to show you how to use it.</p>
<div class="codehilite"><pre><span></span><span class="nt">C</span><span class="o">:</span><span class="err">\</span><span class="nt">aichallenge</span><span class="o">&amp;</span><span class="nt">gt</span><span class="o">;</span><span class="nt">tools</span><span class="err">\</span><span class="nt">play_one_game</span><span class="p">.</span><span class="nc">cmd</span>
<span class="nt">running</span> <span class="nt">for</span> <span class="nt">500</span> <span class="nt">turns</span>
                     <span class="nt">ant_count</span>    <span class="nt">c_turns</span>    <span class="nt">climb</span><span class="o">?</span>    <span class="nt">cutoff</span> <span class="nt">food</span> <span class="nt">r_turn</span> <span class="nt">ranking_bots</span>   <span class="nt">s_alive</span>      <span class="nt">s_hills</span>       <span class="nt">score</span>     <span class="nt">w_turn</span> <span class="nt">winning</span>
<span class="nt">turn</span>    <span class="nt">0</span> <span class="nt">stats</span><span class="o">:</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>    <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">-</span>     <span class="nt">20</span>    <span class="nt">0</span>        <span class="nt">None</span>     <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span> <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span> <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">0</span>     <span class="nt">None</span>
<span class="nt">turn</span>    <span class="nt">1</span> <span class="nt">stats</span><span class="o">:</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>    <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">-</span>     <span class="nt">20</span>    <span class="nt">1</span>     <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span> <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span> <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">1</span>    <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">2</span><span class="p">,</span><span class="mi">3</span><span class="cp">]</span>
<span class="nt">turn</span>    <span class="nt">2</span> <span class="nt">stats</span><span class="o">:</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>    <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">-</span>     <span class="nt">24</span>    <span class="nt">1</span>     <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span> <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span> <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">1</span>    <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">2</span><span class="p">,</span><span class="mi">3</span><span class="cp">]</span>
<span class="nt">turn</span>    <span class="nt">3</span> <span class="nt">stats</span><span class="o">:</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>    <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">-</span>     <span class="nt">24</span>    <span class="nt">1</span>     <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span> <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span> <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">1</span>    <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">2</span><span class="p">,</span><span class="mi">3</span><span class="cp">]</span>
<span class="nt">turn</span>    <span class="nt">4</span> <span class="nt">stats</span><span class="o">:</span>  <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>    <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">-</span>     <span class="nt">22</span>    <span class="nt">1</span>     <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span> <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span> <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">1</span>    <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">2</span><span class="p">,</span><span class="mi">3</span><span class="cp">]</span>
<span class="nt">turn</span>    <span class="nt">5</span> <span class="nt">stats</span><span class="o">:</span>  <span class="cp">[</span><span class="mi">2</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">2</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>    <span class="nt">0</span>    <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">-</span>     <span class="nt">22</span>    <span class="nt">1</span>     <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="p">,</span><span class="mi">0</span><span class="cp">]</span>   <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span> <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span> <span class="cp">[</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">1</span><span class="cp">]</span>   <span class="nt">1</span>    <span class="cp">[</span><span class="mi">0</span><span class="p">,</span><span class="mi">1</span><span class="p">,</span><span class="mi">2</span><span class="p">,</span><span class="mi">3</span><span class="cp">]</span>
<span class="o">...</span>
</pre></div>


<p>If you saw the preceding output, then everything should be working.</p>
<h3 id="create-test-script">Create Test Script</h3>
<p>Now, let's create our own script for this tutorial that uses a new map and our own bot.</p>
<div class="tab_sync">
<div class="tab_content" title="Windows">

Create a file called "tutorial.cmd".

    :::text
    C:\aichallenge>notepad tutorial.cmd

</div>
<div class="tab_content" title="Linux">

Create a file called "tutorial.sh".

    :::text
    user@localhost:~$ gedit tutorial.sh

After editing the file, make it executable:

    :::text
    user@localhost:~$ chmod u+x tutorial.sh

</div>
</div>

<p>The contents of the text file will be:</p>
<div class="tab_sync">
<div class="tab_content" title="Python">

    :::text
    python tools/playgame.py "python MyBot.py" "python tools/sample_bots/python/HunterBot.py" --map_file tools/maps/example/tutorial1.map --log_dir game_logs --turns 60 --scenario --food none --player_seed 7 --verbose -e

</div>
<div class="tab_content" title="Java">

    :::text
    python tools/playgame.py "java -jar MyBot.jar" "python tools/sample_bots/python/HunterBot.py" --map_file tools/maps/example/tutorial1.map --log_dir game_logs --turns 60 --scenario --food none --player_seed 7 --verbose -e


The java bot needs to be compiled into a jar file for us to use.  You can run the following command to create the file:

    make

</div>
</div>

<ul>
<li>The first 2 options are the names of the bots to run.  We'll be using HunterBot as our opponent.</li>
<li>The <code>--map_file</code> options specifies the map to use.  This is a simple map with 2 ant hills.</li>
<li>The <code>--log_dir</code> options specifies a location to store the replay file, and optionally the logs of bot input and output and errors.</li>
<li>The <code>--turns</code> options specifies when to stop the game if it goes too long.  We don't want a lot of extra output, so will keep it to 60 turns.</li>
<li>The <code>--scenario</code> option allows us to use the food specified on the map as the starting food.  It has been specially placed for this tutorial. (remove this for real games)</li>
<li>The <code>--food none</code> option allows us to turn off the food spawning during the game.  Again, it will be off just for this tutorial. (remove this for real games)</li>
<li>The <code>--player_seed</code> option ensures that you can get the same game results as in the tutorial.  HunterBot will use this value to initialize the random number generator, so it will always do the same thing.  <em>(Note: if you want your bot to be able to replay games for debugging, you'll want to implement this as well.)</em></li>
<li>The <code>--verbose</code> option will print a running total of game stats so we can watch the progress as the game is played.</li>
<li>The <code>-e</code> option will output any bot errors to the console, so if you make a mistake during the tutorial you can see what the error message is.
<em>(Note: remove the <code>--scenario</code> and <code>--food none</code>  options when you want to play games on different maps.)
</em>(Note: the tutorial was made with view radius 55, which is not the official view radius.  You can add <code>--viewradius 55</code> to the engine to override the default and make the tutorial bot behave the same as the same replays.)
(Both HunterBot and the tutorial bot we will be making are deterministic, meaning if you give them the same input, they will produce the same set of moves.  This means if you follow along exactly, you should see the tutorial games play out exactly the same on your machine.  The python code that sorts lists will sort every element, the java code wasn't implemented to sort correctly, so results may differ.)</li>
</ul>
<p>Let's run the command and see how the starter bot does.</p>
<div class="tab_sync">
<div class="tab_content" title="Windows">

    :::text
    C:\aichallenge>tutorial

</div>
<div class="tab_content" title="Linux">

    :::text
    user@localhost:~$ ./tutorial.sh

</div>
</div>

<p>You should see the following output:</p>
<div class="codehilite"><pre><span></span>    running for 60 turns
                      ant_count c_turns climb? cutoff food r_turn ranking_bots s_alive s_hills score  w_turn winning
    turn    0 stats:   [1,1,0]     0    [1,1]    -     18    0        None      [1,1]   [1,1]  [1,1]   0     None
    turn    1 stats:   [1,1,0]     0    [1,1]    -     16    1       [0,0]      [1,1]   [1,1]  [1,1]   1     [0,1]
    turn    2 stats:   [2,1,0]     0    [1,1]    -     16    1       [0,0]      [1,1]   [1,1]  [1,1]   1     [0,1]
    turn    3 stats:   [2,2,0]     0    [1,1]    -     15    1       [0,0]      [1,1]   [1,1]  [1,1]   1     [0,1]
    turn    4 stats:   [2,3,0]     0    [1,1]    -     14    1       [0,0]      [1,1]   [1,1]  [1,1]   1     [0,1]
    turn    5 stats:   [2,4,0]     0    [1,1]    -     14    1       [0,0]      [1,1]   [1,1]  [1,1]   1     [0,1]
    turn    6 stats:   [2,4,0]     0    [1,1]    -     14    1       [0,0]      [1,1]   [1,1]  [1,1]   1     [0,1]
    turn    7 stats:   [2,4,0]     0    [1,1]    -     14    1       [0,0]      [1,1]   [1,1]  [1,1]   1     [0,1]
    turn    8 bot 0 eliminated
    turn    8 stats:   [0,4,0]     0    [0,1]    -     14    1       [0,0]      [0,1]   [1,1]  [1,1]   1     [0,1]
    score 1 3
    status eliminated survived
    playerturns 8 8
</pre></div>


<p>The game only ran for 8 turns, which is very fast.  It looks like player 0 (that's us) got eliminated.  A browser should have launched to show you the game in the visualizer.</p>
<div class="codehilite"><pre><span></span># [ { &quot;embedded&quot;: true }, 600, 600, { &quot;speedFactor&quot;: 0, &quot;speedFastest&quot;: 2, &quot;speedSlowest&quot;: 2, &quot;zoom&quot;: 1 }, &quot;example_games/tutorial.0.replay&quot; ]
</pre></div>


<p>You can see the starter bot's strategy is so horrible, it kills itself by colliding 2 ants.  That will be our first improvement.</p>
<h3 id="next">Next</h3>
<p>Here's the list of improvements we'll be implementing in this tutorial:</p>
<ol>
<li><a href="ants_tutorial_step_1.php">Step 1: Avoiding Collisions</a></li>
<li><a href="ants_tutorial_step_2.php">Step 2: Gathering Food</a></li>
<li><a href="ants_tutorial_step_3.php">Step 3: Not Blocking Hills</a></li>
<li><a href="ants_tutorial_step_4.php">Step 4: Explore the Map</a></li>
<li><a href="ants_tutorial_step_5.php">Step 5: Attack the Enemy Hills</a></li>
</ol>
<!--</MarkdownReplacement>-->

<?php

require_once('visualizer_widget.php');
visualize_pre();
require_once('footer.php');

?>
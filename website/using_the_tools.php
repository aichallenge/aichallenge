<?php

$title="Using the Tools";
require_once('header.php');
require_once('visualizer_widget.php');
visualize_pre();

?>

<!--<MarkdownReplacement with="Using-the-Tools.md">--><style>img.latex-inline { vertical-align: middle; }</style>
<h1 id="using-the-tools">Using the Tools</h1>
<p>So you completed the <a href="/quickstart.php">Ants Five Minute Quickstart Guide</a> , and you see your name on the leaderboard. Now what? In this guide, you will use the Ants game visualizer to watch two of the sample bots play against each other.</p>
<p>If you have any trouble with this tutorial, please complain on the <a href="http://forums.aichallenge.org/">forums</a> so that we know to fix it. We'll do whatever it takes to get you off the ground!</p>
<h2 id="getting-the-tools">Getting the Tools</h2>
<p>Download the tools for <a href="/tools.zip">Windows</a> or <a href="/tools.tar.bz2">Linux/MacOS</a>. And if you haven't done so already, download a <a href="/starter_packages.php">starter package</a>.
Unzip the packages in a location of your choice. Since we will be using a command-line terminal in this tutorial, it is better to choose a location that is easy to find using a terminal. The tools also need python 2.6+ in order to run. On Linux or MacOS an appropriate version is probably already installed. On Windows you will probably need to install <a href="http://python.org/download/releases/2.7.2/">Python</a>.</p>
<p><strong>Note: The tools come with the game engine written in python.  You will need to install a python interpreter to run the game engine regardless of which language you are programming in.</strong></p>
<h2 id="watch-bots-fight">Watch Bots Fight</h2>
<p>You can watch a set of bots fight by executing <code>play_one_game.cmd</code> for Windows, or <code>play_one_game.sh</code> for Unix based systems.  (For Unix issue <code>./play_one_game.sh</code> since it will probably not be in your $PATH.)</p>
<p>The above script plays a game between 4 sample bots. It will take some time to run the game (about 30 seconds). Once the game is complete, the visualizer window pops up to play back the game graphically. If you don't want to wait you can also get a live preview of that game as it is being run by calling the script like this: <code>play_one_game.[cmd/sh] -So | java -jar visualizer.jar</code>. Java 6 or higher required.</p>
<p>In the playback you just saw, player 1 and 3 are called Hunterbot, and player 2 and 4 are called LeftyBot.  There are other sample strategies that come with the tools package, you can find them all in the <code>sample_bots/</code> folder.</p>
<p>At this point you'll probably want to test your own bot in a match. First you'll have to know how to run the bot. C++ bots and other compiled bots usually run when their filenames are typed: <code>./botexecutable</code> for Unix and <code>botexecutable.exe</code> for Windows. Interpreted or bots that need a runtime usually need a program to assist them <code>java MyBot.class</code> or <code>python MyBot.py</code>. When executing this in the terminal you will run the bot directly, it will expect to get game commands from you; this is a good way to test your bot against startup crashes (See test section later on). Once you have the command to start your bot you can either pass it as an argument to <code>play_one_game</code>/<code>test_bot</code> or you could edit play_one_game to include your bot in every match by appending the command in quotes at the end of the last line in the file.</p>
<h2 id="testing-your-bot">Testing your bot</h2>
<p>Before submitting your bot to the main website, please make sure that your bot does not crash. Bots can be tested using the <code>test_bot.sh</code> or <code>test_bot.cmd</code> script. Just pass the command to run your bot to it and it'll test the bot. This is the same test the server runs after you submit your bot to make sure it compiled right.</p>
<h2 id="submit-your-code-online">Submit Your Code Online</h2>
<p>When you submit your code on the AI Challenge website, it automatically starts playing against bots submitted by other people from around the world. Within a few minutes, your name will show up in the global rankings. You can submit your bot many times before the deadline.</p>
<p>Before submitting your code, you must create a .zip file that contains all your code. It's recommended that you only put code files into this zip file. On Windows, open the folder where your code is, hold the Ctrl key, then click all the .java files. Once all the .java files are selected, you can put them into a .zip file by right-clicking one of them, and from the context menu selecting "Send To" then "Compressed File". Your main code file MUST be called MyBot.java (or MyBot.cc, MyBot.py, etc).</p>
<p>To submit your code online, make sure that your account is activated and you are signed in to the website, then visit the <a href="/submit.php">Upload Your Code</a> page.</p>
<h2 id="next-steps">Next Steps</h2>
<p>Just by making this one little change to the starter package, you should notice that your ranking will start to improve over the next hour or so. But it doesn't end here! In the next tutorial, you will make a series of simple changes that should give you another big bump. Soon you will be well on your way to the top of the leaderboard!</p>
<!--</MarkdownReplacement>-->

<?php

require_once('footer.php');

?>

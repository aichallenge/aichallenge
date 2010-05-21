<?php include 'header.php'; ?>

<h2>Getting Off the Ground</h2>
<p>So you completed the <a href="quickstart.php">Five-Minute Quickstart Guide
  </a>, and you see your name on the leaderboard. Now what? In this guide, you
  will learn about some of the cool things you can do with the starter package
  you just downloaded.</p>
<p>We'll show you how to run a small tournament of the sample strategies that
  come built in to the starter packages. You'll be able to watch them play
  against one another. Next, we'll show you how to compile and run your own
  entry. After that, you'll be able to watch your own entry play against the
  sample strategies, to test it out. Finally, we'll show you how to zip up
  and submit your code on the website, so you can start to fight your way to
  the top of the Global Leaderboard.</p>
<p>If you have any trouble with this tutorial, please complain on the
  <a href="http://ai-contest.com/forums">forums</a> so that we know to fix it.
  We'll do whatever it takes to get you off the ground!</p>

<a name="toc"><h3>Table of Contents</h3></a>
<ul>
  <li><a href="#setup">Getting Set Up</a></li>
  <li><a href="#example">Watch Two Example Bots Play Tron</a></li>
  <li><a href="#compile">Compile Your Own Entry</a></li>
  <li><a href="#entry">Watch Your Entry Play Against an Example Bot</a></li>
  <li><a href="#nextsteps">Next Steps</a></li>
</ul>

<a name="setup"><h3>Getting Set Up</h3></a>
<p>There are a couple things you need to set up before getting to the fun
  stuff in this tutorial.</p>
<ul>
  <li>Download and install the
    <a href="http://java.sun.com/javase/downloads/index.jsp">Java JDK</a>
    (even if you don't know Java, or don't intend on writing your entry in
    Java)</li>
  <li>Complete the <a href="quickstart.php">Five-Minute Quickstart Guide</a>
    </li>
  <li>Visit the <a href="http://ai-contest.com/forums">forums</a>. This is
    where you go to report any problems with this tutorial. We will do
    whatever it takes to get you off the ground!</li>
</ul>

<a name="example"><h3>Watch Two Example Bots Play Tron</h3></a>
<p>Unzip the starter package to a location of your choice, then navigate to
  the folder that is produced. I unzipped my C++ starter package in "C:\
  Documents and Settings\Jeff\GoogleAiChallenge\", which produced a folder
  called "cpp". To get where I need to be, I open a DOS Prompt and run the
  following two commands.</p>
<code>
cd "C:\Documents and Settings\Jeff\GoogleAiChallenge\"<br>
cd "cpp"
</code>
<p>You'll have to modify these commands depending on where you unzipped your
  starter package, and which particular starter package you are using.</p>
<p>Once you have a command prompt open in the starter package directory, you
  can now invoke the Tron game simulator to play two of the sample strategies
  against each other. Try the following command.</p>
<code>
java -jar engine/Tron.jar maps/u.txt "java -jar example_bots/Chaser.jar" "java -jar example_bots/RunAway.jar"
</code>
<p>Cool huh?! The Chaser bot chases its opponent, and the RunAway bot just
  tries to hide in its corner and stay away from its opponent. Try running
  two other bots against each other on a different map.</p>
<code>
java -jar engine/Tron.jar maps/toronto.txt "java -jar example_bots/RandomBot.jar" "java -jar example_bots/WallHugger.jar"
</code>
<p>Whoa AWESOME!!1! RandomBot wanders aimlessly, and WallHugger just tries to
  stay out of RandomBot's drunken path by staying in the shadows.</p>

<a name="compile"><h3>Compile Your Own Entry</h3></a>
<p>If you're using Java or C++, then this stage is for you. If you're using
  Python, Perl, Ruby, Scheme, or some other interpreted language, then you
  can skip this step, since those languages don't need to be compiled.</p>
<p>Open a DOS Prompt and navigate to the root directory of the starter package,
  just like you did in the last stage. Whatever language you chose, the
  starter package contains already-working code, which is ready to be compiled.
  </p>
<p>If you are using C++ on Windows, we recommend you download and install
  Visual C++ Express Edition or Dev-C++. Create a new C++ project, and add the
  files <i>MyTronBot.cc</i>, <i>Map.h</i>, and <i>Map.cc</i> to your new
  project. Then compile the project. If you have some problems, please post on
  the <a href="http://ai-contest.com/forums">forums!</a></p>
<p>If you are using C++ on Linux, things are a little easier. Just run the
  following command to compile the C++ starter package.</p>
<code>
make
</code>
<p>If you are using java on either Windows or Linux, run the following command
  to compile your starter package.</p>
<code>
javac *.java
</code>
<p>If you are having any trouble at all, please post on the
  <a href="http://ai-contest.com/forums">forums</a>!</p>

<a name="entry"><h3>Watch Your Entry Play Against an Example Bot</h3></a>
<p>If you are using C++ on Windows, hunt around in the project directory
  until you find <i>MyTronBot.exe</i>. Copy this file to the root directory
  of the starter package so that we can watch it play against RandomBot by
  running the following command.</p>
<code>
java -jar engine/Tron.jar maps/empty-room.txt "MyTronBot.exe" "java -jar example_bots/RandomBot.jar"
</code>
<p>If you are using C++ on Linux, the following command will run your bot
  against RandomBot.</p>
<code>
java -jar engine/Tron.jar maps/empty-room.txt "./MyTronBot" "java -jar example_bots/RandomBot.jar"
</code>
<p>If you're using Java on Windows or Linux, use the following command to run
  your bot against RandomBot.</p>
<code>
java -jar engine/Tron.jar maps/empty-room.txt "java MyTronBot" "java -jar example_bots/RandomBot.jar"
</code>
<p>If you're using Python on Windows or Linux, use the following command to run
  your bot against RandomBot.</p>
<code>
java -jar engine/Tron.jar maps/empty-room.txt "python MyTronBot.py" "java -jar example_bots/RandomBot.jar"
</code>
<p>If you're using Ruby on Windows or Linux, use the following command to run
  your bot against RandomBot.</p>
<code>
java -jar engine/Tron.jar maps/empty-room.txt "ruby MyTronBot.rb" "java -jar example_bots/RandomBot.jar"
</code>
<p>If you're using Perl on Windows or Linux, use the following command to run
  your bot against RandomBot.</p>
<code>
java -jar engine/Tron.jar maps/empty-room.txt "perl MyTronBot.pl" "java -jar example_bots/RandomBot.jar"
</code>

<a name="nextsteps"><h3>Next Steps</h3></a>
<p>Once you've learned how to compile your bot and watch it play against the
  sample strategies, it's time to really get started implementing your own
  custom strategy. Here are some places you can look for ideas.</p>
<ul>
  <li><a href="strategy.php">Robert Xiao's Strategy Guide</a></li>
  <li><a href="http://ai-contest.com/forums">Forums</a> (See especially the Strategy section)</li>
</ul>

<?php include 'footer.php'; ?>

<?php include 'header.php'; ?>
<h2>Starting Your Own Bot</h2>
<p>In this guide, you will learn how to compile and run the default bot that
  comes with the starter package. You will actually be able to watch it play
  against itself, or against any one of the sample bots. You will also make a
  quick and easy change to the bot's code that will improve its skill. Finally,
  you will learn how to package up your modified bot and submit it to the
  online tournament. If your goal is to climb the online rankings, then you
  are in the right place.</p>
<p>If you have any trouble with this tutorial, please complain on the
  <a href="forums/">forums</a> so that we know to fix it. We'll do whatever
  it takes to get you off the ground!</p>
<h3>Prerequisites</h3>
<p>For this tutorial, we will be using the Java starter package. In order to
  compile Java code, you must download and install the
  <a href="http://www.google.com/search?sourceid=chrome&ie=UTF-8&q=java+jdk">
  Java JDK</a>, if you don't have it already.</p>
<p>If you haven't already, you should also look through the
  <a href="using_the_tools.php">Using the Tools</a> article before starting
  this one.</p>
<h3>Getting a Starter Package</h3>
<p>If you haven't done so already, download the <a href="starter_packages.php">
  Java starter package</a>. Unzip the starter package in a location of your
  choice. Since we will be using a command-line terminal in this tutorial,
  it is better to choose a location that is easy to find using a terminal.
  I chose to unzip my starter package in C:\planetwars\. This creates a
  new folder C:\planetwars\JavaStarterPackage\ which contains all the
  materials that we need for this tutorial.</p>
<p>If you don't want to use Java, that's fine. You can always download a
  different starter package later. For this tutorial, we're going to be using
  the Java starter package.</p>
<h3>Opening a Command Prompt</h3>
<p>In this tutorial, you will use a command prompt (also known as a terminal)
  to issue commands. Don't worry, it's really easy! If you're using a Windows
  machine, open the Run dialog, type "cmd", then click the Run button. A black
  window containing white text and a flashing cursor should pop up.</p>
<p>Once you have a command prompt open, you also have to point it at the
  location where you unzipped the Java starter package. Since I unzipped my
  starter package in C:\planetwars\, I will type the following command. If you
  unzipped your starter package in a different location, modify the command
  accordingly.</p>
<p class="code">cd C:\planetwars\JavaStarterPackage</p>
<h3>Compiling the Java Starter Package</h3>
<p>The Java starter package contains a simple working bot that you can use as
  a starting point. To compile the starter package, use the following command.
  Remember that you must have the terminal pointed at the correct directory,
  otherwise the Java compiler won't know where to find the code files.</p>
<p class="code">javac *.java</p>
<p>You'll know that the Java code files got compiled correctly if you see the
  file <em>MyBot.class</em> was created.</p>
<h3>Watching Your Bot Play Against Itself</h3>
<p>If the compile goes smoothly, then you can watch your bot play against
  itself using the following command.</p>
<p class="code">java -jar tools/PlayGame.jar maps/map7.txt 1000 1000 log.txt
  "java MyBot" "java MyBot" | java -jar tools/ShowGame.jar</p>
<p>You can also watch your bot play against one of the sample strategy using
  the following command.</p>
<p class="code">java -jar tools/PlayGame.jar maps/map7.txt 1000 1000 log.txt
  "java MyBot" "java -jar example_bots/RandomBot.jar" | java -jar
  tools/ShowGame.jar</p>
<h3>Make an Easy Improvement to Your Bot</h3>
<p>Open the file MyBot.java. This is the code file that you will be working on
  to improve your bot's strategy. Inside, you will see that there is some
  sample code already in this file. Right near the top of the file, you'll see
  the following lines of code.</p>
<p class="code">// (1) If we current have a fleet in flight, just do nothing.
  <br />if (pw.MyFleets().size() >= 1) {<br />&nbsp;&nbsp;&nbsp;&nbsp;return;
  <br />}</p>
<p>These lines are what stop your bot from sending all of its ships at once.
  The number that appears in this code is the maximum number of fleets that
  your bot can have in flight at any given time. By increasing this number,
  you can make your bot more aggressive. Change the number from 1 to 2, so the
  code looks like this:
<p class="code">// (1) If we current have a fleet in flight, just do nothing.
  <br />if (pw.MyFleets().size() >= 2) {<br />&nbsp;&nbsp;&nbsp;&nbsp;return;
  <br />}</p>
<p>Save the file and close it. Back in the terminal, recompile your code using
  the following command.</p>
<p class="code">javac *.java</p>
<p>Assuming the compile goes smoothly, run your bot against itself again. Use
  the following command.</p>
<p class="code">java -jar tools/PlayGame.jar maps/map7.txt 1000 1000 log.txt
  "java MyBot" "java MyBot" | java -jar tools/ShowGame.jar</p>
<p>Notice that your bot now generally keeps two fleets in the air at once,
  instead of only one. You have made your bot more aggressive. You are now an
  AI programmer. Cool, huh? Show your friends!</p>
<p>You can experiment with different numbers. Try 3 or 4 and see how it does.
  You can even experiment with keeping different versions of your bot, so that
  you can play them against each other to see which is better. Try
  experimenting on different maps, too.</p>
<p>If you're really loving tinkering with the code and making simple
  improvements, check out the <a href="simple_strategy_guide.php">Simple
  Strategy Guide</a> to see a whole series of small incremental improvements
  that can be made to the starter package in order to climb up the rankings.
  </p>
<h3>Submit Your Code Online</h3>
<p>When you submit your code on the Google AI Challenge website, it
  automatically starts playing against bots submitted by other people from
  around the world. Within an hour, your name will show up in the global
  rankings. You can submit your code as often as you like. There is no limit.
  </p>
<p>Before submitting your code, you must create a .zip file that contains all
  your code. It's recommended that you only put code files into this zip file.
  On Windows, open the folder where your code is, hold the Ctrl key, then click
  all the .java files. Once all the .java files are selected, you can put them
  into a .zip file by right-clicking one of them, and from the context menu
  selecting "Send To" then "Compressed File". Your main code file MUST be
  called MyBot.java (or MyBot.cc, MyBot.py, etc).</p>
<p>To submit your code online, make sure that your account is activated and you
  are signed in to the website, then visit the <a href="submit.php">Upload
  Your Code</a> page.</p>
<h3>Next Steps</h3>
<p>Just by making this one little change to the starter package, you should
  notice that your ranking will start to improve over the next hour or so.
  But it doesn't end here! In the next tutorial, you will make a series of
  simple changes that should give you another big bump. Soon you will be well
  on your way to the top of the leaderboard!</p>
<ul>
  <li><a href="simple_strategy_guide.php">Climbing the Rankings</a>:
    make some simple improvements to the starter package to climb the
    rankings.</li>
  <li><a href="resources.php">More Articles and Tutorials</a></li>
  <li><a href="forums/">Forums</a> (See especially the Strategy section)</li>
</ul>
<?php include 'footer.php'; ?>

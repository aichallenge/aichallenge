<?php include "header.php"; ?>
<h2>Climbing the Rankings</h2>
<p>If you want to climb the rankings, then you're in the right place. This
  guide will walk you through a series of small improvements to the Java
  starter package. With each step that you complete, your bot will get
  smarter, and you should notice your ranking start to rise.</p>
<p>When you have completed this guide, you will have a smart bot which is able
  to identify and exploit its opponent's mistakes. Your bot will be aware of
  its environment, and will adapt its behavior to different situations.</p>
<p>Excited? Let's do it!</p>
<h3>Prerequisites</h3>
<p>For this tutorial, we will be using the <a href="starter_packages.php">
  Java starter package</a>. In order to compile Java code, you must download
  and install the
  <a href="http://www.google.com/search?sourceid=chrome&ie=UTF-8&q=java+jdk">
  Java JDK</a>, if you don't have it already.</p>
<p>If you haven't already, you should also look through the
  <a href="using_the_tools.php">Using the Tools</a> and
  <a href="starting_your_own.php">Starting Your Own Bot</a> articles before
  starting this one.</p>
<h3>What We've Got: the Default Java Starter Package's Strategy</h3>
<p>The strategy that comes with the Java starter package is not bad, but it
  is quite simple. It sends half the ships from its strongest planet to the
  weakest planet that it does not yet own. The idea behind this strategy is to
  buy new planets as cheaply as possible. The neat thing about this strategy is
  that it's only a few short lines of code. Open up MyBot.java to have a look.
  </p>
<p>While the default strategy is not horrible, it definitely comes up short in
  a few respects. In this guide, we will add the following enhancements:</p>
<ul>
  <li>The bot is not aggressive enough. Because it only allows one fleet at
    a time, it tends to accumulate hundreds of ships that just sit uselessly
    on planets. These ships could be put to use instead of sitting around.
    We will fix this by increasing the number of fleets that the bot sends out
    at a time.</li>
  <li>Larger planets produce ships more quickly, but the bot does not take this
    into account. The bot gives equal consideration to small planets and big
    planets. We will fix this by attaching greater weight to larger planets
    when the bot is calculating which planet to attack.</li>
  <li>The bot does not adapt its strategy based on whether it's winning or
    losing. Experience shows that when you're winning at Planet Wars, you
    should play it safe, and be defensive. When you're losing, it is wise to
    be more agressive, to try to take the lead.</li>
</ul>
<h3>Make Your Bot More Aggressive</h3>
<p>This step was already covered in the <a href="starting_your_own.php">
  Starting Your Own Bot article</a>, but we will cover it here as well. Open
  the file MyBot.java. Near the very top of the file, you will see the
  following code.</p>
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
<p>Once you save the change to MyBot.java, recompile your bot:</p>
<p class="code">javac *.java</p>
<p>Play your new, more agressive bot against BullyBot so you can see the
  changes in action. Use the following command.</p>
<p class="code">java -jar tools/PlayGame.jar maps/map7.txt 1000 1000 log.txt
  "java MyBot" "java -jar example_bots/BullyBot.jar" | java -jar
  tools/ShowGame.jar</p>
<p>Do you see the difference? BullyBot only sends one fleet at a time, whereas
  your bot sends two at a time. Try sending three at a time, or even four at
  a time. See if you can figure out what the optimal agressiveness level is.
  Remember that you need to save AND recompile your code every time you make
  a change in order to see the effects.</p>
<h3>Take Planet Size into Account</h3>
<p>Right now, your bot does not take planet size into account. Ideally, the bot
  would have some idea that larger planets are more valuable. Your bot should
  be more eager to attack a large planet than a small planet, and more
  reluctant to leave a large planet than a small planet.</p>
<p>Within MyBot.java, look for the code that looks like this:</p>
<p class="code">// (2) Find my strongest planet.<br />
  Planet source = null;<br />
  double sourceScore = Double.MIN_VALUE;<br />
  for (Planet p : pw.MyPlanets()) {<br />
    &nbsp;&nbsp;&nbsp;&nbsp;double score = (double)p.NumShips();<br />
    &nbsp;&nbsp;&nbsp;&nbsp;if (score > sourceScore) {<br />
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;sourceScore = score;<br/>
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;source = p;<br />
    &nbsp;&nbsp;&nbsp;&nbsp;}<br />
  }<br />
  // (3) Find the weakest enemy or neutral planet.<br />
  Planet dest = null;<br />
  double destScore = Double.MIN_VALUE;<br />
  for (Planet p : pw.NotMyPlanets()) {<br />
    &nbsp;&nbsp;&nbsp;&nbsp;double score = 1.0 / (1 + p.NumShips());<br />
    &nbsp;&nbsp;&nbsp;&nbsp;if (score > destScore) {<br />
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;destScore = score;<br />
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dest = p;<br />
    &nbsp;&nbsp;&nbsp;&nbsp;}<br />
  }</p>
<p>Change it so that it looks like what follows. Changes are in <b>bold</b>.
  </p>
<p class="code">// (2) Find my strongest planet.<br />
  Planet source = null;<br />
  double sourceScore = Double.MIN_VALUE;<br />
  for (Planet p : pw.MyPlanets()) {<br />
    &nbsp;&nbsp;&nbsp;&nbsp;double score = <b>(double)p.NumShips() /
      (1 + p.GrowthRate());</b><br />
    &nbsp;&nbsp;&nbsp;&nbsp;if (score > sourceScore) {<br />
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;sourceScore = score;<br />
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;source = p;<br />
    &nbsp;&nbsp;&nbsp;&nbsp;}<br />
  }<br />
  // (3) Find the weakest enemy or neutral planet.<br />
  Planet dest = null;<br />
  double destScore = Double.MIN_VALUE;<br />
  for (Planet p : pw.NotMyPlanets()) {<br />
    &nbsp;&nbsp;&nbsp;&nbsp;double score = <b>(double)(1 + p.GrowthRate()) /
      p.NumShips();</b><br />
    &nbsp;&nbsp;&nbsp;&nbsp;if (score > destScore) {<br />
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;destScore = score;<br />
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dest = p;<br />
    &nbsp;&nbsp;&nbsp;&nbsp;}<br />
  }</p>
<p>All we've done here is made a slight change to the scoring formulas that are
  used to choose which planet to attack, and which planet to attack from.</p>
<p>Save the changes, compile your code, then play your bot against BullyBot
  again. Can you see the difference? Your bot should show a slight preference
  for large planets. If the game goes on long enough, you'll notice after a
  while that your bot is occupying all the large planets, while BullyBot has
  been confined to all the little planets. Cool!</p>
<h3>Anything to Get Ahead</h3>
<p>Your bot is getting smarter, but it still doesn't have any concept of how
  well it's doing. What we want to do is vary the agressiveness level of the
  bot depending on how well it's doing.</p>
<p>We have seen that we can easily tweak the agressiveness of our bot by
  changing the number of fleets that it can have in flight at any given time.
  A decent strategy would be to use 1 fleet at a time if we're ahead, and 3
  fleets at a time if we're behind.</p>
<p>Look for the bit of code in MyBot.java that looks like this:</p>
<p class="code">
  if (pw.MyFleets().size() >= 2) {<br />
  &nbsp;&nbsp;&nbsp;&nbsp;return;<br />
  }</p>
<p>and change it to this:</p>
<p class="code">
  <b>int numFleets = 1;<br />
  boolean attackMode = false;<br />
  if (pw.Production(1) >= pw.Production(2)) {<br />
    &nbsp;&nbsp;&nbsp;&nbsp;numFleets = 1;<br />
  } else {<br />
    &nbsp;&nbsp;&nbsp;&nbsp;numFleets = 3;<br />
  }</b><br />
  if (pw.MyFleets().size() >= <b>numShips</b>) {<br />
    &nbsp;&nbsp;&nbsp;&nbsp;return;<br />
  }</p>
<p>Basically what this strategy says is "if you're ahead then take it easy,
  take few risks, and maintain the status quo. If you're behind, take lots of
  risk to try to change the status quo".</p>
<p>Save and compile this code. Try playing it against BullyBot. Notice that as
  long as your bot is ahead, then it is calm and careful. If your bot ever
  falls behind its opponent, then it panics and sends a whole bunch of ships
  out. Anything to get ahead.</p>
<h3>Next Steps</h3>
<p>Go ahead and submit your latest bot to see how it ranks. Your ranking
  should settle down within about an hour. After this, it's really up to you
  which direction to go. There are a couple <a href="resources.php">
  more resources</a> to guide you on your way. However, you're free to
  throw out all of the suggestions from these tutorials, and do your own
  thing instead!</p>
<?php include "footer.php"; ?>

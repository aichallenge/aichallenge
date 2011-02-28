<?php include 'header.php'; ?>

<h2>Special Thanks</h2>
<p>The Google AI Challenge is designed, built, and operated completely by
  volunteers. As the number of participants has steadily grown, so has the
  amount of work required to make it all happen.</p>
<p>With the move to
  <a href="http://code.google.com/p/ai-contest/">fully open-source</a>,
  many of the contestants have been contributing code to help us
  keep up with the volume of feature requests and bug fixes. All-in-all,
  several dozen people have contributed to the design, implementation,
  and operation of the Fall 2010 Google AI Challenge. To all the hackers
  who sent us patches to fix bugs, created new and interesting visualizers,
  set up independent game servers, translated the starter packages into new
  languages, and told us how to fix the webserver whenever it got crushed by
  traffic spikes from Reddit, we are truly grateful. Thank you.</p>
<p>There are a few people who have been especially generous with their time
  and expertise. Our special thanks goes out to these individuals.</p>
<h3>Daniel von Fange</h3>
<p>Daniel was originally a contestant, and generously agreed to join the core
  admin team one hectic day when the web server was being crushed by a traffic
  spike. Drawing on his considerable experience, he quickly brought the
  situation under control. From that moment until mid-October, Daniel became
  the primary operator of the web server and the contest backend.</p>
<p>Daniel took the lead role in creating a distributed system for
  playing games once it became clear that our single game-playing server was
  not keeping up with the demand. He also pushed through dozens of new
  features and bug fixes. Daniel is a machine.</p>
<h3>Janzert</h3>
<p>Janzert was originally a contestant. He joined the core admin team so that
  he could work on improving and stabilizing the contest systems. From October
  onwards, he was the primary operator of the entire contest backend,
  including the newly created cluster of game-playing servers. He boosted
  the throughput of the system from an average of 8 games per minute to
  our current level of 134, meaning that each submission gets to play a lot
  more games.</p>
<p>Janzert contributed dozens of new features and bug fixes, eliminating
  90% of the problems that people found most frustrating about the contest.
  He is a machine.</p>
<h3>Alex Stan</h3>
<p>Better known as amstan, Alex was in charge of the forums and the IRC
  channel. He worked tirelessly to make sure that people's questions got
  answered, and kept the development team focused on the things that
  mattered most to the contest participants.</p>
<p>Alex is a member of the University of Waterloo Computer Science Club,
  and was the winner of the first Google AI Challenge.</p>
<h3>Zannick (A.K.A. jokeserver)</h3>
<p>jokeserver fixed some important bugs in the original game engine's logic.
  He is also responsible for many improvements to the server-side compile
  system that compiles your code after you submit it on the website.</p>
<h3>Syed Albiz</h3>
<p>A member of the University of Waterloo Computer Science Club, Syed gave
  countless hours of his time to create a security system for
  running untrusted code safely on the contest servers.</p>
<h3>Alex Riedler</h3>
<p>Designed and coded the website template from scratch. Alex is a member of
  the UW Computer Science Club.</p>
<h3>Patrick Paskaris</h3>
<p>Created the visualization program that comes with the starter packages,
  better known as ShowGame.jar.</p>
<h3>Daniel Hartmeier</h3>
<p>Better known as dhartmei, Daniel created the "TCP Server", a popular
  unofficial game server. This server let people play their bots against
  others, while running their bots on their own computer locally. This allowed
  power users to get much better feedback than they would from the official
  game server. Most of the top-ranked bots were created with the help of
  dhartmei's game server.</p>
<?php include 'footer.php'; ?>

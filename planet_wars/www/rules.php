<?php include 'header.php' ?>

<h2>Rules</h2>
<ul>
  <li>Your bot is executed at the start of the game and continues executing
      for the duration of the game. Bots must not terminate before reading
      EOF from standard input.</li>
  <li>The final rankings will be determined using a computerized tournament
    designed by the contest organizer. The official results will be final.
    The ongoing rankings on the leaderboard are not official and may not
    be completely representative of the final results.</li>
  <li>You can only have one account. If you have effective control over more
    than one account, even if the accounts are all nominally owned by other
    people, you will be disqualified.</li>
  <li>Your program may not take more than one second to make any
    individual move. The only exception is the first move, for which you have
    three seconds.
    The extra time is for loading your language runtime, or precalculating
    information based on the map if you think you can afford it.
    If your program violates the time limit it becomes suspended from further play
    automatically.</li>
  <li>Entries which are deemed to violate the spirit of fair and sportsmanlike
    competition will be disqualified without any opportunity for appeal.
    In particular, memory scanning, intentionally losing games, and behavior
    conditional on the opponent's identity are prohibited.</li>
  <li>You may not write to files. You may however read from files within your
      submission directory, which will be the current directory.</li>
  <li>Use of multiple processes or threads is prohibited.</li>
  <li>Attempts to circumvent the security mechanisms in place for the contest
  will result in disqualification except when approved beforehand by contest staff.</li>
  <li>We reserve the right to change these rules at any time without notice.</li>
</ul>

<?php include 'footer.php' ?>

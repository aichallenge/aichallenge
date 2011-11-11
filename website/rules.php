<?php

$title="Rules";
include 'header.php'

?>

<h2>Rules</h2>
<ul>
  <li>The final rankings will be determined using a computerized tournament
    designed by the contest organizer. The official results will be final.
    The ongoing rankings on the leaderboard are not official and may not
    be representative of the final results.</li>
  <li>You can only have one account. If you have effective control over more
    than one account, even if the accounts are all nominally owned by other
    people, you will be disqualified.</li>
  <li>Entries which are deemed to violate the spirit of fair and sportsmanlike
    competition will be disqualified without any opportunity for appeal. In
    particular, memory scanning, manipulating rankings by intentionally losing
    games, and trying to gather information about your opponent or the game
    state through means outside of the bot input protocol are prohibited.</li>
  <li>Data written to files will not be preserved between games and a bot found
    to be writing excessive amounts (more than 10MB) will be disqualified.</li>
  <li>Actively using multiple processes or OS threads is not allowed. Some
    language runtimes will not operate without using multiple OS threads so we
    allow the creation of a limited number. But a bot found to be running
    active computation on more than one will be disqualified.</li>
  <li>As one would hope should not even need mentioning; any attempt to bypass
    or disable the contest's security measures, disrupt the contest software or
    servers, or otherwise interfere with normal operation of the contest is of
    course prohibited.</li>
  <li>We reserve the right to change these rules at any time without notice.</li>
</ul>

<?php include 'footer.php' ?>

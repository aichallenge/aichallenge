<?php

include 'session.php';

if (!logged_in_with_valid_credentials()) {
  header('location:login.php');
}

include 'header.php';
?>

<h2>Upload Your Code</h2>

<p>When you upload your code, the testing environment automatically compiles
  it, and your entry will begin playing games against other people's
  entries. Within an hour, you should be able
  to see your username on the <a href="rankings.php">leaderboard</a>.</p>
<p>You can upload your code as often as you want. The rankings are not cumulative,
  so you're not at a disadvantage by re-uploading your code. The only downside is
  that it might take a few minutes for your new entry to play enough games for its
  ranking on the <a href="rankings.php">leaderboard</a> to stabilize.</p>
<p>Just a few things to remember...</p>
<ul>
  <li>Your main code file must be called MyBot.<i>ext</i>, where <i>ext</i>
    is java, cc, or whatever the standard extension for code files in your
    language is. Remember to include all the code files that your entry
    needs.</li>
  <li>Your zip file may not exceed 2 MB in size. If you need to submit a bigger
    file for some reason, that's fine. Post on the forums and we'll work
    something out.</li>
  <li>Make sure that your code compiles okay on your own machine before
    submitting it. If it doesn't compile there, it won't compile in here
    either. Test your code before submitting it to avoid wasting our
    precious precious CPU cycles.</li>
  <li>We highly suggest using one of the premade starter packages as a starting
    point.</li>
  <li>Keep in mind the <a href="rules.php">rules</a>. We take
    security very seriously. Any attempt to compromise the integrity of the
    contest will result in the immediate involvement of law enforcement
    officials. Our policy is to always prosecute..</li>
</ul>

<form enctype="multipart/form-data" action="check_submit.php" method="POST">
<input type="hidden" name="MAX_FILE_SIZE" value="2000000" />
<b>Choose Your Zip File:</b> <input name="uploadedfile" type="file" /><br />
<input type="submit" value="Upload!" />
</form>

<p>If you are having trouble, there are tons of ways to <a href="forums/">get help on the forums!</a></p>

<?php include 'footer.php'; ?>

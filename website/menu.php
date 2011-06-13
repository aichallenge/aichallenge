<div id="menu">
    <h1>Overview</h1>
    <ul>
      <li><a href="index.php">Home</a></li>
      <li><a href="rankings.php">Current Rankings</a></li>
      <li><a href="problem_description.php">Problem Description</a></li>
      <li><a href="rules.php">Contest Rules</a></li>
    </ul>
    <?php if (logged_in_with_valid_credentials()) { ?>
    <h1>My Account</h1>
    <ul>
      <li><a href="profile.php?user=<?php echo current_user_id(); ?>">My Profile</a></li>
      <li><a href="submit.php">Upload Your Code</a></li>
      <li><a href="logout.php">Sign Out</a></li>
    </ul>
    <?php } ?>
    <h1>Getting Started</h1>
    <ul>
      <li><a href="register.php">Create Your Account</a></li>
      <li><a href="quickstart.php">Five Minute Quickstart Guide</a></li>
      <li><a href="starter_packages.php">Starter Packages</a></li>
      <li><a href="resources.php">Tutorials &amp; Strategy Guides</a></li>
      <li><a href="specification.php">Game Specification</a></li>
    </ul>
    <h1>Help</h1>
    <ul>
      <li><a href="faq.php">Frequently Asked Questions</a></li>
      <li><a href="forums/">Forum</a></li>
      <li><a href="irc://irc.freenode.org/aichallenge">IRC</a> <a href="http://webchat.freenode.net/?channels=aichallenge&uio=d4">(webclient)</a></li>
    </ul>
    <h1>User Search:</h1>
    <form name="user_search_form" method="get" action="find_user.php">
        <input name="username" type="text" style="width: 10em" />
        <input type="submit" name="search" value="Go" />
    </form>
</div>
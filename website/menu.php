<div id="menu">
    <h1><a href="index.php">Home</a></h1>
    <h1>Contest Overview</h1>
    <ul>
      <li><a href="rankings.php">Current Rankings</a></li>
      <li><a href="games.php">Latest Games</a></li>
      <li><a href="maps.php">Official Maps</a></li>
    </ul>
    <h1>My Account</h1>
    <?php if (logged_in_with_valid_credentials()) { ?>
    <ul>
      <li><a href="profile.php?user=<?php echo current_user_id(); ?>">My Profile</a></li>
      <li><a href="submit.php">Upload Your Code</a></li>
      <li><a href="logout.php">Sign Out</a></li>
    </ul>
    <?php } else { ?>
    <ul>
      <li><a href="login.php">Sign In</a></li>
      <li><a href="register.php">Sign Up</a></li>
    </ul>
    <?php } ?>
    <h1>Getting Started</h1>
    <ul>
      <li class="highlight"><a href="quickstart.php">Five Minute Quickstart Guide</a></li>
      <li><a href="ants_tutorial.php">Ants Tutorial</a></li>
      <li><a href="rules.php">Contest Rules</a></li>
      <li><a href="using_the_tools.php">Using the Tools</a></li>
      <li><a href="faq.php">Frequently Asked Questions</a></li>
    </ul>
    <h1>Ants</h1>
    <ul>
      <li class="highlight"><a href="starter_packages.php">Starter Packages</a></li>
      <li><a href="problem_description.php">Problem Description</a></li>
      <li><a href="specification.php">Game Specification</a></li>
      <li><a href="game_settings.php">Game Settings</a></li>
      <li><a href="strategy_guide.php">Strategy Guide</a></li>
      <li><a href="http://paste.aichallenge.org/">Share Maps & Replays</a></li>
    </ul>
    <h1>Community</h1>
    <ul>
      <li><a href="http://forums.aichallenge.org/">Forums</a></li>
      <li><a href="irc://irc.freenode.org/aichallenge">IRC</a> <a href="http://webchat.freenode.net/?channels=aichallenge&uio=d4">(webclient)</a></li>
      <li><a href="http://github.com/aichallenge/aichallenge">Github Repository</a></li>
    </ul>
    <h1>Past Contests</h1>
    <ul>
      <li><a href="http://tron.aichallenge.org/">Tron (Winter 2010)</a></li>
      <li><a href="http://planetwars.aichallenge.org/">Planet Wars (Fall 2010)</a></li>
    </ul>
    <h1>User Search:</h1>
    <form name="user_search_form" method="get" action="find_user.php">
        <input name="username" type="text" style="width: 10em" />
        <input type="submit" name="search" value="Go" />
    </form>
</div>

    </div> 
    <div id="sidebar">
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
      <li><a href="profile.php?user_id=<?php echo current_user_id(); ?>">
        My Profile
        </a></li>
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
    </ul>
    <h1>Help</h1>
    <ul>
      <li><a href="faq.php">Frequently Asked Questions</a></li>
      <li><a href="forum/">Forum</a></li>
    </ul>
    <h1>Galcon</h1>
    <ul>
      <li><a href="http://www.galcon.com">Galcon Home</a></li>
      <li><a href="http://www.galcon.com/fusion/">Galcon for Desktop</a></li>
      <li><a href="http://www.galcon.com/iphone/">Galcon for iPhone</a></li>
      <li><a href="http://www.galcon.com/flash">Play Galcon Online for
        Free</a></li>
    </ul>
    <?php if (logged_in_as_admin()) { ?>
    <h1>Admin</h1>
    <ul>
      <li><a href="#">Link 1</a></li>
      <li><a href="#">Link 2</a></li>
      <li><a href="#">Link 3</a></li>
    </ul>
    <?php } ?>
  </div>
    </div>

	<div id="footer"></div>
<!-- /div -->
</div>

</body>
</html>

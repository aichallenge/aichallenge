<?php include 'session.php'; ?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
      "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="content-type" content="text/html; charset=utf-8" />
  <title>Google AI Challenge</title>
  <link rel="stylesheet" href="css/reset.css" type="text/css" media="screen, projection">
  <link rel="stylesheet" href="css/typo.css" type="text/css" media="screen, projection">
  <link rel="stylesheet" href="css/main.css" type="text/css" media="screen, projection">
<script type="text/javascript">
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-10883001-6']);
  _gaq.push(['_trackPageview']);
  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
 })();
</script>
</head>
<body>
<div id="main">
<!-- <div id="wrapper"> -->
  <div id="header">
    <div class="grid">
    <h1>Google AI Challenge</h1>
    <h2>Organized by the University of Waterloo Computer Science Club and sponsored by Google</h2>
    <span id="sign">
      <?php if (logged_in_with_valid_credentials()) { ?>
        <a href="profile.php?user_id=<?php echo current_user_id(); ?>">
          My Profile (<?php echo htmlspecialchars(current_username()); ?>)
          </a> |
        <a href="logout.php">Sign Out</a>
      <?php } else { ?>
        <a href="login.php">Sign In</a> |
        <a href="register.php">Sign Up</a>
      <?php } ?>
    </span>
    </div>
  </div>
  <div class="shadow"></div>
  <div id="content">
  <div id="news">

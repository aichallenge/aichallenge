<?php include 'session.php'; ?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
      "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="content-type" content="text/html; charset=iso-8859-1" />
  <title>Google AI Challenge</title>
  <link rel="stylesheet" href="css/reset.css" type="text/css" media="screen, projection">
  <link rel="stylesheet" href="css/typo.css" type="text/css" media="screen, projection">
  <link rel="stylesheet" href="css/main.css" type="text/css" media="screen, projection">
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
          My Profile (<?php echo current_username(); ?>)
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

<?php include 'session.php'; ?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
      "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="content-type" content="text/html; charset=iso-8859-1" />
  <title>Google AI Challenge</title>
  <meta name="generator" content="Amaya, see http://www.w3.org/Amaya/" />
  <link rel="stylesheet" href="css/main.css" type="text/css"
	media="screen, projection">
</head>
<body>
<div id="main">
<div class="space"></div>
<div id="wrapper">
  <div id="header">
    <h1>Google AI Challenge</h1>
    <h2>Organized by the Uniersity of Waterloo Computer Science Club.
      Sponsored by Google.</h2>
    <span id="sign">
      <?php if (logged_in_with_valid_credentials()) { ?>
        <a href="profile.php">My Profile</a> |
        <a href="logout.php">Sign Out</a>
      <?php } else { ?>
        <a href="login.php">Sign In</a> |
        <a href="register.php">Sign Up</a>
      <?php } ?>
    </span>
  </div>
  <div class="shadow"></div>
  <div id="sidebar">
    <h2>Overview</h2>
    <ul>
      <li><a href="index.php">Home</a></li>
      <li><a href="rankings.php">Current Rankings</a></li>
      <li><a href="problem_description.php">Problem Description</a></li>
    </ul>
    <h2>Getting Started</h2>
    <ul>
      <li><a href="register.php">Create Your Account</a></li>
      <li><a href="quickstart.php">Five Minute Quickstart Guide</a></li>
      <li><a href="resources.php">Tutorials &amp; Strategy Guides</a></li>
    </ul>
    <h2>Help</h2>
    <ul>
      <li><a href="faq.php">Frequently Asked Questions</a></li>
      <li><a href="forum/">Forum</a></li>
    </ul>
  </div>
  <div id="content">

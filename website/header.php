<!DOCTYPE html>
<?php
$page_render_start_time = microtime(true);
?>
<html xmlns="http://www.w3.org/1999/xhtml"> 
    <head> 
        <title>Google AI Challenge</title> 
        <meta name="description" content="The Google AI Challenge is an international programming contest started by the University of Waterloo Computer Science Club." /> 
        <meta name="keywords" content="" /> 
        
        <!-- Global Tags --> 
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" /> 
        <link rel="shortcut icon" href="favicon.ico" />
 
        <!-- CSS --> 
        <link href="ai-contest.css" rel="stylesheet" type="text/css" /> 
    </head> 
<?php
require_once('session.php');
flush();
?>    
    <body> 
        <div id="wrapper">
            <div id="header" class="columns">
                <div class="left">
                    <h1>Google AI Challenge</h1>
                </div>
                <div class="right">
                    <img src="images/ants.png" alt="Ants Logo" />
                    <h2>Ants</h2>
                    <h3>Spring 2011</h3>
                    <div id="triangle"></div>
                </div>
            </div>
            <div class="columns">
                <div class="left">
                    <div id="login">
                        <?php if (logged_in_with_valid_credentials()) { ?>
                            <a href="profile.php?user=<?php echo current_user_id(); ?>">
                            My Profile: <span class="large"><?php echo htmlspecialchars(current_username()); ?></span>
                            </a> |
                            <a href="logout.php">Sign Out</a>
                        <?php } else { ?>
                            <a href="login.php">Sign In</a> |
                            <span><a href="register.php">Sign Up</a></span>
                        <?php } ?>
                    </div>
                    <div id="content">

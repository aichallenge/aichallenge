<!DOCTYPE html>
<?php

require_once('server_info.php');
$page_render_start_time = microtime(true);

//Title mechanism, $title is declared on every page. CMS next year please.
if(isset($title)) {
  $title = $title." | AI Challenge";
} else {
  $title = "AI Challenge";
}

?>
<html xmlns="http://www.w3.org/1999/xhtml">
    <head>
        <title><?php echo $title; ?></title>
        <meta name="description" content="The AI Challenge (sponsored by Google) is an international programming contest started by the University of Waterloo Computer Science Club." />
        <meta name="keywords" content="" />

        <!-- Global Tags -->
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <link rel="shortcut icon" href="favicon.ico" />

        <!-- CSS -->
        <link href="aichallenge.css" rel="stylesheet" type="text/css" />

        <!-- JavaScript -->
        <script src="//ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"></script>
        <script src="/js/jquery.tablesorter.min.js"></script>
        <script src="/js/jquery.countdown.min.js"></script>
        <script src="/js/tab_sync.js"></script>
<?php

if (file_exists('server_head.html')) {
    include('server_head.html');
}

?>
    </head>
<?php

require_once('session.php');

// attempt cookie login if needed
if (!logged_in_with_valid_credentials()) {
    if (list($header_user_id, $header_user_cookie) = validate_user_cookie()) {
        require_once('mysql_login.php');
        check_credentials_cookie($header_user_id, $header_user_cookie);
    }    
}

flush();
?>
    <body>
        <div id="wrapper">
            <div id="header" class="columns">
                <div class="left">
                    <h1>AI Challenge</h1>
                    <h3>Sponsored by Google</h3>
                </div>
                <div class="right">
                    <h2>Ants</h2>
                    <h3>Fall 2011</h3>
                    <div id="triangle"></div>
                </div>
            </div>
<?php
if ($server_info["show_countdown"]) { 
?>
        	<div class="countdown" id="countdown">Timer Goes Here</div>
			<script>
				$('#countdown').countdown({until: new Date('<?php $now = new DateTime($server_info['deadline']); echo $now->format("M j, Y H:i:s O"); ?>'),
					                       description: "<?php echo $server_info['deadline_desc']; ?>"});
			</script>
<?php 
}
?>
            <div class="columns">
                <div class="left">
                    <div id="content">

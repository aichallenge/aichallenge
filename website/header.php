<!DOCTYPE html>
<?php
$page_render_start_time = microtime(true);
?>
<html xmlns="http://www.w3.org/1999/xhtml"> 
    <head> 
        <title>AI Challenge</title> 
        <meta name="description" content="The AI Challenge (sponsored by Google) is an international programming contest started by the University of Waterloo Computer Science Club." /> 
        <meta name="keywords" content="" /> 
        
        <!-- Global Tags --> 
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" /> 
        <link rel="shortcut icon" href="favicon.ico" />
 
        <!-- CSS --> 
        <link href="ai-contest.css" rel="stylesheet" type="text/css" />

        <!-- JavaScript -->
        <script src="//ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"></script>
        <script src="/js/jquery.tablesorter.min.js"></script>
        <script src="/js/tab_sync.js"></script>
    </head> 
<?php
require_once('session.php');
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
            <div class="columns">
                <div class="left">
                    <div id="content">

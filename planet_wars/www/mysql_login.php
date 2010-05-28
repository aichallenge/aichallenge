<?php

// include guard
if (!isset($JPC_CONTEST_MYSQL_LOGIN_PHP__)) {
$JPC_CONTEST_MYSQL_LOGIN_PHP__ = 1;

// Get the database login information from the server_info.txt file.
include 'server_info.php';
$server_info = get_server_info();

// Login credentials for MySQL database.
$db_host = $server_info["db_host"]; // Host name
$db_username = $server_info["db_username"]; // Mysql username
$db_password = $server_info["db_password"]; // Mysql password
$db_name = $server_info["db_name"]; // Database name

// Connect to server and select database.
mysql_connect($db_host, $db_username, $db_password) or die('cannot connect: ' . print_r(error_get_last()));
mysql_select_db("$db_name")or die("cannot select DB");

}
?>

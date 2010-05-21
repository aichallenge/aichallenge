<?php

// include guard
if (!isset($JPC_CONTEST_MYSQL_LOGIN_PHP__)) {
$JPC_CONTEST_MYSQL_LOGIN_PHP__ = 1;

// Login credentials for MySQL database.
$db_host = "127.0.0.1"; // Host name
$db_username = "j3camero"; // Mysql username
$db_password = "nxjGRAeqLQ"; // Mysql password
$db_name = "j3camero_contest"; // Database name

// Connect to server and select database.
mysql_connect($db_host, $db_username, $db_password) or die('cannot connect: ' . print_r(error_get_last()));
mysql_select_db("$db_name")or die("cannot select DB");

}
?>

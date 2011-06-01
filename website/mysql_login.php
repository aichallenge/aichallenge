<?php

require_once('server_info.php');
require_once('sql.php');

// Get the database login information from the server_info.txt file.

// Login credentials for MySQL database.
$db_host = $server_info["db_host"]; // Host name
$db_username = $server_info["db_username"]; // Mysql username
$db_password = $server_info["db_password"]; // Mysql password
$db_name = $server_info["db_name"]; // Database name

// Connect to server and select database.
mysql_pconnect($db_host, $db_username, $db_password) or die('cannot connect: ' . print_r(error_get_last()));
mysql_select_db("$db_name")or die("cannot select DB");

// salty function, used for passwords in crypt with SHA
function salt($len=16) {
    $pool = range('!', '~');
    $high = count($pool) - 1;
    $tmp = '';
    for ($c = 0; $c < $len; $c++) {
        $tmp .= $pool[rand(0, $high)];
    }
    return $tmp;
}

function contest_query() {
    global $contest_sql;
    $args = func_get_args();
    if (count($args) >= 1) {
        $query_name = $args[0];
        if (count($args) > 1) {
            $query_args = array_map('mysql_real_escape_string',
                                    array_slice($args, 1));
            if (function_exists('api_log')) {
                api_log(vsprintf($contest_sql[$query_name], $query_args));
            }
            return mysql_query(vsprintf($contest_sql[$query_name], $query_args));
        } else {
            if (function_exists('api_log')) {
                api_log("sql: ".$contest_sql[$query_name]);
            }
            return mysql_query($contest_sql[$query_name]);
        }
    }
}


function check_credentials($username, $password) {
  $query = "
        SELECT *
        FROM user u
        WHERE
            username='$username' AND
            activated = 1
    ";
  $result = mysql_query($query);
    if( $user = mysql_fetch_assoc( $result ) ) {
        if (crypt($password, $user['password']) == $user['password']) {
            $_SESSION['username']   = $user['username'];
            $_SESSION['admin']      = $user['admin'];
            $_SESSION['user_id']    = $user['user_id'];
            return true;
        } else {
            return false;
        }
    } else {
        return false;
    }
}

?>

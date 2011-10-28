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

if (!function_exists("api_log")) {
    function api_log($message) {
        global $server_info;
        $message = str_replace("\n", "", $message);
        $message = str_replace("\r", "", $message);
        $message = sprintf("%s - %s", date(DATE_ATOM), $message) . "\n";
        error_log($message, 3, $server_info["api_log"]);
    }
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
                // api_log(vsprintf($contest_sql[$query_name], $query_args));
            }
            return mysql_query(vsprintf($contest_sql[$query_name], $query_args));
        } else {
            if (function_exists('api_log')) {
                // api_log("sql: ".$contest_sql[$query_name]);
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
/*
 * Checks if stored in cookie value is right, logs in user if so.
 * @since 28 Oct 2011 bear@deepshiftlabs.com
 */
function check_credentials_cookie($user_id, $login_cookie) {
  $query = "
        SELECT *
        FROM user u
        WHERE
            user_id='$user_id' AND
            activated = 1
    ";
  $result = mysql_query($query);
    if( $user = mysql_fetch_assoc($result) ) {
        if ($login_cookie == $user['login_cookie']) {
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

function check_reset_credentials($username, $reset) {
  $query = "
        SELECT *
        FROM user u
        WHERE
            username='$username' AND
            activated = 1
    ";
  $result = mysql_query($query);
    if( $user = mysql_fetch_assoc( $result ) ) {
        if (crypt($password, $user['reset']) == $user['reset']) {
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

/*
 * Generates and stores cookie_value for user
 * @return string cookie_value if success, NULL otherwise
 * @since 28 Oct 2011 bear@deepshiftlabs.com
 */
function create_cookie_value($user_id) {
    $login_cookie = md5(salt(64));
    $query = "UPDATE user SET login_cookie='" . $login_cookie . "' WHERE user_id = " . $user_id;
    if (mysql_query($query)) {
        return $login_cookie;
    } else {
        return NULL;
    }
}

/*
 * Returns user's cookie uid as "user_id:cookie_value".
 * If cookie_value is not created yet, creates it. If creation fails, returns NULL
 * @since 28 Oct 2011 bear@deepshiftlabs.com
 */
function get_cookie_uid($user_id) {
  $login_cookie = NULL;
  $query = "
        SELECT login_cookie
        FROM user u
        WHERE
            user_id = $user_id
    ";
    $result = mysql_query($query);
    $user = mysql_fetch_assoc( $result );
    if (!$user) {
        return NULL;
    }

    if (!empty($user['login_cookie'])) {
        $login_cookie = $user['login_cookie'];
    } else {
        $login_cookie = create_cookie_value($user_id);
    }

    if (!$login_cookie) {
        return NULL;
    } else {
        return $user_id . ":" . $login_cookie;
    }
}

?>

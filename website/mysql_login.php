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
function salt($len=16, $cookie=FALSE) {
    if ($cookie) {
        // set of characters that look nice in cookies, excluding -, . and _
        $pool = array_merge(range('0','9'), range('a', 'z'), range('A','Z'));
    } else {
        $pool = range('!', '~');
    }
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
            $query = vsprintf($contest_sql[$query_name], $query_args);
        } else {
            $query = $contest_sql[$query_name];
        }
        $result = mysql_query($query);
        if (!$result) {
            api_log("Contest Query Error: ".$query."\n".mysql_error());
        }
        return $result;
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

function check_credentials_forgot($user_id, $forgot_code) {
    // $login_cookie is not encrypted nor stored in the database
    // $user_cookie['cookie'] is encrypted
    $user_forgets = contest_query("select_user_forgot_code", $user_id);
    while ($user = mysql_fetch_assoc($user_forgets)) {
        if (crypt($forgot_code, $user['cookie']) == $user['cookie']) {
            // found valid cookie, reset expire date
            contest_query("delete_user_cookie", $user_id, $user['cookie']);     
            // update session vars
            $_SESSION['username']   = $user['username'];
            $_SESSION['admin']      = $user['admin'];
            $_SESSION['user_id']    = $user['user_id'];
            $_SESSION['cookie']     = $user['cookie'];
            return true;
        }
    }
    return false;
}

/*
 * Checks if stored in cookie value is right, logs in user if so.
 * Updates database and browser with new expiration date
 * @since 28 Oct 2011 bear@deepshiftlabs.com
 */
function check_credentials_cookie($user_id, $login_cookie) {
    // $login_cookie is not encrypted nor stored in the database
    // $user_cookie['cookie'] is encrypted
    $user_cookies = contest_query("select_user_cookies", $user_id);
    while ($user = mysql_fetch_assoc($user_cookies)) {
        if (crypt($login_cookie, $user['cookie']) == $user['cookie']) {
            // found valid cookie, reset expire date
            contest_query("update_user_cookie", $user_id, $user['cookie']);
            setcookie('uid', $login_cookie, time()+60*60*24*5);        
            // update session vars
            $_SESSION['username']   = $user['username'];
            $_SESSION['admin']      = $user['admin'];
            $_SESSION['user_id']    = $user['user_id'];
            $_SESSION['cookie']     = $user['cookie'];
            return true;
        }
    }
    return false;
}

/*
 * Generates and stores cookie for user in database and browser
 * @return string cookie_value if success, NULL otherwise
 * @since 28 Oct 2011 bear@deepshiftlabs.com
 */
function create_user_cookie($user_id) {
    if (isset($_SESSION['user_id'])) {
        $user_id = $_SESSION['user_id'];
        $login_cookie = $user_id . "-" . salt(32, true);
        $encrytped_cookie = crypt($login_cookie, '$6$rounds=54321$' . salt() . '$');
        if (contest_query("insert_user_cookie", $user_id, $encrytped_cookie)) {
            setcookie('uid', $login_cookie, time()+60*60*24*5, '', '', false, true);
            $_SESSION['cookie'] = $encrytped_cookie;
            return $login_cookie;
        } else {
            return NULL;
        }
    }
}

function delete_user_cookie() {
    if (isset($_SESSION['user_id']) && isset($_SESSION['cookie'])) {
        contest_query("delete_user_cookie", $_SESSION['user_id'], $_SESSION['cookie']);
    }
}

function create_user_forgot_code ($username) {
    $user_result = contest_query("select_user_by_name", $username);
    if ($user_result) {
        $user_row = mysql_fetch_assoc($user_result);
        $user_id = $user_row['user_id'];
        $username = $user_row['username'];
        $user_email = $user_row['email'];
        $login_code = $user_id . "-" . salt(32, true);
        $encrypted_code = crypt($login_code, '$6$rounds=54321$' . salt() . '$');
        if (contest_query("insert_user_forgot_code", $user_id, $encrypted_code)) {
            return array($user_id, $username, $user_email, $login_code);
        } else {
            return NULL;
        }
    }
}

?>

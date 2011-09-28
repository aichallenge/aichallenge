<?php
//ini_set('error_reporting', E_ALL);
//ini_set('display_errors', true);

require_once('memcache.php');
require_once('mysql_login.php');
require_once('server_info.php');

if (!isset($_GET['api_key'])) {
    api_log('api request without a key from '.$_SERVER['REMOTE_ADDR']);
    header('HTTP/1.0 401 Unauthorized');
    die();
} else {
	$worker = valid_worker($_GET['api_key'],$_SERVER['REMOTE_ADDR']);
	if (!$worker) {
        api_log('api request with invalid key from '.$_SERVER['REMOTE_ADDR'].' : '.$_GET['api_key']);
	    header('HTTP/1.0 401 Unauthorized');
    	die();
	}
}

/*
 * Functions
 */

function valid_worker($api_key,$ip_address) {
	global $memcache;
	if ($memcache) {
		$memcache->delete('workers');
		// pull workers array from memcache
		$workers = $memcache->get('workers');
		if (!$workers) {
			$workers = array();
			$sql = "select * from worker";
			$result = mysql_query($sql);
			while ($row = mysql_fetch_assoc($result)) {
				$workers[$row["api_key"]] = $row;
			}
			$memcache->set('workers', $workers);
		}
		if (isset($workers[$api_key]) and $workers[$api_key]["ip_address"] == $ip_address) {
			return $workers[$api_key];
		} else {
			return false;
		}
	} else {
		// fallback for no memcache
		$sql = "SELECT * FROM worker WHERE api_key = '".mysql_real_escape_string($api_key)."';";
	    $result = mysql_query($sql);
	    if(!$result || mysql_num_rows($result) == 0){
	        return false;
	    }
	    $worker = mysql_fetch_assoc($result);
	    if($ip_address!=$worker['ip_address']){
	        return false;
	    }
	    return $worker;
	}
}

function api_log($message) {
	global $server_info;
	$message = str_replace("\n", "", $message);
	$message = str_replace("\r", "", $message);
	$message = sprintf("%s - %s", date(DATE_ATOM), $message) . "\n";
	error_log($message, 3, $server_info["api_log"]);
}

?>

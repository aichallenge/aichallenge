<?

require_once('server_info.php');
if ($_GET['api_query_key'] != $server_info['api_query_key']) {
  header('HTTP/1.0 401 Unauthorized');
  die();
}

require_once('mysql_login.php');

$ip = $_GET['ip'];
$worker_lookup = mysql_query(
    "select worker_id from workers where ip_address='".$ip."';");
if ($worker_lookup && mysql_num_rows($worker_lookup) != 0) {
  $row = mysql_fetch_row($worker_lookup);
  $worker_id = $row[0];
  $info = array('id' => $worker_id);
  $gpm_sql = "select count(*)/5 from games where worker='".$worker_id
    ."' and timestamp > timestampadd(minute, -5, current_timestamp)";
  $gpm_query = mysql_query($gpm_sql);
  $row = mysql_fetch_row($gpm_query);
  $info['gpm'] = $row[0];
  $epm_sql = "select count(*)/5 from errors inner join games on
      games.game_id = errors.game_id where games.worker = '".$worker_id
      ."' and errors.timestamp > timestampadd(minute, -5, current_timestamp);";
  $epm_query = mysql_query($epm_sql);
  $row = mysql_fetch_row($epm_query);
  $info['epm'] = $row[0];
  echo json_encode($info);
} else {
  echo json_encode(array("error" => "could not look up ip"));
}

?>


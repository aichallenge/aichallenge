<?

require_once('server_info.php');
if ($_GET['api_query_key'] != $server_info['api_query_key']) {
  header('HTTP/1.0 401 Unauthorized');
  die();
}

require_once('mysql_login.php');

$ip = $_GET['ip'];
$worker_lookup = mysqli_query($mysqli,
    "select worker_id from worker where ip_address='".$ip."';");
if ($worker_lookup && mysqli_num_rows($worker_lookup) != 0) {
  $row = mysqli_fetch_row($worker_lookup);
  $worker_id = $row[0];
  $info = array('id' => $worker_id);
  $gpm_sql = "select count(*)/5 from game where worker='".$worker_id
    ."' and timestamp > timestampadd(minute, -5, current_timestamp)";
  $gpm_query = mysqli_query($mysqli, $gpm_sql);
  $row = mysqli_fetch_row($gpm_query);
  $info['gpm'] = $row[0];
  $epm_sql = "select count(*)/5 from error inner join game on
      game.game_id = error.game_id where game.worker = '".$worker_id
      ."' and error.timestamp > timestampadd(minute, -5, current_timestamp);";
  $epm_query = mysqli_query($mysqli, $epm_sql);
  $row = mysqli_fetch_row($epm_query);
  $info['epm'] = $row[0];
  echo json_encode($info);
} else {
  echo json_encode(array("error" => "could not look up ip"));
}

?>


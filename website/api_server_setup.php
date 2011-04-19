<?

require_once('mysql_login.php');
require_once('server_info.php');

if($_GET['api_create_key'] != $server_info["api_create_key"]){
  header('HTTP/1.0 401 Unauthorized');
  die();
}

$ip = $_SERVER['REMOTE_ADDR'];
$check_sql = "select api_key from worker where ip_address = '".$ip."';";
$check_result = mysql_query($check_sql);
if ($check_result && mysql_num_rows($check_result) != 0){
  $row = mysql_fetch_row($check_result);
  $new_key = $row[0];
} else {
  $new_key = md5(uniqid(null,true).rand());

  $insert_sql = "insert into worker SET api_key = '".mysql_real_escape_string($new_key)."', ip_address = '".$ip."';";
  $success = mysql_query($insert_sql);
  if(!$success){
    echo("# ".mysql_error());
    die('#Failed to create worker key');
  }
}

?>

export api_base_url='http://ai-contest.com'
export api_key='<?php echo $new_key?>'

echo '<?php echo $new_key?>' > /root/.api_key

curl 'http://ai-contest.googlecode.com/svn/branches/20100929-games-in-the-cloud/planet_wars/backend/game_server_build.sh' > /root/game_server_build.sh
sh /root/game_server_build.sh


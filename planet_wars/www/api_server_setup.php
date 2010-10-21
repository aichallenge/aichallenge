<?

require_once('mysql_login.php');
require_once('server_info.php');

if($_GET['api_create_key'] != $server_info["api_create_key"]){
  header('HTTP/1.0 401 Unauthorized');
  die();
}

$new_key = md5(uniqid(null,true).rand());
$ip = $_SERVER['REMOTE_ADDR'];

$sql = "insert into workers SET api_key = '".addslashes($new_key)."', ip_address = '".$ip."';";
$success = mysql_query($sql);
if(!$success){
  echo("# ".mysql_error());
  die('#Failed to create worker key');
}

?>

export api_base_url='http://ai-contest.com'
export api_key='<?=$new_key?>'

echo '<?=$new_key?>' > /root/.api_key

curl 'http://ai-contest.googlecode.com/svn/branches/20100929-games-in-the-cloud/planet_wars/backend/game_server_build.sh' > /root/game_server_build.sh
sh < /root/game_server_build.sh


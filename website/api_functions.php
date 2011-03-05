<?
require_once('mysql_login.php');

$worker = valid_worker($_GET['api_key'],$_SERVER['REMOTE_ADDR']);
if($worker==false){
  header('HTTP/1.0 401 Unauthorized');
  die();
}


/*
 * Functions
 */
 
function valid_worker($api_key,$ip_address){
   $sql = "SELECT * FROM worker WHERE api_key = '".addslashes($api_key)."';";
   $r = mysql_query($sql);
   if(!$r || mysql_num_rows($r) == 0){
     return false;
   }
   $worker = mysql_fetch_assoc($r);
   if($ip_address!=$worker['ip_address']){
     return false;
   }
   return $worker;
}
 
?>
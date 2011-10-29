<?php

require_once('mysql_login.php');
session_start();
delete_user_cookie();  
session_destroy();
setcookie("uid", "", time()-60*60*24*365);
header('Location:index.php');

?>

<?php
  session_start();
  session_destroy();
  setcookie ("uid", "", time()-60*60*24*365);
  header('Location:index.php');
?>

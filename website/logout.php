<?php
  session_start();
  session_destroy();
  setcookie ("aich_login", "", time()-60*60*24*365);
  setcookie ("aich_remme", "", time()-60*60*24*365);
  header('Location:index.php');
?>

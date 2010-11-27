<?php 
header("Cache-Control: no-cache, must-revalidate"); // HTTP/1.1 
header("Expires: Fri, 1 Jan 2010 08:00:00 GMT"); // Date in the past 
$now = new DateTime(); 
echo $now->format("M j, Y H:i:s O")."\n"; 
?>

<?php include 'header.php'; 

$games_per_minute = array();
foreach(array(5,60,1444) as $minutes){
  $sql = "select count(*)/$minutes from games where timestamp > timestampadd(minute, -$minutes, current_timestamp);";
  $r = mysql_fetch_row(mysql_query($sql));
  $games_per_minute[$minutes] = $r[0];
}

?>

<h2>Games per minute</h2>

<table class="bigstats">

  <tr>
    <td><?=number_format($games_per_minute[5],1)?></td>
    <td><?=number_format($games_per_minute[60],1)?></td>
    <td><?=number_format($games_per_minute[1444],1)?></td>
  </tr>
  <tr>
    <th>Last 15 minutes</th>
    <th>Last hour</th>
    <th>Last 24 hours</th>
  </tr>
</table>


<?php include 'footer.php'; ?>
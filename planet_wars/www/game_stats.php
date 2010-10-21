<?php include 'header.php'; 

$games_per_minute = array();
foreach(array(5,60,1444) as $minutes){
  $sql = "select count(*)/$minutes from games where timestamp > timestampadd(minute, -$minutes, current_timestamp);";
  $r = mysql_fetch_row(mysql_query($sql));
  $games_per_minute[$minutes] = $r[0];
}

$games_per_server = array();
$sql = "select count(*)/5 as gpm, worker from games where timestamp > timestampadd(minute, -5, current_timestamp) group by worker;";
$q = mysql_query($sql);
while($r = mysql_fetch_assoc($q)){
  $games_per_server[] = $r;
}


$errors_per_server = array();
$sql = "select e.*, worker from errors e inner join games g on g.game_id = e.game_id where e.timestamp > timestampadd(minute, -5, current_timestamp) group by game_id order by worker,e.timestamp desc ;";
$q = mysql_query($sql);
while($r = mysql_fetch_assoc($q)){
  $errors_per_server[$r['worker']] +=1/5;
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
    <th>Last 5 minutes</th>
    <th>Last hour</th>
    <th>Last 24 hours</th>
  </tr>
</table>


<h2 style="margin-top:1em">Games per minute per server</h2>

<table class="bigstats">
  <tr>
  <?php foreach ($games_per_server as $server): ?>
    <td><?=number_format($server['gpm'])?></td>
  <?php endforeach ?>
  </tr>
  <tr>
  <?php foreach ($games_per_server as $server): ?>
    <th>Server #<?=htmlspecialchars($server['worker'])?></th>
  <?php endforeach ?>
  </tr>
  <tr>
  <?php foreach ($games_per_server as $server): ?>
    <th style="color:#ccc"><?=number_format($errors_per_server[$server['worker']],1)?> EPM</th>
  <?php endforeach ?>
  </tr>

</table>


<?php include 'footer.php'; ?>
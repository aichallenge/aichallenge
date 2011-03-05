<?php include 'header.php'; 

$query = "select count(*) from user where activated=1 and created > (now() - interval 24 hour)";
$result = mysql_query($query);
$row = mysql_fetch_row($result);
$new_users = $row[0];

$query = "select count(*) from submission where timestamp > (now() - interval 24 hour)";
$result = mysql_query($query);
$row = mysql_fetch_row($result);
$submissions = $row[0];

$query = "select count(*) from submission where timestamp > (now() - interval 24 hour) and status = 40";
$result = mysql_query($query);
$row = mysql_fetch_row($result);
$submissions_successful = $row[0];
if ($submissions) {
    $submissions_percentage = ($submissions_successful / $submissions) * 100.0;
} else {
    $submissions_percentage = 0;
}

$query = "select count(*) from game where timestamp > (now() - interval 24 hour)";
$result = mysql_query($query);
$row = mysql_fetch_row($result);
$games_played = $row[0];

$games_per_minute = array();
foreach(array(5,60,1444) as $minutes){
  $sql = "select count(*)/$minutes from game where timestamp > timestampadd(minute, -$minutes, current_timestamp);";
  $r = mysql_fetch_row(mysql_query($sql));
  $games_per_minute[$minutes] = $r[0];
}

$games_per_server = array();
$sql = "select count(*)/5 as gpm, worker from game where timestamp > timestampadd(minute, -5, current_timestamp) group by worker;";
$q = mysql_query($sql);
while($r = mysql_fetch_assoc($q)){
  $games_per_server[] = $r;
}

$errors_per_server = array();
$sql = "select e.*, worker from error e inner join game g on g.game_id = e.game_id where e.timestamp > timestampadd(minute, -5, current_timestamp) group by game_id order by worker,e.timestamp desc ;";
$q = mysql_query($sql);
while($r = mysql_fetch_assoc($q)){
  $errors_per_server[$r['worker']] +=1/5;
}

$error_percentage = array();
foreach ($games_per_server as $server) {
  $id = $server['worker'];
  if ($server['gpm'] == 0) {
    $error_percentage[$id] = 0;
  } else {
    $error_percentage[$id] = ($errors_per_server[$id] / $server['gpm']) * 100;
  }
}

$PAIRCUT_FILE = "/home/contest/pairing_cutoff";
if (is_readable($PAIRCUT_FILE)) {
  $pfc = file($PAIRCUT_FILE);
  $pair_cutoff = $pfc[0];
} else {
  $pair_cutoff = "None";
}

?>

<h2>Last 24 hours</h2>

<table class="bigstats">
  <tr>
    <td><?php echo $new_users?></td>
    <td><?php echo $submissions?></td>
    <td><?php echo $submissions_successful?>
      <span style="font-size: smaller">
        (<?php echo number_format($submissions_percentage,0)?>%)
      </span>
    </td>
    <td><?php echo $games_played?></td>
    <td><?php echo $pair_cutoff?></td>
  </tr>
  <tr>
    <th>New users</th>
    <th>New submissions</th>
    <th>Successful submissions</th>
    <th>Games played</th>
    <th>Pairing cutoff</th>
  </tr>
</table>

<h2 style="margin-top: 1em">Games per minute</h2>

<table class="bigstats">
  <tr>
    <td><?php echo number_format($games_per_minute[5],1)?></td>
    <td><?php echo number_format($games_per_minute[60],1)?></td>
    <td><?php echo number_format($games_per_minute[1444],1)?></td>
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
    <td><?php echo number_format($server['gpm'])?></td>
  <?php endforeach ?>
  </tr>
  <tr>
  <?php foreach ($games_per_server as $server): ?>
    <th>Server #<?php echo htmlspecialchars($server['worker'])?></th>
  <?php endforeach ?>
  </tr>
  <tr>
  <?php foreach ($games_per_server as $server): ?>
    <th style="color:#ccc">
      <?php echo number_format($errors_per_server[$server['worker']],1)?> EPM <br />
      <?php echo number_format($error_percentage[$server['worker']],1)?>%
    </th>
  <?php endforeach ?>
  </tr>

</table>


<?php include 'footer.php'; ?>

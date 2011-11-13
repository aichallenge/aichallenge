<?php

$title="Server Statistics";
include 'header.php';
require_once('mysql_login.php');

$query = "select count(*) from user where activated=1";
$result = mysql_query($query);
$row = mysql_fetch_row($result);
$num_users = $row[0];

$uptime = shell_exec('uptime');
$uptime = explode(' up ', $uptime);
$uptime = explode(', ', $uptime[1]);
$uptime = $uptime[0].', '.$uptime[1];

$query = "select count(*) from game;";
$result = mysql_query($query);
$row = mysql_fetch_row($result);
$num_games = $row[0];

$query = "select count(*) from user where activated=1 and created > (now() - interval 24 hour)";
$result = mysql_query($query);
$row = mysql_fetch_row($result);
$new_users = $row[0];

$query = "select count(*) from submission where timestamp > (now() - interval 24 hour)";
$result = mysql_query($query);
$row = mysql_fetch_row($result);
$submissions = $row[0];

$query = "select count(*) from submission where timestamp > (now() - interval 24 hour) and status in (40, 100)";
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

$q = contest_query("select_worker_stats");
if ($q) {
    while ($r = mysql_fetch_assoc($q)) {
        $games_per_server[] = $r;
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

<h1>Server Statistics</h1>

<h2>GIT information</h2>
<p><strong>Source: </strong><code><?=exec("git remote --v|grep origin|grep fetch")?></code></p>
<p><strong>Branch/Version Information: </strong><code><?=substr(exec("git branch -vv|grep -e ^\\*"),2);?></code></p>

<h2>General</h2>
<table class="bigstats">
  <tr>
    <td><?php echo $num_users?></td>
    <td><?php echo $num_games?></td>
    <td><?php echo $uptime?></td>
  </tr>
  <tr>
    <th>Total users</th>
    <th>Total games played</th>
    <th>Manager uptime</th>
  </tr>
</table>

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

<h2>Games per minute</h2>

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
<?php if(count($games_per_server)==0) echo "<p>Workers are offline.</p>"; else { ?>
<p></p>
<table class="bigstats">
  <tr>
  <?php foreach ($games_per_server as $server): ?>
    <td><?php echo number_format($server['gpm'],1)?></td>
  <?php endforeach ?>
  </tr>
  <tr>
  <?php foreach ($games_per_server as $server): ?>
    <th>Server #<?php echo htmlentities($server['worker_id'], ENT_COMPAT, "UTF-8")?></th>
  <?php endforeach ?>
  </tr>
  <tr>
  <?php foreach ($games_per_server as $server): ?>
    <th style="color:#ccc">
      <?php echo number_format($server['epm'],1)?> EPM <br />
      <?php echo number_format($server['epm'] / $server['gpm'] * 100,1)?>%
    </th>
  <?php endforeach ?>
  </tr>
</table>
<?php } ?>

<?php include 'footer.php'; ?>

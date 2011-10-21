<?php

$title="Map Statistics";
include 'header.php';
require_once('mysql_query.php');

$query = "SELECT map_id, count(1), MAX(game_id) FROM games GROUP BY map_id";
$result = mysql_query($query);
$played = array();
$latest = array();
while ($row = mysql_fetch_assoc($result)) {
    $played[$row['map_id']] = $row['count(1)'];
    $latest[$row['map_id']] = $row['MAX(game_id)'];
}

$query = "SELECT map_id, count(1) FROM game WHERE draw = 1 GROUP BY map_id";
$result = mysql_query($query);
$draws = array();
while ($row = mysql_fetch_assoc($result)) {
    $draws[$row['map_id']] = $row['count(1)'];
}

# this could pull a partial leaderboard with less than 100 players
$query = "SELECT submission_id FROM ranking
    WHERE leaderboard_id = (SELECT MAX(leaderboard_id) FROM leaderboard
        WHERE complete = 1)
    ORDER BY rank LIMIT 100";
$result = mysql_query($query);
$top_players = "";
while ($row = mysql_fetch_assoc($result)) {
    $top_players .= ",".$row['submission_id'];
}
$top_players = substr($top_players, 1);

$query = "SELECT map_id, count(1) FROM game
    WHERE player_one in ($top_players) AND player_two in ($top_players)
    GROUP BY map_id";
$result = mysql_query($query);
$top_played = array();
while ($row = mysql_fetch_assoc($result))
{
    $top_played[$row['map_id']] = $row['count(1)'];
}

$query = "SELECT map_id, count(1) FROM game
    WHERE player_one in ($top_players) AND player_two in ($top_players)
        AND draw = 1
    GROUP BY map_id";
$result = mysql_query($query);
$top_draws = array();
while ($row = mysql_fetch_assoc($result)) {
    $top_draws[$row['map_id']] = $row['count(1)'];
}

$query = "SELECT * FROM map";
$result = mysql_query($query);
$map_info = array();
while ($row = mysql_fetch_assoc($result)) {
    $map_info[$row['map_id']." "] = $row;
}
foreach ($map_info as $map_id => $info) {
    $map_names[$map_id] = $info['name'];
    $priority = $info['priority'];
    if ($priority == 0) $priority = 1000;
    $map_priorities[$map_id] = $priority;
}
array_multisort($map_priorities, SORT_ASC, $map_names, SORT_ASC, $map_info);

$table = <<<EOT
<table class="leaderboard">
<thead>
<tr>
  <th>Map</th>
  <th>Priority</th>
  <th>Total Games</th>
  <th>Total Draws</th>
  <th>Top 100 Games</th>
  <th>Top 100 Draws</th>
</tr>
</thead>
<tbody>
EOT;
$row_num = 0;
foreach ($map_info as $id => $info) {
    $id = intval($id);
    if (!isset($played[$id]) || $played[$id] < 10) continue;
    $row_num += 1;
    $row_class = $row_num % 2 ? "odd" : "even";
    $table .= "<tr class=\"$row_class\">\n";
    $table .= "  <td><a href=\"/visualizer.php?game_id=".$latest[$id]."\">"
        .$info['name']."</a></td> <td>".$info['priority']."</td>\n";
    $draw_per = number_format(($draws[$id] / $played[$id]) * 100, 1);
    $table .= "  <td>".$played[$id]."</td> <td>".$draws[$id]
        ." (".$draw_per."%)</td>\n";
    if ($top_played[$id] > 0) {
        $draw_per = number_format(($top_draws[$id] / $top_played[$id]) *100, 1);
        $table .= "  <td>".$top_played[$id]."</td> <td>".$top_draws[$id]
            ." (".$draw_per."%)</td>\n";
    } else {
        $table .= "  <td>0</td> <td>0 (0)</td>\n";
    }
    $table .= "</tr>\n";
}
$table .= "</tbody></table>\n";

?>

<h2>Map Stats</h2>

<?php echo $table?>

<?php include 'footer.php'; ?>

<?php

$title="Maps";
require_once("header.php");
require_once("mysql_login.php");
require_once("nice.php");

function get_map_data() {
    $map_query = "select m.map_id, filename, priority, max_turns, m.timestamp,
        players, count(*) as game_count, avg(g.game_length) as avg_length,
        STDDEV(g.game_length) as stddev_length
        from map m inner join game g on g.map_id = m.map_id
        where g.timestamp > m.timestamp
        group by m.map_id order by IF(priority >= 0, 0, 1), m.filename asc";
    $cutoff_query = "select cutoff, count(*) as cutoff_count,
        avg(game_length) as avg_length from game
        where map_id = %s and timestamp > '%s'
        group by cutoff";

    $map_results = mysql_query($map_query);
    if (!$map_results) {
        return NULL;
    }
    $maps = array();
    $totals = array();
    $totals["overall"] = 0;
    while ($list_row = mysql_fetch_assoc($map_results)) {
        $maps[] = $list_row;
    }
    foreach ($maps as &$map) {
        $cutoff_results = mysql_query(sprintf($cutoff_query, $map["map_id"],
            $map["timestamp"]));
        $totals["overall"] += $map["game_count"];
        $map["cutoffs"] = array();
        if ($cutoff_results) {
            while ($cutoff_row = mysql_fetch_assoc($cutoff_results)) {
                $map["cutoffs"][] = $cutoff_row;
                $name = $cutoff_row["cutoff"];
                if (isset($totals[$name])) {
                    $totals[$name] += $cutoff_row["cutoff_count"];
                } else {
                    $totals[$name] = $cutoff_row["cutoff_count"];
                }
            }
        }
    }
    ksort($totals);
    return array($maps, $totals);
}

function render_map_list($map_data) {
    $html = "<div><h3>Overall game stats</h3>";
    $totals = $map_data[1];
    $total_games = $totals["overall"];
    $html .= "<p>Total Games: ". $total_games;
    $html .= "<table><tr><th>Game End Reason</th><th>Count</th></th></tr>";
    foreach ($totals as $reason => $count) {
        if (strcmp($reason, "overall") == 0) {
            continue;
        }
        $cutoff_per = ($count / $total_games) * 100;
        $html .= sprintf("<tr><td>%s</td><td>%d (%.2f%%)</td></tr>",
            $reason, $count, $cutoff_per);
    }
    $html .= "</table>
        <p>Each &uarr; and &darr; next to ending percentage signify a 10% difference from the overall average.</p>";

    foreach ($map_data[0] as $map) {
        $html .= "<div><h3>". nice_map($map["filename"]) ."</h3>\n";
        if ($map["priority"] < 0) {
            $html .= "<p><strong>Disabled</strong></p>";
        }
        $html .= "<p>Players: ". $map["players"] ." Max turns: ".
            $map["max_turns"] ." Set at: ". $map["timestamp"] ."</p>\n";
        $html .= sprintf("<p>Games played: %d Average Length: %.2f +- %.2f</p>\n",
            $map["game_count"], $map["avg_length"], $map["stddev_length"]);

        $html .= "<table><tr><th>Game End Reason</th><th>Count</th><th>Average Length</th></tr>\n";
        foreach($map["cutoffs"] as $cutoff) {
            $cutoff_per = ($cutoff["cutoff_count"] / $map["game_count"]) * 100;
            $total_per = ($totals[$cutoff["cutoff"]] / $total_games) * 100;
            $per_diff = $total_per - $cutoff_per;
            $diff_arrow = "";
            if ($per_diff > 10) {
                $diff_arrow = str_repeat("&darr;", $per_diff / 10);
            }
            else if ($per_diff < -10) {
                $diff_arrow = str_repeat("&uarr;", (0-$per_diff) / 10);
            }
            $html .= sprintf("<tr><td>%s</td><td>%d (%.1f%%)%s</td><td>%.2f</td></tr>\n",
                $cutoff["cutoff"], $cutoff["cutoff_count"], $cutoff_per,
                $diff_arrow, $cutoff["avg_length"]);
        }
        $html .= "</table></div>";
    }
    $html .= "</div>";
    return $html;
}

$map_data = get_map_data();
echo render_map_list($map_data);

require_once("footer.php");
?>

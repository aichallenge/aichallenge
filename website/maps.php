<?php
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
        where map_id = %s and timestamp > %s
        group by cutoff";

    $map_results = mysql_query($map_query);
    if (!$map_results) {
        return NULL;
    }
    $maps = array();
    while ($list_row = mysql_fetch_assoc($map_results)) {
        $maps[] = $list_row;
    }
    foreach ($maps as &$map) {
        $cutoff_results = mysql_query(sprintf($cutoff_query, $map["map_id"],
            $map["timestamp"]));
        if (!$cutoff_results) {
            die("Error while retreiving map data.");
        }
        $map["cutoffs"] = array();
        while ($cutoff_row = mysql_fetch_assoc($cutoff_results)) {
            $map["cutoffs"][] = $cutoff_row;
        }
    }
    return $maps;
}

function render_map_list($map_data) {
    $html = "<div>";
    foreach ($map_data as $map) {
        $html .= "<div><h3>". nice_map($map["filename"]) ."</h3>\n";
        if ($map["priority"] < 0) {
            $html .= "<p><em>Disabled</em></p>";
        }
        $html .= "<p>Players: ". $map["players"] ." Max turns: ".
            $map["max_turns"] ." Set at: ". $map["timestamp"] ."</p>\n";
        $html .= sprintf("<p>Games played: %d Average Length: %.2f +- %.2f</p>\n",
            $map["game_count"], $map["avg_length"], $map["stddev_length"]);

        $html .= "<table><tr><th>Game End Reason</th><th>Count</th><th>Average Length</th></tr>\n";
        foreach($map["cutoffs"] as $cutoff) {
            $cutoff_per = ($cutoff["cutoff_count"] / $map["game_count"]) * 100;
            $html .= sprintf("<tr><td>%s</td><td>%d (%.1f%%)</td><td>%.2f</td></tr>\n",
                $cutoff["cutoff"], $cutoff["cutoff_count"], $cutoff_per,
                $cutoff["avg_length"]);
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

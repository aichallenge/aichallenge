<?php
include_once "pagination.php";

/*
 * getGamesTableString
 *
 * Returns Games Table as a string,
 * if $viewmore is true (default), it outputs $viewresults rows and has an underlink to page given
 * if $viewmore is false, it uses pagination with $viewresults rows per page
 * page linking is done with $viewlink
 *
 */
function getGamesTableString($user_id, $viewmore = true, $viewresults = 10, $viewlink, $page=0)
{
    // Avoid SQL injections
    if(!filter_var($user_id, FILTER_VALIDATE_INT)) {
        return "";
    } else {
        $user_id = intval($user_id);
    }
    if (!filter_var($page, FILTER_VALIDATE_INT)) {
        $page = 0;
    } else {
        $page = intval($page);
    }

    // Fetch user's current submission's id
$submission_query = <<<EOT
select
    max(s.submission_id) as id
from
    submission s
where
    s.user_id = $user_id
EOT;

    $submission_data = mysql_query($submission_query);
    if ($submission_data) {
        list($submission) = mysql_fetch_row($submission_data);
    } else {
        $submission = -1;
    }

    // Fetch Row Count
$rowcount_query = <<<EOT
select
    count(1)
from
    game g 
where
    (g.player_one = $submission or g.player_two = $submission)
EOT;

    $rowcount_data = mysql_query($rowcount_query);
    if ($rowcount_data) {
        list($rowcount) = mysql_fetch_row($rowcount_data);
    } else {
        $rowcount = 0;
    }

    // Fetch Game Information For Users Current Submission
$games_query = <<<EOT
(select
    u.username as opp_name,
    u.user_id as opp_id,
    g.game_id,
    g.draw,
    date_format(g.timestamp,'%b %D %H:%i:%S') as date,
    g.timestamp,
    if( g.draw = 0, 'Win', 'Draw' ) as outcome
    from
    game g USE INDEX (winner_3)
    inner join submission s USE INDEX (submission_id) on s.submission_id = g.loser
    inner join user u USE INDEX (user_id) on u.user_id = s.user_id
    where g.winner = $submission
    )
union
(select
    u.username as opp_name,
    u.user_id as opp_id,
    g.game_id,
    g.draw,
    date_format(g.timestamp,'%b %D %H:%i:%S') as date,
    g.timestamp,
    if( g.draw = 0, 'Loss', 'Draw' ) as outcome
   from
    game g USE INDEX (loser_3)
    inner join submission s USE INDEX (submission_id) on s.submission_id = g.winner
    inner join user u USE INDEX (user_id) on u.user_id = s.user_id
    where g.loser = $submission 
    )
union
(select
    u.username as opp_name,
    u.user_id as opp_id,
    g.game_id,
    g.draw,
    date_format(g.timestamp,'%b %D %H:%i:%S') as date,
    g.timestamp,
    'Draw' as outcome
   from
    game g
    inner join submission s USE INDEX (submission_id)
        on s.submission_id = g.player_two
    inner join user u USE INDEX (user_id) on u.user_id = s.user_id
    where g.player_one = $submission AND g.draw = 1
    )
union
(select
    u.username as opp_name,
    u.user_id as opp_id,
    g.game_id,
    g.draw,
    date_format(g.timestamp,'%b %D %H:%i:%S') as date,
    g.timestamp,
    'Draw' as outcome
   from
    games g
    inner join submission s USE INDEX (submission_id)
        on s.submission_id = g.player_one
    inner join user u USE INDEX (user_id) on u.user_id = s.user_id
    where g.player_two = $submission AND g.draw = 1
    )
order by
    timestamp desc
EOT;

    if ($viewmore) {
        $games_query .= " limit $viewresults";
    } else if ($page != 0) {
        $games_query .= " limit $viewresults OFFSET " . ($viewresults * ($page-1));
    }

    $games_results = mysql_query($games_query);

    // If query fails
    if (!$games_results || $rowcount == 0) {
        return "<p>No game information available at this time.</p>";
    }

    // Build table
    $table = "";
    if (!$viewmore) {
        $table .= getPaginationString($page, $rowcount, $viewresults, $viewlink);
    }
    $table .= "<table class=\"submissions\"><thead><tr><th>Time</th><th>Opponent</th><th>Outcome</th><th>&nbsp;</th></tr></thead>";
    $table .= "<tbody>";
    for ($i = 1; $row = mysql_fetch_assoc($games_results); $i += 1) {
        $opp_name = htmlentities($row["opp_name"], ENT_COMPAT, "UTF-8");
        $opp_id = $row["opp_id"];
	$game_id = $row["game_id"];
        $outcome = $row["outcome"];
        $datetime = $row["date"];
        if ($row["draw"] == 1) {
            $outcome = "Draw";
        }

        if ($outcome == "Win") {
            $outcome_class = "game_win";
        } else if ($outcome == "Loss") {
            $outcome_class = "game_loss";
        } else {
            $outcome_class = "game_draw";
        }

        $timestamp = $row["timestamp"];
        $row_class = $i % 2 == 0 ? "even" : "odd";
        $table .= "  <tr class=\"$row_class\">";
        $table .= "    <td>$datetime</td>";
        $table .= "    <td><a href=\"profile.php?user=$opp_id\">$opp_name</a></td>";
        $table .= "    <td class=\"$outcome_class\">$outcome</td>";
	$table .= "    <td><a href=\"visualizer.php?game_id=$game_id\">View Game &gt;&gt;</a></td>";
        $table .= "  </tr>";
    }
    $table .= "</tbody></table>";
    if ($viewmore && $rowcount > $viewresults) {
        $table .= "<a href=\"$viewlink\">View More</a>";
    }

    return $table;
}
?>

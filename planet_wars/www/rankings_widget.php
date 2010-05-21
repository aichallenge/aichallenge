<?php
include_once 'pagination.php';

/*
 * getRankingsTableString
 *
 * Returns Rankings Table as a string,
 * if viewmore is true (default), it outputs viewresults rows and has an underlink to page given
 * if viewmore is false, it uses pagination with viewresults rows per page
 *
 * Also possible to filter table based on single tag and parameter name
 *
 */
function getRankingsTableString($user_id, $viewmore = true, $viewresults = 10, $viewlink, $page=0, $filter=null, $filterparam=null)
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
    $user_id = mysql_real_escape_string($user_id);
    $page = mysql_real_escape_string($page);
    $filter = mysql_real_escape_string($filter);
    $filterparam = mysql_real_escape_string($filterparam);

    $page = $_GET["page"];
    if(!filter_var($page, FILTER_VALIDATE_INT)) {
        $page = 1;
    }

    $filter_text = ($filter == NULL)?"":"and $filter = '$filterparam'";

    // Fetch row count
$rowcount_query = <<<EOT
select
    count(1)
from
    contest_rankings r 
    inner join contest_submissions s on s.submission_id = r.submission_id
    inner join contest_users u on u.user_id = s.user_id
    left outer join contest_organizations o on o.org_id = u.org_id
    left outer join contest_countries c on c.country_id = u.country_id
where
    leaderboard_id = (select max(leaderboard_id) from contest_leaderboards)
    $filter_text
EOT;

    $rowcount_data = mysql_query($rowcount_query);
    if ($rowcount_data) {
        list($rowcount) = mysql_fetch_row($rowcount_data);
    } else {
        $rowcount = 0;
    }

    // Fetch Only Rows Needed For Current Page
    $offset = ($viewresults * ($page-1));
$rankings_query = <<<EOT
select
    u.user_id,
    u.username,
    s.*,
    r.*,
    c.country_id,
    c.name as country_name,
    c.flag_filename,
    o.org_id,
    o.name as org_name,
    round(((r.wins + 0.5*r.draws)/(r.wins+r.draws+r.losses))*100, 2) as rank_percent
from
    contest_rankings r 
    inner join contest_submissions s on s.submission_id = r.submission_id
    inner join contest_users u on u.user_id = s.user_id
    left outer join contest_organizations o on o.org_id = u.org_id
    left outer join contest_countries c on c.country_id = u.country_id
where
    leaderboard_id = (select max(leaderboard_id) from contest_leaderboards)
    $filter_text
order by
    rank asc
EOT;

    if ($viewmore) {
        $rankings_query .= " limit $viewresults";
    } else if ($page != 0) {
        $rankings_query .= " limit $viewresults OFFSET " . ($viewresults * ($page-1));
    }

    $rankings_results = mysql_query($rankings_query);

    // If query fails
    if (!$rankings_results || $rowcount == 0) {
        return "<p>Rankings are not available at the moment. Check back soon!</p>";
    }

    $pagination .= getPaginationString($page, $rowcount, $viewresults, $viewlink);

    $table = "";
    if ($filter != NULL) {
        $table .= "<a href=\"rankings.php\">&#0171; Back to Main Leaderboard</a>";
    }
    if (!$viewmore) {
        $table .= $pagination;
    }
$table .= <<<EOT
<table class="leaderboard">
<thead>
<tr>
  <th>Rank</th>
  <!--<th>Score</th>-->
  <th>Username</th>
  <th>Country</th>
  <th>Organization</th>
  <th>Language</th>
  <th>Elo Score</th>
  <!--<th>Wins</th>-->
  <!--<th>Losses</th>-->
  <!--<th>Draws</th>-->
</tr>
</thead>
<tbody>
EOT;
    $old_score = 999999;
    $old_rank = -1;
    for ($i = 1; $row = mysql_fetch_assoc($rankings_results); $i += 1) {
        $username = htmlentities($row["username"]);
        $programming_language = $row["programming_language"];
	$score = $row["score"];
        $programming_language_link = urlencode($row["programming_language"]);
        $rank = $row["rank"];
	if ($score == $old_score) {
	  $rank = $old_rank;
	}
	$old_score = $score;
	$old_rank = $rank;
        $rank = ($filter == null)? $rank : ($i + $offset) . " <span title='Global Rank'>($rank)</span>";
        $rank_percent = $row["rank_percent"];
        $wins = $row["wins"];
        $losses = $row["losses"];
        $draws = $row["draws"];
        $flag_filename = $row["flag_filename"];
        $country_id = $row["country_id"];
        $country_name = $row["country_name"];
        $country_name = $country_name == NULL ? "Unknown" : htmlentities($country_name);
        $org_name = htmlentities($row["org_name"]);
        $org_id = $row["org_id"];
        $user_id = $row["user_id"];
        $row_class = $i % 2 == 0 ? "even" : "odd";
        $flag_filename = $flag_filename == NULL ? "unk.png" : $flag_filename;
        $flag_filename = "<img alt=\"$country_name\" width=\"16\" height=\"11\" title=\"$country_name\" src=\"flags/$flag_filename\" />";
        if (current_username() == $username) {
            $table .= "  <tr class=\"$row_class, user\">\n";
        } else {
            $table .= "  <tr class=\"$row_class\">\n";
        }
        $table .= "    <td>$rank</td>\n";
        //$table .= "    <td>$rank_percent</td>\n";
        $table .= "    <td><a href=\"profile.php?user_id=$user_id\">$username</a></td>\n";
        $table .= "    <td><a href=\"country_profile.php?country_id=$country_id\">$flag_filename</a></td>";
        $table .= "    <td><a href=\"organization_profile.php?org_id=$org_id\">$org_name</a></td>";
        $table .= "    <td><a href=\"language_profile.php?lang=$programming_language_link\">$programming_language</a></td>";
	$table .= "    <td>$score</td>";
        //$table .= "    <td>$wins</td>";
        //$table .= "    <td>$losses</td>";
        //$table .= "    <td>$draws</td>";
        $table .= "  </tr>\n";
    }
    $table .= "</tbody></table>";
    if (!$viewmore) {
        $table .= $pagination;
    }
    if ($viewmore && $rowcount > $viewresults) {
        $table .= "<a href=\"$viewlink\">View More</a>";
    }
    return $table;
}
?>

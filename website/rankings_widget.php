<?php

require_once('mysql_login.php');
include_once('pagination.php');

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
    $rankings_results = contest_query("select_rankings");
    // If query fails
    if (!$rankings_results) {
        return "<p>Rankings are not available at the moment. Check back soon!</p>";
    }

    $table = "";
    if ($filter != NULL) {
        $table .= "<a href=\"rankings.php\">&#0171; Back to Main Leaderboard</a>";
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
  <th>Skill</th>
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
        $username = htmlentities($row["username"], ENT_COMPAT, 'UTF-8');
        $programming_language = $row["programming_language"];
	$score = $row["skill"];
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
        $country_name = $country_name == NULL ? "Unknown" : htmlentities($country_name, ENT_COMPAT, 'UTF-8');
        $org_name = htmlentities($row["org_name"], ENT_COMPAT, 'UTF-8');
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
        $table .= "    <td><a href=\"profile.php?user= $user_id\">$username</a></td>\n";
        $table .= "    <td><a href=\"country_profile.php?country=$country_id\">$flag_filename</a></td>";
        $table .= "    <td><a href=\"organization_profile.php?org=$org_id\">$org_name</a></td>";
        $table .= "    <td><a href=\"language_profile.php?language=$programming_language_link\">$programming_language</a></td>";
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

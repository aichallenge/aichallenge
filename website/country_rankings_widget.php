<?php

require_once('mysql_login.php');
require_once('session.php');

/*
 * getCountryRankingsTableString
 *
 * Returns Rankings Table as a string,
 * if viewmore is true (default), it outputs viewresults rows and has an underlink to page given
 * if viewmore is false, it uses pagination with viewresults rows per page
 *
 * Also possible to filter table based on single tag and parameter name
 *
 */
function getCountryRankingsTableString($top)
{

    $username = current_username();

    if ($username !== null) {
    // Fetch Country ID for logged in user
    $user_query = <<<EOT
select
    u.country_id
from
    user u
where
    username = '$username'
EOT;

    $user_country_data = mysql_query($user_query);
    if ($user_country_data) {
        list ($user_country_id) = mysql_fetch_row($user_country_data);
    } else {
        $user_country_id = -1;
    }
    }

    // Fetch Rows
    $rankings_query = <<<EOT
select
    c.name as country_name,
    c.country_id,
    c.flag_filename,
    count(*) as num_leaders
from
    ranking r
    inner join submission s on s.submission_id = r.submission_id
    inner join user u on u.user_id = s.user_id
    inner join country c on c.country_id = u.country_id
where
    r.leaderboard_id = (select max(leaderboard_id) from leaderboard
        where complete=1)
    and r.rank <= $top
group by u.country_id
order by num_leaders desc
EOT;
    $rankings_results = mysql_query($rankings_query);

    // If query fails
    if (!$rankings_results) {
        return "<p>Rankings are not available at the moment. Check back soon!</p>";
    }

    $table = "";
$table .= <<<EOT
<table class="leaderboard">
<thead>
<tr>
  <th>Rank</th>
  <th>Leaders</th>
  <th>Country</th>
</tr>
</thead>
<tbody>
EOT;
    for ($i = 1; $row = mysql_fetch_assoc($rankings_results); $i += 1) {
        $num_leaders = $row["num_leaders"];
        $country_id = $row["country_id"];
        $country_name = $row["country_name"];
        $country_name = $country_name == NULL ? "Unknown" : htmlentities($country_name, ENT_COMPAT, 'UTF-8');
        $flag_filename = $row["flag_filename"];
        $flag_filename = $flag_filename == NULL ? "unk.png" : $flag_filename;
        $flag_filename = "<img alt=\"$country_name\" width=\"16\" height=\"11\" title=\"$country_name\" src=\"flags/$flag_filename\" />";

        $row_class = $i % 2 == 0 ? "even" : "odd";
        if ($country_id == $user_country_id) {
            $table .= "  <tr class=\"$row_class, user\">\n";
        } else {
            $table .= "  <tr class=\"$row_class\">\n";
        }
        $table .= "    <td>$i</td>\n";
        $table .= "    <td>$num_leaders</td>\n";
        $table .= "    <td><a href=\"country_profile.php?country_id=$country_id\">$country_name</a></td>";
        $table .= "  </tr>\n";
    }
    $table .= "</tbody></table>";
    return $table;
}

?>

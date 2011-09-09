<?php

require_once('mysql_login.php');
require_once('session.php');

/*
 * getOrganizationRankingsTableString
 *
 * Returns Rankings Table as a string,
 * if viewmore is true (default), it outputs viewresults rows and has an underlink to page given
 * if viewmore is false, it uses pagination with viewresults rows per page
 *
 * Also possible to filter table based on single tag and parameter name
 *
 */
function getOrganizationRankingsTableString($top)
{

    $username = current_username();

    if ($username != null) {
    // Fetch Org ID for logged in user
    $user_query = <<<EOT
select
    u.org_id
from
    user u
where
    username = '$username'
EOT;

    $user_org_data = mysql_query($user_query);
    if ($user_org_data) {
        list ($user_org_id) = mysql_fetch_row($user_org_data);
    } else {
        $user_org_id = -1;
    }
    }

    // Fetch Rows
    $rankings_query = <<<EOT
select
    o.org_id,
    o.name as org_name,
    count(*) as num_leaders
from
    ranking r
    inner join submission s on s.submission_id = r.submission_id
    inner join user u on u.user_id = s.user_id
    inner join organization o on o.org_id = u.org_id
where
    r.leaderboard_id = (select max(leaderboard_id) from leaderboard
        WHERE complete = 1)
    and r.rank <= $top
    and o.org_id <> 0
group by o.org_id
order by num_leader desc
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
  <th>Organization</th>
</tr>
</thead>
<tbody>
EOT;
    for ($i = 1; $row = mysql_fetch_assoc($rankings_results); $i += 1) {
        $num_leaders = $row["num_leaders"];
        $org_name = htmlentities($row["org_name"], ENT_COMPAT, 'UTF-8');
        $org_id = $row["org_id"];
        $row_class = $i % 2 == 0 ? "even" : "odd";
        if ($org_id == $user_org_id) {
            $table .= "  <tr class=\"$row_class, user\">\n";
        } else {
            $table .= "  <tr class=\"$row_class\">\n";
        }
        $table .= "    <td>$i</td>\n";
        $table .= "    <td>$num_leaders</td>\n";
        $table .= "    <td><a href=\"organization_profile.php?org_id=$org_id\">$org_name</a></td>";
        $table .= "  </tr>\n";
    }
    $table .= "</tbody></table>";
    return $table;
}
?>

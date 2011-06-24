<?php
require_once( "pagination.php");
require_once("session.php");
require_once("nice.php");

$status_msg = array(10 => "Created: entry record created in database",
                    20 => "Uploaded: ready to be unzipped and compiled",
                    30 => "Compiling: compiling and running tests",
                    40 => "Success: ready to play",
                    50 => "Download Error: error receiving submission zip file",
                    60 => "Unpack Error: error while unzipping submission file",
                    70 => "Compile Error: error while compiling submission",
                    80 => "Test Error: compiled, but failed test cases",
                    90 => "Upload Error: server failed to retrieve uploaded file correctly",
                    0  => "Unknown Error");

/*
 * getSubmissionTableString
 *
 * Returns Submission Table as a string,
 * if viewmore is true (default), it outputs viewresults rows and has an underlink to page given
 * if viewmore is false, it uses pagination with viewresults rows per page
 *
 */
function getSubmissionTableString($user_id, $viewmore = true, $viewresults = 10, $viewlink, $page=0)
{
    global $status_msg;

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

    // Fetch row count
$rowcount_query = <<<EOT
select
    count(*)
from
    submission
    where user_id = $user_id
EOT;

    $rowcount_data = mysql_query($rowcount_query);
    if ($rowcount_data) {
        list($rowcount) = mysql_fetch_row($rowcount_data);
    } else {
        $rowcount = 0;
    }

    // Fetch submission data
$submission_query = <<<EOT
select
    s.status,
    s.errors,
    s.timestamp,
    s.version,
    s.mu - 3 * s.sigma as skill,
    s.mu,
    s.sigma,
    l.name as language,
    gt.game_count,
    timestampdiff(second, gmin.timestamp, gmax.timestamp)/60/gt.game_count as game_rate
from
    submission s
    left outer join (
        select submission_id, min(game_id) min_game, max(game_id) max_game, count(*) as game_count
        from game_player
        group by submission_id
    ) gt
    	on gt.submission_id = s.submission_id
    left outer join game gmin
        on gmin.game_id = gt.min_game
    left outer join game gmax
        on gmax.game_id = gt.max_game
    inner join user u on u.user_id = s.user_id
    left outer join language l on l.language_id = s.language_id
where
    u.user_id = $user_id
    order by s.timestamp desc
EOT;

    if ($viewmore) {
        $submission_query .= " limit $viewresults";
    } else if ($page != 0) {
        $submission_query .= " limit $viewresults OFFSET " . ($viewresults * ($page-1));
    }

    $submission_results = mysql_query($submission_query);

    // If query fails
    if (!$submission_results || $rowcount == 0) {
        return "<p>No submissions available at this time.</p>";
    }

    // Build table
    $table = "";
    if (!$viewmore) {
        $table .= getPaginationString($page, ceil($rowcount/$viewresults), $viewresults, $viewlink);
    }
    $table .= "<table class=\"submissions\"><thead><tr><th>Version</th><th>Submission Time</th><th>Status</th><th>Skill</th><th>Games</th><th>Game Rate</th><th>Language</th></tr></thead><tbody>";
    for ($i = 1; $row = mysql_fetch_assoc($submission_results); $i += 1) {
        $status = $row["status"];
        $status_class = ($status == 40 ? "success": (($status == 30 || $status > 40)? "fail" : "inprogress"));
        if (isset($status_msg[$status])) {
            $status = $status_msg[$status];
        } else {
            $status = $status_msg[0];
        }

        $version = $row["version"];
        $timestamp = $row["timestamp"];
        $language = $row["language"];
        $language_link = urlencode($language);
        $row_class = $i % 2 == 0 ? "even" : "odd";
        $errors = $row["errors"];
        $skill = $status_class == "success" ? nice_skill($row['skill'], $row['mu'], $row['sigma']) : "-"; // trueskill formula
        $games = $row["game_count"];
        $game_rate = "<span title=\"average minutes between games\">".($row['game_rate'] == NULL ? "" : round($row["game_rate"],0))."</span>";
        
        $table .= "<tr class=\"$row_class\">";
        $table .= "  <td>$version</td>";
        $table .= "  <td>$timestamp</td>";
        $table .= "  <td class=\"$status_class\">$status</td>";
        $table .= "  <td>$skill</td>";
        $table .= "  <td>$games</td>";
        $table .= "  <td>$game_rate</td>";
        $table .= "  <td><a href=\"language_profile.php?language=$language_link\">
            $language</a></td>";
        $table .= "</tr>";
        if ($errors and $status_class == "fail" and $user_id == current_user_id()) {
            $errors = json_decode($errors);
            $error_msg = "<pre class=\"error\">";
            if (isset($errors->errors)) {
                for ($i = 0; $i < count($errors->errors); $i++) {
                    $error_msg .= str_replace('\n', "\n", $errors->errors[$i])."\n";
                }
            } elseif (gettype($errors) == "array") {
                foreach ($errors as $error) {
                    $error_msg .= str_replace('\n', "\n", $error)."\n";
                }
            }
            $error_msg .= "</pre>";
            $table .= "<tr><td colspan=\"5\">$error_msg</td></tr>";
        }
    }
    $table .= "</tbody></table>";
    if ($viewmore && $rowcount > $viewresults) {
        $table .= "<a href=\"$viewlink\">View More</a>";
    }

    return $table;
}
?>

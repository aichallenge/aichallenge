<?php
include_once "pagination.php";

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
    count(1)
from
    submissions r
    inner join submissions s on s.submission_id = r.submission_id
    inner join users u on u.user_id = s.user_id
where
    u.user_id = $user_id
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
    date_format(s.timestamp,'%b %D %H:%i:%S') as timestamp,
    l.name as language
from
    submissions r
    inner join submissions s on s.submission_id = r.submission_id
    inner join users u on u.user_id = s.user_id
    inner join languages l on l.language_id = s.language_id
where
    u.user_id = $user_id
    order by s.timestamp desc
EOT;

 echo "<p>$submission_query</p>";

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
        $table .= getPaginationString($page, $rowcount, $viewresults, $viewlink);
    }
    $table .= "<table class=\"submissions\"><thead><tr><th>Submission Time</th><th>Status</th><th>Language</th></tr></thead><tbody>";
    for ($i = 1; $row = mysql_fetch_assoc($submission_results); $i += 1) {
        $status = $row["status"];
        $status_class = ($status == 40 ? "success": (($status == 30 || $status > 40)? "fail" : "inprogress"));
        $status = ($status == 10 ? "entry created in db"
            : ($status == 15 ? "temporary location, awaiting transfer"
            : ($status == 20 ? "ready to be compiled"
            : ($status == 24 ? "currently compiling"
            : ($status == 27 ? "compiled successfully, awaiting testing"
            : ($status == 30 ? "error receiving submission file"
            : ($status == 39 ? "garbage collected (<a class=\"error_code\" href=\"submission_errors.php?error=$status\">more info</a>)"
            : ($status == 40 ? "sucessfully entered into contest"
            : ($status == 50 ? "error while unzipping submission"
            : ($status == 60 ? "problem with submission file"
            : ($status == 70 ? "error while compiling submission"
            : ($status == 80 ? "compiled sucessfully but failed test cases"
            : ($status >= 90 ? "submission suspended (<a class=\"error_code\" href=\"submission_errors.php?error=$status\">more info</a>)"
            : "Unknown Error")))))))))))));

        $timestamp = $row["timestamp"];
        $language = $row["language"];
        $row_class = $i % 2 == 0 ? "even" : "odd";

        $table .= "<tr class=\"$row_class\">";
        $table .= "  <td>$timestamp</td>";
        $table .= "  <td class=\"$status_class\">$status</td>";
        $table .= "  <td>$language</td>";
        $table .= "</tr>";
    }
    $table .= "</tbody></table>";
    if ($viewmore && $rowcount > $viewresults) {
        $table .= "<a href=\"$viewlink\">View More</a>";
    }

    return $table;
}
?>

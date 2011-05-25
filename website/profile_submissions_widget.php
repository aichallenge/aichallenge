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
    submission r
    inner join submission s on s.submission_id = r.submission_id
    inner join user u on u.user_id = s.user_id
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
    submission r
    inner join submission s on s.submission_id = r.submission_id
    inner join user u on u.user_id = s.user_id
    inner join language l on l.language_id = s.language_id
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
        $table .= getPaginationString($page, $rowcount, $viewresults, $viewlink);
    }
    $table .= "<table class=\"submissions\"><thead><tr><th>Submission Time</th><th>Status</th><th>Language</th></tr></thead><tbody>";
    for ($i = 1; $row = mysql_fetch_assoc($submission_results); $i += 1) {
        $status = $row["status"];
        $status_class = ($status == 40 ? "success": (($status == 30 || $status > 40)? "fail" : "inprogress"));
        $status = ($status == 10 ? "Created: entry record created in database"
            : ($status == 20 ? "Uploaded: ready to be unzipped and compiled"
            : ($status == 30 ? "Compiling: compiling and running tests"
            : ($status == 40 ? "Success: ready to play"
            : ($status == 50 ? "Download Error: error receiving submission zip file"
            : ($status == 60 ? "Unpack Error: error while unzipping submission file"
            : ($status == 70 ? "Compile Error: error while compiling submission"
            : ($status == 80 ? "Test Error: compiled, but failed test cases"
            : ($status >= 90 ? "Upload Error: server failed to retrieve uploaded file correctly"
            : "Unknown Error")))))))));

        $timestamp = $row["timestamp"];
        $language = $row["language"];
        $language_link = urlencode($language);
        $row_class = $i % 2 == 0 ? "even" : "odd";

        $table .= "<tr class=\"$row_class\">";
        $table .= "  <td>$timestamp</td>";
        $table .= "  <td class=\"$status_class\">$status</td>";
        $table .= "  <td><a href=\"language_profile.php?language=$language_link\">
            $language</a></td>";
        $table .= "</tr>";
    }
    $table .= "</tbody></table>";
    if ($viewmore && $rowcount > $viewresults) {
        $table .= "<a href=\"$viewlink\">View More</a>";
    }

    return $table;
}
?>

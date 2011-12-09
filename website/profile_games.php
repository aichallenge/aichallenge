<?php

require_once('game_list.php');
require_once('lookup.php');

$user_id = get_type_or_else('user', NULL);
$submission_id = get_type_or_else('submission', NULL);

if ($user_id !== NULL) {
    $user_row = get_user_row($user_id);
    $partial_title = "'s Recent Games";
} elseif ($submission_id !== NULL) {
    $user_row = get_user_by_submission($submission_id);
    $partial_title = "'s Games for Version ".$user_row['version'];
}

if (!isset($user_row)) {
    die("No user or submission data found.");
}

$user_id = $user_row['user_id'];
$username = htmlentities($user_row['username'], ENT_COMPAT, "UTF-8");

$title=$username.$partial_title;
require_once('header.php');

$page = get_type_or_else("page", FILTER_VALIDATE_INT, 1);

echo "<h2><a href=\"profile.php?user=$user_id\">$username</a>$partial_title</h2>";

echo get_game_list_table($page, ($submission_id ? NULL : $user_id), $submission_id);

echo '
<script>
$(function () {
    $(".games").tablesorter({
        /*textExtraction: function (node) {
            node = $(node);
            if (node.attr("class") === "number") {
        		var n = parseFloat(node.text());
                return isNaN(n) ? 0.0 : n;
            } else {
                return node.text();
            }
        }*/
    });
});
</script>
';

require('footer.php');
?>

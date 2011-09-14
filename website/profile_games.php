<?php
require_once('header.php');
require_once('game_list.php');
require_once('lookup.php');

$user_id = get_type_or_else('user', NULL);
$submission_id = get_type_or_else('submission', NULL);

if ($user_id !== NULL) {
    $user_row = get_user_row($user_id);
    $title = "'s Recent Games";
} elseif ($submission_id !== NULL) {
    $user_row = get_user_row($submission_id);
    $title = "'s Games for Version ".$user_row['version'];
}
$user_id = $user_row['user_id'];
$username = htmlentities($user_row['username'], ENT_COMPAT, "UTF-8");

$page = get_type_or_else("page", FILTER_VALIDATE_INT, 1);

echo "<h2><a href=\"profile.php?user=$user_id\">$username</a>$title</h2>";

echo get_game_list_table($page, ($submission_id ? NULL : $user_id), $submission_id);

echo '
<script>
$(function () {
    $(".games").tablesorter({
        /*textExtraction: function (node) {
            node = $(node);
            if (node.attr("class") === "number") {
                return parseInt(node.text());
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

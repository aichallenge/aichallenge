<?php
include 'header.php';
require_once('game_list.php');
require_once('lookup.php');

$user_row = get_user_row(get_type_or_else('user'));
$user_id = $user_row['user_id'];
$username = htmlspecialchars($user_row['username']);

$page = get_type_or_else("page", FILTER_VALIDATE_INT, 1);

echo "<h2><a href=\"profile.php?user=$user_id\">$username</a>'s latest submission's games</h2>";

echo get_game_list_table($page, $user_id);

include 'footer.php';
?>

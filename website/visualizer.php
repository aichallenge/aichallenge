<?php

include "header.php";

include "visualizer_widget.php";
$game_id = filter_input(INPUT_GET, 'game', FILTER_VALIDATE_INT);
if ($game_id !== FALSE and $game_id !== NULL) {
    visualize_game($game_id);
    require_once('session.php');
    if (TRUE or logged_in_with_valid_credentials()) {
        require_once('mysql_login.php');
        $game_errors = contest_query('select_game_errors',
                                     $game_id);
        if ($game_errors) {
        	$error_msg = "<ul>";
        	while ($row = mysql_fetch_assoc($game_errors)) {
        		// TODO: turn off for all users for contest
        		// also make the query user specific
        		if (TRUE or $row["user_id"] == current_user_id()) {
        			$username = $row["username"];
        			$status = $row["status"];
        			$error_msg .= "<li><p>$username - $status</p><pre class=\"error\">";
                    $error_msg .= str_replace('\n', "\n", $row["errors"])."\n";
        			$error_msg .= "</pre></li>";
        		}
        	}
        	$error_msg .= "</ul>";
        	echo $error_msg;
        }	
    }
    
} else {
    echo '<p>Incorrect Game Number</p>';
}

include "footer.php";

?>

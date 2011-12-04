<?php

$title="Visualizer";
include "header.php";

include "visualizer_widget.php";
$game_id = filter_input(INPUT_GET, 'game', FILTER_VALIDATE_INT);
$user_id = filter_input(INPUT_GET, 'user', FILTER_VALIDATE_INT);
if ($game_id !== FALSE and $game_id !== NULL) {
    visualize_game($game_id);
    require_once('session.php');
    if ($user_id &&
            logged_in_with_valid_credentials() &&
            (logged_in_as_admin() || current_user_id() == $user_id)) {
        require_once('mysql_login.php');
    	if (logged_in_as_admin()) {
    		$game_errors = contest_query('select_game_all_errors', $game_id);
    		
    	} else {
	        $game_errors = contest_query('select_game_errors',
	                                     $game_id,
	                                     $user_id);
    	}
        while ($row = mysql_fetch_assoc($game_errors)) {
            $error_msg = "<ul>";
            $username = $row["username"];
            $status = $row["status"];
            $error_msg .= "<li><p>$username - $status</p><pre class=\"error\">";
            $error_msg .= str_replace('\n', "\n", $row["errors"])."\n";
            $error_msg .= "</pre></li>";
            $error_msg .= "</ul>";
            echo $error_msg;
        }
    }
    
} else {
    echo '<p>Incorrect Game Number</p>';
}

include "footer.php";

?>

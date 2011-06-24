<?php

function nice_interval($interval) {
    if ($interval->y > 0) {
        return $interval->format('%y yrs %m months');
    } elseif ($interval->m > 0) {
        return $interval->format('%m months %d days');
    } elseif ($interval->d > 0) {
        return $interval->format('%d days %h hrs');
    } elseif ($interval->h > 0) {
        return $interval->format('%h hrs %i min');
    } else {
        return $interval->format('%i min %s sec');
    }
}

function nice_ago($date) {
    $datetime = new DateTime($date);
    return nice_interval($now->diff($datetime))." ago";
}

function nice_datetime_span($date) {
    return "<span title=\"".no_wrap(nice_ago($date))."\">".no_wrap($time->format('j M g:ia'))."</span>";
}

function nice_date($date) {
    return date("M jS Y", strtotime($date));
}

function nice_version($version, $date) {
    return "<span title=\"".nice_ago($date)."\">$version</span>";
}

function nice_map($map) {
    $map_name = explode('.', $map, 1);
    $pos1 = strrpos($map,'/') + 1;
    $pos2 = strrpos($map,'.');
    $map_name = substr($map, $pos1, $pos2-$pos1);
    return "<span><a href=\"map.php?map=".$map."\">$map_name</a></span>";
}

function nice_ordinal($num) {
    switch ($num % 10) {
        case 1:
            return strval($num)."st";
            break;
        case 2:
            return strval($num)."nd";
            break;
        case 3:
            return strval($num)."rd";
            break;
        default:
            return strval($num)."th";
    }
}


function nice_change_marker($value, $cushion, $reverse=FALSE) {
    if ($value == NULL) {
        $arrow = "";
    } elseif ($value > $cushion) {
        $arrow = $reverse ? "&darr;" : "&uarr;";
    } elseif ($value < -$cushion) {
        $arrow = $reverse ? "&uarr;" : "&darr;";
    } else {
        $arrow = "&ndash;";
    }
    return "<span title=\"$value\">$arrow</span>";
}

function no_wrap($data) {
    return str_replace(" ", "&nbsp;", strval($data));
}

function nice_outcome($num, $total) {
    return "<span>".no_wrap(nice_ordinal($num)." of ".$total)."</span>";
}

function nice_user($user_id, $username) {
    return "<a href=\"profile.php?user=".$user_id."\">".$username."</a>";
}

function nice_opponent($user_id, $username, $rank, $user=False) {
    if ($user) {
        return "<span><strong>".nice_ordinal($rank)."</strong>-".nice_user($user_id, $username)."</span>";
    } else {
        return "<span>".nice_ordinal($rank)."-".nice_user($user_id, $username)."</span>";
    }
}

function nice_game($game_id, $turns, $winning_turn, $ranking_turn, $user_id=NULL) {
    $query_string = "";
    if ($user_id) {
        $query_string .= "&user=$user_id";
    }
    return "<a href=\"visualizer.php?game=$game_id$query_string\">$turns&nbsp;turns&nbsp;&raquo;</a>
    <span title=\"Turn the winner last took the lead\">
    	<a href=\"visualizer.php?game=$game_id$query_string&turn=$winning_turn\">Won&nbsp;at&nbsp;$winning_turn&nbsp;&raquo;</a>
    </span>";
    /*
    <span title=\"Turn the player ranks stopped changing\">
    	<a href=\"visualizer.php?game=$game_id$query_string&turn=$ranking_turn\">Rank&nbsp;$ranking_turn&nbsp;&raquo;</a>
    </span>";
    */
}

function nice_skill($skill, $mu, $sigma, $skill_change=NULL, $mu_change=NULL, $sigma_change=NULL) {
    $skill = number_format($skill, 2);
    $skill_change = nice_change_marker($skill_change, 0.1);
    if ($skill_change == NULL) {
        $skill_hint = sprintf("mu=%0.2f sigma=%0.2f", $mu, $sigma);
    } else {
        $skill_hint = sprintf("mu=%0.2f(%+0.2f) sigma=%0.2f(%+0.2f) skill=%0.2f(%+0.2f)", $mu, $mu_change, $sigma, $sigma_change, $skill, $skill_change);
    }
    return "<span title=\"$skill_hint\">$skill&nbsp;$skill_change</span>";
}

function nice_rank($rank, $rank_change, $filter_rank=NULL) {
    $rank_arrow = nice_change_marker($rank_change, 0, FALSE);
    if ($filter_rank) {
        $rank = str_pad("(".strval($rank).")", 6, " ", STR_PAD_LEFT);
        $filter_rank = str_pad(strval($filter_rank), 4, " ", STR_PAD_LEFT);
        return $filter_rank." <span title=\"Global Rank\">$rank $rank_arrow</span>";
    } else {
        $rank = str_pad(strval($rank), 4, " ", STR_PAD_LEFT);
        return "$rank $rank_arrow";
    }
}

?>

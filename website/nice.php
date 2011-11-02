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

function nice_ago($datetime) {
    if (is_string($datetime)) {
        $datetime = new DateTime($datetime);
    }
    $now = new DateTime();
    if ($now > $datetime ) {
        return nice_interval($now->diff($datetime))." ago";
    } else {
        return "in ".nice_interval($now->diff($datetime));
    }
}

function nice_datetime($datetime) {
    if (is_string($datetime)) {
        $datetime = new DateTime($datetime);
    }
    return no_wrap($datetime->format('M jS')) ." ".
        no_wrap($datetime->format('g:ia'));
}

function nice_datetime_span($datetime) {
    if (is_string($datetime)) {
        $datetime = new DateTime($datetime);
    }
    return "<span title=\"". no_wrap(nice_ago($datetime)) ."\">".
        nice_datetime($datetime)."</span>";
}

function nice_date($date) {
    return date("M jS", strtotime($date));
}

function nice_version($version, $date=NULL, $submission_id=NULL) {
    if ($submission_id !== NULL) {
        $version = "<a href=\"profile_games.php?submission=$submission_id\">$version</a>";
    }
    if ($date !== NULL) {
        $version = "<span title=\"".nice_ago($date)."\">$version</span>";
    }
    return $version;
}

function nice_map($map) {
    $map_name = explode('.', $map, 1);
    $pos1 = strrpos($map,'/');
    $pos1 = $pos1 ? $pos1 + 1 : 0;
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
        $arrow = "&nbsp;";
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
    $username = htmlentities($username, ENT_COMPAT, "UTF-8");
    return "<a href=\"profile.php?user=".$user_id."\">".$username."</a>";
}

function nice_organization($org_id, $org_name) {
    $org_name = htmlentities($org_name, ENT_COMPAT, 'UTF-8');
    return "<a href=\"organization_profile.php?org=$org_id\">$org_name</a>";
}

function nice_country($country_id, $country_name, $flag_filename) {
        $country_name = htmlentities($country_name, ENT_COMPAT, 'UTF-8');
        $flag_filename = "<img alt=\"$country_name\" width=\"16\" height=\"11\" title=\"$country_name\" src=\"flags/$flag_filename\" />";
        return "<a href=\"country_profile.php?country=$country_id\">$flag_filename</a>";
}

function nice_language($language_id, $programming_language) {
    $programming_language = htmlentities($programming_language, ENT_COMPAT, 'UTF-8');
    $programming_language_link = urlencode($programming_language);
    return "<a href=\"language_profile.php?language=$programming_language_link\">$programming_language</a>";
}

function skill_hint($skill, $mu, $sigma,
        $skill_change=NULL, $mu_change=NULL, $sigma_change=NULL) {
    if ($skill_change == NULL) {
        $skill_hint = sprintf("mu=%0.2f sigma=%0.2f", $mu, $sigma);
    } else {
        $skill_hint = sprintf("mu=%0.2f(%+0.2f) sigma=%0.2f(%+0.2f) skill=%0.2f(%+0.2f)", $mu, $mu_change, $sigma, $sigma_change, $skill, $skill_change);
    }
    return $skill_hint;
}

function nice_opponent($user_id, $username, $game_rank, $rank_before,
        $skill, $mu, $sigma, $skill_change, $mu_change, $sigma_change,
        $user=False) {
    $skill_hint = skill_hint($skill, $mu, $sigma, $skill_change, $mu_change, $sigma_change);
    if ($user) {
        return "<span><em title='$skill_hint'>#$rank_before-</em><strong>".nice_ordinal($game_rank)."</strong>-".nice_user($user_id, $username)."</span>";
    } else {
        return "<span><em title='$skill_hint'>#$rank_before-</em>".nice_ordinal($game_rank)."-".nice_user($user_id, $username)."</span>";
    }
}

function nice_game($game_id, $turns, $winning_turn, $ranking_turn, $end_reason,
        $user_id=NULL) {
    $query_string = "";
    if ($user_id) {
        $query_string .= "&user=$user_id";
    }
    return "<a href=\"visualizer.php?game=$game_id$query_string\">$turns&nbsp;turns, $end_reason&nbsp;&raquo;</a><br />
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
    $skill_hint = skill_hint($skill, $mu, $sigma,
        $skill_change, $mu_change, $sigma_change);
    $skill_change = nice_change_marker($skill_change, 0.1);
    $skill = number_format($skill, 2);
    return "<span title=\"$skill_hint\">$skill&nbsp;$skill_change</span>";
}

function nice_rank($rank, $rank_change, $filter_rank=NULL) {
    $rank_arrow = nice_change_marker($rank_change, 0, FALSE);
    if ($filter_rank) {
        $rank = str_replace(" ", "&nbsp;", str_pad("(".strval($rank).")", 6, " ", STR_PAD_LEFT));
        $filter_rank = str_replace(" ", "&nbsp;", str_pad(strval($filter_rank), 4, " ", STR_PAD_LEFT));
        return $filter_rank."&nbsp;<span title=\"Global Rank\">$rank&nbsp;$rank_arrow</span>";
    } else {
        $rank = str_replace(" ", "&nbsp;", str_pad(strval($rank), 4, " ", STR_PAD_LEFT));
        return "$rank&nbsp;$rank_arrow";
    }
}

?>

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

function nice_version($version, $date) {
    return "<span title=\"".nice_ago($date)."\">$version</span>";
}

function nice_map($map) {
    $map_name = explode('.', $map, 1);
    $pos1 = strrpos($map,'/') + 1;
    $pos2 = strrpos($map,'.');
    $map_name = substr($map, $pos1, $pos2-$pos1);
    return "<span><a href=\"map/".$map."\">$map_name</a></span>";
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

function no_wrap($data) {
    return str_replace(" ", "&nbsp;", strval($data));
}

function nice_outcome($num, $total) {
    return "<span>".no_wrap(nice_ordinal($num)." of ".$total)."</span>";
}

function nice_user($user_id, $username) {
    return "<a href=\"profile.php?user=".$user_id."\">".$username."</a>";
}

function nice_opponent($user_id, $username, $rank) {
    return "<span>($rank)".nice_user($user_id, $username)."</span>";
}

function nice_game($game_id, $user_id=NULL) {
    if ($user_id) {
        return "<a href=\"visualizer.php?game=$game_id\">Game&nbsp;&raquo;</a>";
    } else {
        return "<a href=\"visualizer.php?game=$game_id&user=$user_id\">Game&nbsp;&raquo;</a>";
    }
}

function nice_skill($skill, $mu, $sigma, $old_skill=NULL, $old_mu=NULL, $old_sigma=NULL) {

}

?>

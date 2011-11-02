<?php

require_once('mysql_login.php');
require_once('memcache.php');
require_once('pagination.php');
require_once('nice.php');

//require_once('api_functions.php');

//require_once('session.php');
// session is needed to highlight the current user
// but is not included here so that json requests go faster
// you must include it in your php if you wish to render html tables

// used for looking at latest results only with view more link
//  should be smaller than page size
$top_results_size = 10;
// used for looking at pages with pagination links
$page_size = 50;

// this function doesn't really belong here, but I can't think of a good place
// it works like filter_input with an optional filter and default value
function get_type_or_else($key, $type=NULL, $default=NULL) {
    if (isset($_GET[$key])) {
        $value = $_GET[$key];
        if ($type == NULL) {
            return $value;
        } else {
            $filter_value = filter_var($value, $type, FILTER_NULL_ON_FAILURE);
            if ($filter_value !== NULL) {
                return $filter_value;
            }
        }
    }
    return $default;
}

function get_last_game_id($type=NULL, $id=NULL) {
    global $memcache;
    $last_game_id = 0;
    if ($memcache) {
        if ($type === NULL) {
            $$last_game_id = $memcache->get('l:all');            
        } else {
            $last_game_id = $memcache->get('l:'.$type.':'.$id);
        }
        if ($last_game_id === False) {
            $last_game_id = 0;
        }
    }
    return $last_game_id;
}

function cache_key($page=0, $user_id=NULL, $submission_id=NULL, $map_id=NULL, $format='json') {
    $cache_length = 120;
    if ($user_id !== NULL) {
        $key = get_last_game_id('u', $user_id)."g:u:" . strval($user_id);
        $cache_length = 3600;
    } elseif ($submission_id !== NULL) {
        $key = get_last_game_id('s', $submission_id)."g:s:" . strval($submission_id);
        $cache_length = 3600;
    } elseif ($map_id !== NULL) {
        $key = get_last_game_id('m', $map_id)."g:m:" . strval($map_id);
        $cache_length = 3600;
    } else {
        $key = get_last_game_id()."g:all:";
    }
    return array($key. ":" . strval($page) . ":" . $format, $cache_length);
}


// page 0 is all results, page 1 is the top N results
function get_query_results($page=0, $user_id=NULL, $submission_id=NULL, $map_id=NULL, $top=FALSE) {
    global $page_size;
    global $top_results_size;

    $user_fields = array("user_id", "submission_id", "username", "version",
                         "player_id", "game_rank", "status", "skill", "mu", "sigma",
                         "skill_change", "mu_change", "sigma_change", "rank_before");
    
    $page_count_query = "select_game_list_page_count";
    $list_query = "select_game_list";
    $list_type = NULL;
    $list_id = 1;
    $list_id_field = NULL;
    $list_name = NULL;
    if ($user_id !== NULL) {
        $list_select_field = "user_id";
        $list_id = $user_id;
        $list_type = "user";
        $list_id_field = "username";
    } elseif ($submission_id !== NULL) {
        $list_select_field = "submission_id";
        $list_id = $submission_id;
        $list_type = "submission";
        $list_id_field = "submission_id";
    } elseif ($map_id !== NULL) {
        $page_count_query = "select_map_game_list_page_count";
        $list_query = "select_map_game_list";
        $list_select_field = "map_id";
        $list_id = $map_id;
        $list_type = "map";
        $list_id_field = "map_name";
    } else {
        // make the where clause always return true
        $page_count_query = "select_map_game_list_page_count";
        $list_query = "select_map_game_list";
        $list_select_field = "1";
    }
    $list_results = contest_query($page_count_query, $list_select_field, $list_id);
    if ($list_results) {
        while ($list_row = mysql_fetch_array($list_results, MYSQL_NUM)) {
            $row_count = $list_row[0];
            $page_count = ceil($row_count / $page_size);
        }
    }
    if ($top) {
        $offset = 0;
        $limit = $top_results_size;
        $page_count = 1;
    } elseif ($page === 0) {
        $offset = 0;
        $limit = $row_count;
    } else {
        $offset = ($page - 1) * $page_size;
        $limit = $page_size;
    }
    $results = contest_query($list_query, $list_select_field, $list_id, $limit, $offset);
    if ($results) {    
        // loop through results, turning multiple rows for the same game into arrays
        $rows = array();
        $last_game_id = -1;
        $cur_row = NULL;
        while ($row = mysql_fetch_assoc($results)) {
            // get list type name
            if ($list_type && !$list_name && $row[$list_select_field] == $list_id) {
                $list_name = $row[$list_id_field];                
            }               
            if ($last_game_id !== $row['game_id']) {
                if ($cur_row !== NULL) {
                    $rows[] = $cur_row;
                }
                $cur_row = $row;
                foreach ($user_fields as $user_field) {
                    $cur_row[$user_field] = array($cur_row[$user_field]);
                }
            } else {
                foreach ($user_fields as $user_field) {
                    $cur_row[$user_field][] = $row[$user_field];
                }
            }
            $last_game_id = $row['game_id'];
        }
        if ($cur_row !== NULL) {
            $rows[] = $cur_row;
        }
        return array($rows, $page_count, $list_type, $list_id, $list_name);
    } else {
        return array(NULL, NULL);
    }
}

function create_game_list_json($page=0, $user_id=NULL, $submission_id=NULL, $map_id=NULL) {    
    list ($rows, $page_count, $list_type, $list_id, $list_name) = get_query_results($page, $user_id, $submission_id, $map_id);
    
    if ($rows) {
        $json = array("fields" => array_keys($rows[0]),
                      "values" => array());
        foreach ($rows as $row) {
            $json["values"][] = array_values($row);
        }
        if ($page > 0) {
            $json["page"] = $page;
            $json["page_count"] = $page_count;
        }
        if ($list_type) {
            $json["type"] = $list_type;
            $json["type_id"] = $list_id;
            $json["type_name"] = $list_name;
        } else {
            $json["type"] = "all";
        }
        $json_result = json_encode($json);

        return $json_result;
    }
    return NULL;
}

function create_game_list_table($page=0, $user_id=NULL, $submission_id=NULL, $map_id=NULL, $top=FALSE, $targetpage=NULL) {
    global $page_size;
    
    // if cached copy does not exists, create html table
    list ($rows, $page_count, $list_type, $list_id, $list_name) = get_query_results($page, $user_id, $submission_id, $map_id, $top);
    
    if (!$rows) {
        return '<h4>There are no games at this time.  Please check back later.</h4>';
    }
            
    if ($list_type) {
        $page_string = '?'.$list_type.'='.$list_id.'&page=';
    } else {
        $page_string = '?page=';
    }

    $table = '<table class="games">';
    if (!$top) {
        $table .= '<caption>'.getPaginationString($page, $page_count, $page_size, $page_string)."</caption>";
    }
    // produce header
    $table .= '<thead><tr><th>Time</th>';
    if ($user_id) {
        $table .= '<th>Version</th><th>Skill</th>';
    }
    $table .= '<th>Opponents</th>';
    if ($user_id) {
        $table .= '<th>Outcome</th>';
        $table .= '<th>Status</th>';
    }
    $table .= '<th>Map</th><th>Viewer</th></tr></thead>';
    $table .= '<tbody>';
    $oddity = 'even';
    foreach ($rows as $row) {
        // find current user info
        if ($user_id) {
            for ($i = 0; $i < $row['players']; $i++) {
                if ($row["user_id"][$i] == $user_id) {
                    $user_version = $row["version"][$i];
                    $user_submission_id = $row["submission_id"][$i];
                    $user_skill = $row["skill"][$i];
                    $user_mu = $row["mu"][$i];
                    $user_sigma = $row["sigma"][$i];
                    $user_skill_change = $row["skill_change"][$i];
                    $user_mu_change = $row["mu_change"][$i];
                    $user_sigma_change = $row["sigma_change"][$i];
                    $user_rank = $row["game_rank"][$i];
                    $user_status = $row["status"][$i];
                    break;
                }
            }                
        }

        $oddity = $oddity == 'odd' ? 'even' : 'odd';  // quite odd?
        $user_class = current_username() == $row["username"] ? ' user' : '';
        $table .= "<tr class=\"$oddity$user_class\">";

        $time = new DateTime($row["timestamp"]);
        // $time = "<span title=\"".no_wrap(nice_interval($now->diff($time))." ago")."\">".no_wrap($time->format('j M G:i'))."</span>";
        $time = nice_datetime_span($time);
        $table .= "<td class=\"time\">$time</td>";

        if ($user_id) {
            $table .= "<td class=\"number\">".nice_version($user_version, NULL, $user_submission_id)."</td>";
            $table .= "<td class=\"number\">".nice_skill($user_skill, $user_mu, $user_sigma, $user_skill_change, $user_mu_change, $user_sigma_change)."</td>";
        }

        // TODO: consider linking the submission id instead
        $opponents = "";
        for ($i = 0; $i < $row['players']; $i++) {
            $opponents .= nice_opponent($row["user_id"][$i], $row["username"][$i], $row["game_rank"][$i] + 1, $row['rank_before'][$i],
                $row['skill'][$i], $row['mu'][$i], $row['sigma'][$i],
                $row['skill_change'][$i], $row['mu_change'][$i], $row['sigma_change'][$i],
                $row["user_id"][$i] == $user_id);
        }
        $table .= "<td class=\"list\">$opponents</td>";

        if ($user_id) {
            $outcome = nice_outcome($user_rank+1, $row['players']);
            $table .= "<td>$outcome</td>";

            $table .= "<td>$user_status</td>";
        }

        $map = nice_map($row['map_name']);
        $table .= "<td class=\"map\">$map</td>";

        $game = nice_game($row['game_id'], $row['game_length'], $row['winning_turn'], $row['ranking_turn'], $row['cutoff'], $user_id);
        $table .= "<td class=\"game\">$game</td>";

        $table .= "</tr>";
    }
    $table .= '</tbody>';
    if ($top) {
        if ($targetpage === NULL) {
    	    $targetpage = $_SERVER['PHP_SELF'];
        }
        $table .= '<caption align="bottom"><a href="'.$targetpage.$page_string.'1">View More &raquo;</a></caption>';
    }
    $table .= '</table>';
    if (!$top) {
        $table .= '<div style="text-align:center">'.getPaginationString($page, $page_count, $page_size, $page_string)."</div>";
    }
    
    return $table;
}

function get_game_list_json($page=0, $user_id=NULL, $submission_id=NULL, $map_id=NULL) {
    global $memcache;
    list ($cache_key, $cache_length) = cache_key($page, $user_id, $submission_id, $map_id, 'json');
    if ($memcache) {
        $json = $memcache->get($cache_key);
        if ($json) {
            return $json;
        }
    }
    $json = create_game_list_json($page, $user_id, $submission_id, $map_id);
    if ($memcache) {
        $memcache->set($cache_key, $json_result, MEMCACHE_COMPRESSED, $cache_length);
    }
    return $json;
}

function get_game_list_table($page=0, $user_id=NULL, $submission_id=NULL, $map_id=NULL, $top=FALSE, $targetpage=NULL) {
    global $memcache;
    $format = 'html';
    if ($top) {
        $format = 'top';
        $page = 0;
    }
    list ($cache_key, $cache_length) = cache_key($page, $user_id, $submission_id, $map_id, $format);
    if ($memcache) {
        $table = $memcache->get($cache_key);
        if ($table) {
            return $table;
        }
    }
    $table = create_game_list_table($page, $user_id, $submission_id, $map_id, $top, $targetpage);
    if ($memcache) {
        $memcache->set($cache_key, $table, MEMCACHE_COMPRESSED, $cache_length);
    }
    return $table;
}

function get_user_game_list($user_id, $page=1, $top=FALSE, $targetpage=NULL) {
    return get_game_list_table($page, $user_id, NULL, NULL, $top, $targetpage);
}

function get_submission_game_list($submission_id, $page=1, $top=FALSE, $targetpage=NULL) {
    return get_game_list_table($page, NULL, $submission_id, NULL, $top, $targetpage);
}

function get_map_game_list($map_id, $page=1, $top=FALSE, $targetpage=NULL) {
    return get_game_list_table($page, NULL, NULL, $map_id, $top, $targetpage);
}

?>

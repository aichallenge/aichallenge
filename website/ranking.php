<?php
//ini_set('error_reporting', E_ALL);
//ini_set('display_errors', true);

require_once('mysql_login.php');
require_once('memcache.php');
require_once('pagination.php');
require_once('nice.php');

$page_size = 100;
$last_game_id = 0;
if ($memcache)
    $last_game_id = $memcache->get('last_game_id');
if (!$last_game_id) {
    $last_game_id = 0;
}

//require_once('session.php');
// session is needed to highlight the current user
// but is not included here so that json requests go faster
// you must include it in your php if you wish to render html tables

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

function cache_key($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL, $format='json') {
    $cache_length = 120;    
    if ($org_id !== NULL) {
        $key = get_last_game_id('o',$org_id)."r:o:" . strval($org_id);
        $cache_length = 1440;
    } elseif ($country_id !== NULL) {
        $key = get_last_game_id('c',$country_id)."r:c:" . strval($country_id);
        $cache_length = 600;
    } elseif ($language_id !== NULL) {
        $key = get_last_game_id('l',$language_id)."r:l:" . strval($language_id);
    } else {
        $key = get_last_game_id()."r:a:";
    }
    return array($key . ":" . strval($page) . ":" . $format, $cache_length);
}

function create_ranking_json($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL) {
    global $page_size;

    // setup query and querystring
    $filtered = False;
    $where = "";
    $page_string = "";
    $rank_type = "all";
    $rank_id = NULL;
    $rank_id_field = NULL;
    $rank_id_name = NULL;
    if ($org_id !== NULL) {
        $filtered = True;
        $where .= " and u.org_id = ".$org_id;
        $rank_type = "org";
        $rank_id = $org_id;
        $rank_id_field = "org_name";
    }
    if ($country_id !== NULL) {
        $filtered = True;
        $where .= " and u.country_id = ".$country_id;
        $rank_type = "country";
        $rank_id = $country_id;
        $rank_id_field = "country";
    }
    if ($language_id !== NULL) {
        $filtered = True;
        $where .= " and s.language_id = ".$language_id;
        $rank_type = "language";
        $rank_id = $language_id;
        $rank_id_field = "programming_language";
    }
    if ($page === 0) {
        $limit = "";
    } else {
        $limit = "limit ".$page_size." offset ".($page_size * ($page-1));
    }
    // get count of rows and pages
    $results = contest_query("select_rankings_page_count", $where);
    if ($results) {
        while ($row = mysql_fetch_array($results, MYSQL_NUM)) {
            $row_count = $row[0];
            $page_count = ceil($row_count / $page_size);
        }
    } else {
        $page_count = 1000;
    }
    // get results
    $results = contest_query("select_rankings", $where, $limit);
        
    $json = array("type" => $rank_type,
                  "page" => $page,
                  "page_count" => $page_count);
    if ($rank_id !== NULL) {
        $json["type_id"] = $rank_id;
    }

    if (!$results) {
        return json_encode($json);
    } else {
        $field_count = mysql_num_fields($results);
        $row_count = mysql_num_rows($results);
        $field_names = array();
        for ($i = 0; $i < $field_count; $i++) {
            $field_names[] = mysql_field_name($results, $i);
        }
        if ($filtered) {
            $field_names[] = "filter_rank";
            $filter_rank = $page_size * ($page - 1);
        } else {
            $filter_rank = NULL;
        }   
        while ($rank_row = mysql_fetch_array($results, MYSQL_NUM)) {
            if ($filtered) {
                if ($row["rank"]) {
                    $filter_rank += 1;
                    $rank_row[] = $filter_rank;
                } else {
                    $rank_row[] = NULL;
                }
            }
            if ($filtered and !$rank_id_name) {
                $rank_row_by_name = array_combine($field_names, $rank_row);
                $rank_id_name = $rank_row_by_name[$rank_id_field];
                $json["type_name"] = $rank_id_name;
                $json["values"] = array();
            }
            $json["fields"] = $field_names;
            $json["values"][] = $rank_row;
        }
    }
    $json = json_encode($json);

    return $json;
}

function create_ranking_table($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL) {
    global $page_size;

    // setup query and querystring
    $filtered = False;
    $where = "";
    $page_string = "";
    if ($org_id !== NULL) {
        $filtered = True;
        $where .= " and u.org_id = ".$org_id;
        $page_string .= '&org='.$org_id;
    }
    if ($country_id !== NULL) {
        $filtered = True;
        $where .= " and u.country_id = ".$country_id;
        $page_string .= '&country='.$country_id;
    }
    if ($language_id !== NULL) {
        $filtered = True;
        $where .= " and s.language_id = ".$language_id;
        $page_string .= '&language='.$language_id;
    }
    $page_string .= "&page=";
    $page_string[0] = "?";
    if ($page === 0) {
        $limit = "";
    } else {
        $limit = "limit ".$page_size." offset ".($page_size * ($page-1));
    }
    // get count of rows and pages
    $results = contest_query("select_rankings_page_count", $where);
    if ($results) {
        while ($row = mysql_fetch_array($results, MYSQL_NUM)) {
            $row_count = $row[0];
            $page_count = ceil($row_count / $page_size);
        }
    } else {
        $page_count = 1000;
    }
    // get results
    $results = contest_query("select_rankings", $where, $limit);
    if (!$results) {
        return '<h4>There are no rankings at this time.  Please check back later.</h4>';
    }
    $table = '<table class="ranking">';
    if ($page !== 0) {
        $table .= '<caption>'.getPaginationString($page, $page_count, $page_size, $page_string)."</caption>";
    }
    // produce header
    $table .= '<thead>
<tr>
  <th>Rank</th>
  <th>Username</th>
  <th>Country</th>
  <th>Organization</th>
  <th>Language</th>
  <th>Version</th>
  <th><span title="mu - 3 * sigma">Skill</span></th>
  <th><span title="total games for current submission">Games</span></th>
  <th><span title="number of games in past 24 hours">Recent</span></th>
</tr>
</thead>';
    $table .= '<tbody>';
    $oddity = 'even';

    if ($filtered) {
        $filter_rank = $page_size * ($page - 1);
    } else {
        $filter_rank = NULL;
    }   
    while ($row = mysql_fetch_assoc($results)) {
        $oddity = $oddity == 'odd' ? 'even' : 'odd';  // quite odd?
        $user_class = current_username() == $row["username"] ? ' user' : '';
        $rank_class = $row["rank"] ? '' : ' old';
        $table .= "<tr class=\"$oddity$user_class$rank_class\">";

        $rank = $row["rank"];
        if ($row["rank"]) {
            if ($filtered) {
                $filter_rank += 1;
            }
            $table .= "<td class=\"number\">".nice_rank($row["rank"], $row["rank_change"], $filter_rank)."</td>";
        } else {
            $table .= "<td class=\"number\"><span title=\"best submission's last rank\">(&gt;\")&gt;</span></td>";
        }

        $table .= "<td class=\"username\">".nice_user($row["user_id"], $row["username"])."</td>";
        $table .= "<td class=\"country\">".nice_country($row["country_id"], $row["country"], $row["flag_filename"])."</td>";
        $table .= "<td class=\"org\">".nice_organization($row["org_id"], $row["org_name"])."</td>";
        
        $programming_language = htmlentities($row["programming_language"], ENT_COMPAT, 'UTF-8');
        $programming_language_link = urlencode($row["programming_language"]);
        $table .= "<td>".nice_language($row["language_id"], $row["programming_language"])."</td>";

        $version = $row["version"];
        $age = nice_ago($row["timestamp"]);
        $table .= "<td class=\"number\"><span title=\"$age\">$version</span></td>";

        $skill = nice_skill($row['skill'],$row['mu'],$row['sigma'],
                            $row['skill_change'],$row['mu_change'],$row['sigma_change']);
        $table .= "<td class=\"number\">$skill</td>";
        
        $table .= "<td class=\"number\">".$row["game_count"]."</td>";
        $table .= "<td class=\"number\">".$row["game_rate"]."</td>";
        
        $table .= "</tr>";
    }
    $table .= '</tbody>';
    $table .= '</table>';

    return $table;
}

function get_ranking_json($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL) {
    global $memcache;
    list ($cache_key, $cache_length) = cache_key($page, $org_id, $country_id, $language_id, 'json');
    if ($memcache) {
        $json = $memcache->get($cache_key);
        if ($json) {
            return $json;
        }
    }
    $json = create_ranking_json($page, $org_id, $country_id, $language_id);
    if ($memcache) {
        $memcache->set($cache_key, $json, MEMCACHE_COMPRESSED, $cache_length);
    }
    return $json;
}

function get_ranking_table($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL) {
    global $memcache;
    list($cache_key, $cache_length) = cache_key($page, $org_id, $country_id, $language_id, 'html');
    if ($memcache) {
        $table = $memcache->get($cache_key);
        if ($table) {
            return $table;
        }
    }
    $table = create_ranking_table($page, $org_id, $country_id, $language_id);
    if ($memcache) {
        $memcache->set($cache_key, $table, MEMCACHE_COMPRESSED, $cache_length);
    }    
    return $table;
}

function get_language_ranking($language_id, $page=1) {
    return get_ranking_table($page, NULL, NULL, $language_id);
}

function get_country_ranking($country_id, $page=1) {
    return get_ranking_table($page, NULL, $country_id);
}

function get_org_ranking($org_id, $page=1) {
    return get_ranking_table($page, $org_id);
}

?>

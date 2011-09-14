<?php
ini_set('error_reporting', E_ALL);
ini_set('display_errors', true);

require_once('mysql_login.php');
require_once('memcache.php');
require_once('pagination.php');
require_once('nice.php');

$page_size = 100;

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

function produce_cache_results($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL) {
    global $memcache;
    global $page_size;
    global $expiration_time;

    // setup query and querystring
    $rank_type = NULL;
    $where = "";
    $page_string = "";
    if ($org_id !== NULL) {
        $where .= " and o.org_id = ".$org_id;
    }
    if ($country_id !== NULL) {
        $where .= " and o.country_id = ".$country_id;
    }
    if ($language_id !== NULL) {
        $where .= " and l.language_id = ".$language_id;
    }
    if ($page === NULL) {
        $limit = "";
    } else {
        $limit = "limit ".$page_size." offset ".($page_size * $page);
    }
    // get count of rows and pages
    $results = contest_query("select_rankings_page_count", $where);
    if ($results) {
        while ($row = mysql_fetch_array($list_results, MYSQL_NUM)) {
            $row_count = $row[0];
            $page_count = ceil($row_count / $page_size);
        }
    }
    // get results
    $results = contest_query("select_rankings", $where, $limit);

    $json = array("fields" => array(),
                  "values" => array());
    if ($org_id !== NULL) {
        $rank_results = contest_query("select_rankings_by_org", $org_id);
        $rank_type = "org";
        $rank_id = $org_id;
        $rank_id_field = "org_name";
    } elseif ($country_id !== NULL) {
        $rank_results = contest_query("select_rankings_by_country", $country_id);
        $rank_type = "country";
        $rank_id = $country_id;
        $rank_id_field = "country";
    } elseif ($language_id !== NULL) {
        $rank_results = contest_query("select_rankings_by_language", $language_id);
        $rank_type = "language";
        $rank_id = $language_id;
        $rank_id_field = "programming_language";
    } else {
        $rank_results = contest_query("select_rankings");
        $rank_type = NULL;
    }
    $rank_id_name = NULL;

    if ($rank_results) {
        $field_count = mysql_num_fields($rank_results);
        $row_count = mysql_num_rows($rank_results);
        $page_count = ceil($row_count / $page_size);
        $field_names = array();
        for ($i = 0; $i < $field_count; $i++) {
            $field_names[] = mysql_field_name($rank_results, $i);
        }
        if ($rank_type !== NULL) {
            $field_names[] = "filter_rank";
            $filter_rank_ix = array_search("filter_rank", $field_names);
            $rank_ix = array_search("rank", $field_names);
            $filter_rank = 0;
        }
        $json["fields"] = $field_names;
        $row_num = 0;
        $page_num = 0;
        while ($rank_row = mysql_fetch_array($rank_results, MYSQL_NUM)) {
            if ($rank_type and !$rank_id_name) {
                $rank_row_by_name = array_combine($field_names, $rank_row);
                $rank_id_name = $rank_row_by_name[$rank_id_field];
            }
            if ($row_num % $page_size == 0) {
                // dump results of page
                if ($page_num > 0) {
                    $memcache->set(cache_key($page_num, $org_id, $country_id, $language_id),
                        json_encode($json_page), 0,
                        $expiration_time + 1);
                }
                // setup next page
                if ($row_num < $row_count) {
                    $page_num++;
                    $json_page = array("fields" => array(),
                                       "values" => array(),
                                       "page" => $page_num,
                                       "page_count" => $page_count);
                    $json_page["fields"] = $field_names;
                    if ($rank_type) {
                        $json_page["type"] = $rank_type;
                        $json_page["type_id"] = $rank_id;
                        $json_page["type_name"] = $rank_id_name;
                    }
                }
            }
            $row_num++;
            if ($rank_type !== NULL) {
                if ($rank_row[$rank_ix] !== NULL) {
                    $filter_rank++;
                    $rank_row[$filter_rank_ix] = $filter_rank;
                } else {
                    $rank_row[$filter_rank_ix] = NULL;
                }
            }
            $json["values"][] = $rank_row;
            $json_page["values"][] = $rank_row;
        }
        // dump results
        if ($rank_type) {
            $json["type"] = $rank_type;
            $json["type_id"] = $rank_id;
            $json["type_name"] = $rank_id_name;
        } else {
            $json["type"] = "all";
        }
        if (isset($json_page)) {
            $memcache->set(cache_key($page_num, $org_id, $country_id, $language_id),
                           json_encode($json_page), 0, $expiration_time + 1);
        }
        $memcache->set(cache_key(-1, $org_id, $country_id, $language_id),
                       $page_num, 0, $expiration_time + 1);
        $memcache->set(cache_key(0, $org_id, $country_id, $language_id),
                       json_encode($json), 0, $expiration_time + 1);
        return $memcache->get(cache_key($page, $org_id, $country_id, $language_id));
    }
    return NULL;
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
        echo mysql_error();
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
  <th>Skill</th>
  <th>Games</th>
  <th>Rate</th>
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
            $table .= "<td class=\"number\"><span title=\"best submission's last rank\">(>\")></span>";
        }

        $table .= "<td class=\"username\">".nice_user($row["user_id"], $row["username"])."</td>";
        $table .= "<td class=\"country\">".nice_country($row["country_id"], $row["country"], $row["flag_filename"])."</td>";
        $table .= "<td class=\"org\">".nice_organization($row["org_id"], $row["org_name"])."</td>";
        
        $programming_language = htmlentities($row["programming_language"], ENT_COMPAT, 'UTF-8');
        $programming_language_link = urlencode($row["programming_language"]);
        $table .= "<td>".nice_language($row["language_id"], $row["programming_language"])."</td>";

        $version = $row["version"];
        $table .= "<td class=\"number\">$version</td>";

        $skill = nice_skill($row['skill'],$row['mu'],$row['sigma'],
                            $row['skill_change'],$row['mu_change'],$row['sigma_change']);
        $table .= "<td class=\"number\">$skill</td>";
        
        $games = $row["game_count"];
        $table .= "<td class=\"number\">$games</td>";
        $game_rate = "<span title=\"average minutes between games\">".round($row["game_rate"], 0)."</span>";
        $table .= "<td class=\"number\">$game_rate</td>";
        
        $table .= "</tr>";
    }
    $table .= '</tbody>';
    $table .= '</table>';

    return $table;
}

function get_ranking_json($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL) {
    global $memcache;
    global $no_cache_results;

    $cache_key = cache_key($page, $org_id, $country_id, $language_id);
    if ($memcache) {
        $results = $memcache->get($cache_key);
    }
    if ($no_cache_results) {
        $results = NULL; // use to force data refresh when debugging
    }
    if (!$results) {
        $results = produce_cache_results($page, $org_id, $country_id, $language_id);
    }
    return $results;
}

function get_ranking_table($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL) {
    return create_ranking_table($page, $org_id, $country_id, $language_id);
}

/*
function get_ranking_table($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL) {
    global $memcache;
    global $no_cache_results;
    global $expiration_time;

    $cache_key = cache_key($page, $org_id, $country_id, $language_id, 'table');
    if ($memcache) {
        $results = $memcache->get($cache_key);
    }
    if ($no_cache_results) {
        $results = NULL; // use to force data refresh when debugging
    }
    if (!$results) {
        $results = create_ranking_table(json_decode(get_ranking_json($page, $org_id, $country_id, $language_id), true));
        if ($memcache) {
            $memcache->set($cache_key, $results, 0, $expiration_time + 1);
        }
    }
    return $results;
}
*/

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

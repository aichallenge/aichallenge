<?php

require_once('mysql_login.php');
require_once('memcache.php');
require_once('pagination.php');

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

function cache_key($page=0, $user_id=NULL, $submission_id=NULL, $map_id=NULL, $format='json') {
    if ($page == -1) {
        $page = "page_count";
    }
    if ($user_id !== NULL) {
        $key = "game_list:user:" . strval($user_id);
    } elseif ($submission_id !== NULL) {
        $key = "game_list:submission:" . strval($submission_id);
    } elseif ($map_id !== NULL) {
        $key = "game_list:map:" . strval($map_id);
    } else {
        $key = "game_list:all::";
    }
    return $key. ":" . strval($page) . ":" . $format;
}

function produce_cache_results($page=0, $user_id=NULL, $submission_id=NULL, $map_id=NULL) {
    global $memcache;
    
    $page_size = 10;
    $json = array("fields" => array(),
                  "values" => array());
    if ($user_id !== NULL) {
        $list_results = contest_query("select_game_list_by_user", $user_id);
        $list_type = "user";
        $list_id = $user_id;
        $list_id_field = "username";
    } elseif ($submission_id !== NULL) {
        $list_results = contest_query("select_game_list_by_submission", $submission_id);
        $list_type = "submission";
        $list_id = $submission_id;
        $list_id_field = "submission_id";
    } elseif ($map_id !== NULL) {
        $list_results = contest_query("select_game_list_by_map", $map_id);
        $list_type = "map";
        $list_id = $map_id;
        $list_id_field = "map_id";
    } else {
        $list_results = contest_query("select_game_list");
        $list_type = NULL;
    }
    $list_id_name = NULL;
    if ($list_results) {
        $field_count = mysql_num_fields($list_results);
        $row_count = mysql_num_rows($list_results);
        $page_count = ceil($row_count / $page_size);
        $field_names = array();
        for ($i = 0; $i < $field_count; $i++) {
            $field_names[] = mysql_field_name($list_results, $i);
        }
        $json["fields"] = $field_names;
        if ($user_id !== NULL) {
            $json["fields"][] = 'user_mu';
            $json["fields"][] = 'user_rank';
        }
        $row_num = 0;
        $game_row_num = 0;
        $page_num = 0;
        $last_game_id = -1;
        $cur_row = NULL;
        while ($list_row = mysql_fetch_array($list_results, MYSQL_NUM)) {
            // get list name, run once
            if ($list_type and !$list_id_name) {
                $list_row_by_name = array_combine($field_names, $list_row);
                $list_id_name = $list_row_by_name[$list_id_field];
            }
            // get additional opponent info
            if ($list_row[0] == $last_game_id) {
                $cur_row[2][] = $list_row[2];
                $cur_row[3][] = $list_row[3];
                $cur_row[4][] = $list_row[4];
                $cur_row[5][] = $list_row[5];
                $cur_row[6][] = $list_row[6];
                $cur_row[7][] = $list_row[7];
                $cur_row[8][] = $list_row[8];                
                if ($user_id !== NULL) {
                    if ($list_row[2] == $user_id) {
                        $cur_row[] = $list_row[5];
                        $cur_row[] = $list_row[8];
                    }
                }
            } else {
            // get new game info
                // dump results of row
                if ($cur_row !== NULL) {
                    $json["values"][] = $cur_row;
                    $json_page["values"][] = $cur_row;
                    $game_row_num++;
                }
                if ($game_row_num % $page_size == 0) {
                    // dump results of page
                    if ($page_num > 0) {
                        $memcache->set(cache_key($page_num, $user_id, $submission_id, $map_id),
                                       json_encode($json_page));
                    }
                    // setup next page
                    if ($row_num < $row_count) {
                        $page_num++;
                        $json_page = array("fields" => array(),
                                           "values" => array(),
                                           "page" => $page_num,
                                           "page_count" => $page_count);
                        $json_page["fields"] = $field_names;
                        if ($user_id !== NULL) {
                            $json_page["fields"][] = 'user_mu';
                            $json_page["fields"][] = 'user_rank';
                        }
                        if ($list_type) {
                            $json_page["type"] = $list_type;
                            $json_page["type_id"] = $list_id;
                            $json_page["type_name"] = $list_id_name;
                        }
                    }
                }
                // setup new row
                $row_num++;
                $cur_row = $list_row;
                $cur_row[2] = array($cur_row[2]);
                $cur_row[3] = array($cur_row[3]);
                $cur_row[4] = array($cur_row[4]);
                $cur_row[5] = array($cur_row[5]);
                $cur_row[6] = array($cur_row[6]);
                $cur_row[7] = array($cur_row[7]);
                $cur_row[8] = array($cur_row[8]);
                if ($user_id !== NULL) {
                    if ($list_row[2] == $user_id) {
                        $cur_row[] = $list_row[5];
                        $cur_row[] = $list_row[8];
                    }
                }
            }
            $last_game_id = $list_row[0];
        }
        // dump results
        if ($list_type) {
            $json["type"] = $list_type;
            $json["type_id"] = $list_id;
            $json["type_name"] = $list_id_name;
        } else {
            $json["type"] = "all";
        }
        if (isset($json_page)) {
            $memcache->set(cache_key($page_num, $user_id, $submission_id, $map_id),
                           json_encode($json_page));
        }
        $memcache->set(cache_key(-1, $user_id, $submission_id, $map_id),
                       $page_num);
        $memcache->set(cache_key(0, $user_id, $submission_id, $map_id),
                       json_encode($json));
        return $memcache->get(cache_key($page, $user_id, $submission_id, $map_id));
    }
    return NULL;
}

function create_game_list_table($json) {
    if ($json == NULL) {
        return '<h4>There are no rankings at this time.  Please check back later.</h4>';
    }
    if ($json['type'] == 'user') {
        $user_id = $json['type_id'];
    } else {
        $user_id = NULL;
    }
    $table = '<table class="ranking">';
    if (array_key_exists('type', $json)) {
        // language by name, others by id
        if ($json['type'] == 'language') {
            $page_string = '?'.$json['type'].'='.$json['type_name'].'&page=';
        } else {
            $page_string = '?'.$json['type'].'='.$json['type_id'].'&page=';
        }
    } else {
        $page_string = '?page=';
    }
    $table .= '<caption>'.getPaginationString($json['page'], $json['page_count'], 10, $page_string)."</caption>";
    // produce header
    $table .= '<thead><tr><th>Time</th>';
    if ($user_id) {
        $table .= '<th>Skill</th>';
    }
    $table .= '<th>Opponents</th>';
    if ($user_id) {
        $table .= '<th>Outcome</th>';
    }
    $table .= '<th>Map</th><th>Viewer</th></tr></thead>';
    if (count($json["values"]) > 0) {
        $table .= '<tbody>';
        $oddity = 'even';
        $fields = $json["fields"];
        foreach ($json["values"] as $values) {
            $row = array_combine($fields, $values);
            
            // $rank = ($filter == null)? $rank : ($i + $offset) . " <span title='Global Rank'>($rank)</span>";
            // $rank_percent = $row["rank_percent"];
            
            $oddity = $oddity == 'odd' ? 'even' : 'odd';  // quite odd?
            $user_class = current_username() == $row["username"] ? ' user' : '';
            $table .= "<tr class=\"$oddity$user_class\">";
                    
            $time = $row["timestamp"];
            $table .= "<td>$time</td>";
    
            if ($user_id) {
                $skill = $row["user_mu"];
                $table .= "<td>$skill</td>";
            }
    
            // TODO: consider linking the submission id instead
            $opponents = "";
            for ($i = 0; $i < $row['players']; $i++) {
                $opponents .= "(".($row['game_rank'][$i]+1).")<a href=\"profile.php?user="
                              .$row['user_id'][$i]."\">".
                              $row['username'][$i]."</a> ";
            }
            $table .= "<td>$opponents</td>";
    
            if ($user_id) {
                $outcome = ($row['user_rank']+1)." of ".$row['players'];
                $table .= "<td>$outcome</td>";
            }
            $map_name = explode('.', $row['map_name'], 1);
            $pos1 = strrpos($row['map_name'],'/') + 1;
            $pos2 = strrpos($row['map_name'],'.');
            $map_name = substr($row['map_name'], $pos1, $pos2-$pos1);
            $map = "<a href=\"map/".$row['map_name']."\">$map_name</a>";
            $table .= "<td>$map</td>";
            
            $game = "<a href=\"visualizer.php?game=" . $row['game_id'] . "\">View Game &raquo;</a>";
            $table .= "<td>$game</td>";
            
            $table .= "</tr>";
        }
        $table .= '</tbody>';
    }
    $table .= '</table>';
    
    return $table;
}

function get_game_list_json($page=0, $user_id=NULL, $submission_id=NULL, $map_id=NULL) {
    global $memcache;
    
    $cache_key = cache_key($page, $user_id, $submission_id, $map_id);
    if ($memcache) {
        $results = $memcache->get($cache_key);
    }
    $results = NULL; // use to force data refresh when debugging
    if (!$results) {
        $results = produce_cache_results($page, $user_id, $submission_id, $map_id);
    }
    return $results;
}

function get_game_list_table($page=0, $user_id=NULL, $submission_id=NULL, $map_id=NULL) {
    global $memcache;
    
    $cache_key = cache_key($page, $user_id, $submission_id, $map_id, 'table');
    if ($memcache) {
        $results = $memcache->get($cache_key);
    }
    $results = NULL; // use to force data refresh when debugging
    if (!$results) {
        $results = create_game_list_table(json_decode(get_game_list_json($page, $user_id, $submission_id, $map_id), true));
        if ($memcache) {
            $memcache->set($cache_key, $results);
        }
    }
    return $results;
}

function get_country_row($country) {
    global $memcache;    

    $country_row_by_id = NULL;
    if ($memcache) {
        $country_row_by_id = $memcache->get('lookup:country_id');
        $country_row_by_name = $memcache->get('lookup:country_name');
        $country_row_by_code = $memcache->get('lookup:country_code');
    }
    $country_row_by_id = NULL;
    if (!$country_row_by_id) {
        $country_result = contest_query("select_countries");
        if ($country_result) {
            $country_row_by_id = array();
            $country_row_by_name = array();
            $country_row_by_code = array();
            while ($country_row = mysql_fetch_assoc($country_result)) {
                $country_row_by_id[$country_row['country_id']] = $country_row;
                $country_row_by_name[$country_row['name']] = $country_row;
                $country_row_by_code[$country_row['country_code']] = $country_row;
            }
            if ($memcache) {
                $memcache->set('lookup:country_id', $country_row_by_id);
                $memcache->set('lookup:country_name', $country_row_by_name);
                $memcache->set('lookup:country_code', $country_row_by_code);
            }
        }
    }

    // search by id, code, then name
    if ($country_row_by_id) {
        if (array_key_exists($country, $country_row_by_id)) {
            return $country_row_by_id[$country];
        }
    }
    if ($country_row_by_code) {
        if (array_key_exists($country, $country_row_by_code)) {
            return $country_row_by_code[$country];
        }
    }
    if ($country_row_by_name) {
        if (array_key_exists($country, $country_row_by_name)) {
            return $country_row_by_name[$country];
        }
    }
    return NULL;
}

function get_language_row($language) {
    global $memcache;    

    $language_row_by_id = NULL;
    if ($memcache) {
        $language_row_by_id = $memcache->get('lookup:language_id');
        $language_row_by_name = $memcache->get('lookup:language_name');
    }
    $language_row_by_id = NULL;
    if (!$language_row_by_id) {
        $language_result = contest_query("select_languages");
        if ($language_result) {
            $language_row_by_id = array();
            $language_row_by_name = array();
            while ($language_row = mysql_fetch_assoc($language_result)) {
                $language_row_by_id[$language_row['language_id']] = $language_row;
                $language_row_by_name[$language_row['name']] = $language_row;
            }
            if ($memcache) {
                $memcache->set('lookup:language_id', $language_row_by_id);
                $memcache->set('lookup:language_name', $language_row_by_name);
            }
        }
    }

    // search by id, then name
    if ($language_row_by_id) {
        if (array_key_exists($language, $language_row_by_id)) {
            return $language_row_by_id[$language];
        }
    }
    if ($language_row_by_name) {
        if (array_key_exists($language, $language_row_by_name)) {
            return $language_row_by_name[$language];
        }
    }
    return NULL;
}

function get_org_row($org) {
    global $memcache;    

    $org_row_by_id = NULL;
    if ($memcache) {
        $org_row_by_id = $memcache->get('lookup:org_id');
        $org_row_by_name = $memcache->get('lookup:org_name');
    }
    $org_row_by_id = NULL;
    if (!$org_row_by_id) {
        $org_result = contest_query("select_organizations");
        if ($org_result) {
            $org_row_by_id = array();
            $org_row_by_name = array();
            while ($org_row = mysql_fetch_assoc($org_result)) {
                $org_row_by_id[$org_row['org_id']] = $org_row;
                $org_row_by_name[$org_row['name']] = $org_row;
            }
            if ($memcache) {
                $memcache->set('lookup:org_id', $org_row_by_id);
                $memcache->set('lookup:org_name', $org_row_by_name);
            }
        }
    }

    // search by id, then name
    if ($org_row_by_id) {
        if (array_key_exists($org, $org_row_by_id)) {
            return $org_row_by_id[$org];
        }
    }
    if ($org_row_by_name) {
        if (array_key_exists($org, $org_row_by_name)) {
            return $org_row_by_name[$org];
        }
    }
    return NULL;
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
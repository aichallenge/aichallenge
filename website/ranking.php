<?php

require_once('mysql_login.php');
require_once('memcache.php');
require_once('pagination.php');

//require_once('session.php');
// session is needed to highlight the current user
// but is not included here so that json requests go faster
// you must include it in your php if you wish to render html tables

// this function doesn't really belong here, but I can't think of a good place
function get_type_or_else($key, $type, $default=NULL) {
    if (!empty($_GET[$key])) {
        $value = $_GET[$key];
        if(filter_var($value, $type)) {
          return $value;
        }
    }
    return $default;
}

function cache_key($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL, $format='json') {
    if ($page == -1) {
        $page = "page_count";
    }
    if ($org_id) {
        $key = "ranking:organization:" . strval($org_id);
    } elseif ($country_id) {
        $key = "ranking:country:" . strval($country_id);
    } elseif ($language_id) {
        $key = "ranking:language:" . strval($language_id);
    } else {
        $key = "ranking:all::";
    }
    return $key. ":" . strval($page) . ":" . $format;
}

function get_cache_results($cache_key) {
    global $memcache;
    if ($memcache) {
        $cache = $memcache->get($cache_key);
        if ($cache) {
            return $cache;
        }
    }
    return NULL;
}

function produce_cache_results($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL) {
    global $memcache;
    
    $page_size = 10;
    $json = array("fields" => array(),
                  "values" => array());
    if ($org_id) {
        $rank_results = contest_query("select_rankings_by_org", $org_id);
        $rank_type = "organization";
        $rank_id = $org_id;
        $rank_id_field = "org_name";
    } elseif ($country_id) {
        $rank_results = contest_query("select_rankings_by_country", $country_id);
        $rank_type = "country";
        $rank_id = $country_id;
        $rank_id_field = "country";
    } elseif ($language_id) {
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
                    if ($rank_type) {
                        $json["type"] = $rank_type;
                        $json["type_id"] = $rank_id;
                        $json["type_name"] = $rank_id_name;
                    }
                }
            }
            $row_num++;
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
        $memcache->set(cache_key($page_num, $org_id, $country_id, $language_id),
                       json_encode($json_page));
        $memcache->set(cache_key(-1, $org_id, $country_id, $language_id),
                       $page_num);
        $memcache->set(cache_key(0, $org_id, $country_id, $language_id),
                       json_encode($json));
        return $memcache->get(cache_key($page, $org_id, $country_id, $language_id));
    }
    return NULL;
}

function create_ranking_table($json) {
    $table = '<table class="ranking">';
    $table .= getPaginationString($page, $json["page_count"], 10);    
    $table .= '<caption>
    <span class="right"></span>
    <span class="left">Page 0 of 0</span>
</caption>';
    // produce header
    $table .= '<thead>
<tr>
  <th>Rank</th>
  <th>Username</th>
  <th>Country</th>
  <th>Organization</th>
  <th>Language</th>
  <th>Skill</th>
</tr>
</thead>';
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
                
        $rank = $row["rank"];
        $table .= "<td>$rank</td>";

        $user_id = $row["user_id"];
        $username = htmlentities($row["username"]);
        $table .= "<td><a href=\"profile.php?user_id=$user_id\">$username</a></td>";

        $country_id = $row["country_id"];
        $country_name = htmlentities($row["country_name"]);
        $flag_filename = $row["flag_filename"];
        $flag_filename = "<img alt=\"$country_name\" width=\"16\" height=\"11\" title=\"$country_name\" src=\"flags/$flag_filename\" />";
        $table .= "<td><a href=\"country_profile.php?country_id=$country_id\">$flag_filename</a></td>";

        $org_name = htmlentities($row["org_name"]);
        $org_id = $row["org_id"];
        $table .= "<td><a href=\"organization_profile.php?org_id=$org_id\">$org_name</a></td>";
        
        $programming_language = htmlentities($row["programming_language"]);
        $programming_language_link = urlencode($row["programming_language"]);
        $table .= "<td><a href=\"language_profile.php?lang=$programming_language_link\">$programming_language</a></td>";
        
        $skill = $row["skill"];
        $table .= "<td>$skill</td>";
        
        $table .= "</tr>";
    }
    $table .= '</tbody>';
    $table .= '</table>';
    
    return $table;
}

function get_ranking_json($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL) {
    global $memcache;
    
    $cache_key = cache_key($page, $org_id, $country_id, $language_id);
    if ($memcache) {
        $results = $memcache->get($cache_key);
    }
    // $results = NULL; // use to force data refresh when debugging
    if (!$results) {
        $results = produce_cache_results($page, $org_id, $country_id, $language_id);
    }
    return $results;
}

function get_ranking_table($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL) {
    global $memcache;
    
    $cache_key = cache_key($page, $org_id, $country_id, $language_id, 'table');
    if ($memcache) {
        $results = $memcache->get($cache_key);
    }
    // $results = NULL; // use to force data refresh when debugging
    if (!$results) {
        $results = create_ranking_table(json_decode(get_ranking_json($page, $org_id, $country_id, $language_id), true));
        if ($memcache) {
            $memcache->set($cache_key, $results);
        }
    }
    return $results;
}

?>
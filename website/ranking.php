<?php

require_once('mysql_login.php');
require_once('memcache.php');
header("Content-type: application/json");

function get_type_or_else($key, $type, $default=NULL) {
    if (!empty($_GET[$key])) {
        $value = $_GET[$key];
        if(filter_var($value, $type)) {
          return $value;
        }
    }
    return $default;
}

function cache_key($page=0, $org_id=NULL, $country_id=NULL, $language_id=NULL) {
    if ($page == -1) {
        $page = "page_count";
    }
    if ($org_id) {
        return "ranking:organization:" . ":" . strval($org_id) . ":" . strval($page);
    } else if ($country_id) {
        return "ranking:country:" . ":" . strval($country_id) . ":" . strval($page);
    } else if ($language_id) {
        return "ranking:language" . ":" . strval($language_id) . ":" . strval($page);
    } else {
        return "ranking:" . ":" . strval($page);
    }
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
    $json_page = array("fields" => array(),
                       "values" => array());
    if ($org_id) {
        $rank_results = contest_query("select_rankings_by_org", $org_id);
    } else if ($country_id) {
        $rank_results = contest_query("select_rankings_by_country", $country_id);
    } else if ($language_id) {
        $rank_results = contest_query("select_rankings_by_language", $language_id);
    } else {
        $rank_results = contest_query("select_rankings");
    }
    if ($rank_results) {
        $field_count = mysql_num_fields($rank_results);
        $row_count = mysql_num_rows($rank_results);
        $page_count = $row_count / $page_size;
        $field_names = array();
        for ($i = 0; $i < $field_count; $i++) {
            $field_names[] = mysql_field_name($rank_results, $i);
        }
        $json["fields"] = $field_names;
        $row_num = 0;
        $page_num = 0;
        while ($rank_row = mysql_fetch_array($rank_results, MYSQL_NUM)) {
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
                                       "page" => $page_num);
                    $json_page["fields"] = $field_names;
                }
            }
            $row_num++;
            $json["values"][] = $rank_row;
            $json_page["values"][] = $rank_row;
        }
        // dump results
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

$org_id = get_type_or_else("org_id", FILTER_VALIDATE_INT);
$country_id = get_type_or_else("country_id", FILTER_VALIDATE_INT);
$language_id = get_type_or_else("language_id", FILTER_VALIDATE_INT);
$page = get_type_or_else("page", FILTER_VALIDATE_INT, 0);
$cache_key = cache_key($page, $org_id, $country_id, $language_id);

if ($memcache) {
    $results = $memcache->get($cache_key);
}
if (!$results) {
    $results = produce_cache_results($page, $org_id, $country_id, $language_id);
}

#echo $cache_key;
echo $results;

?>
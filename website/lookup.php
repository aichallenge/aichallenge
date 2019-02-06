<?php

require_once('mysql_login.php');
require_once('memcache.php');

function get_country_row($country) {
    global $memcache;

    $country_row_by_id = NULL;
    if ($memcache) {
        $country_row_by_id = $memcache->get('lookup:country_id');
        $country_row_by_name = $memcache->get('lookup:country_name');
        $country_row_by_code = $memcache->get('lookup:country_code');
    }
    if (!$country_row_by_id) {
        $country_result = contest_query("select_countries");
        if ($country_result) {
            $country_row_by_id = array();
            $country_row_by_name = array();
            $country_row_by_code = array();
            while ($country_row = mysqli_fetch_assoc($country_result)) {
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
    if (!$language_row_by_id) {
        $language_result = contest_query("select_languages");
        if ($language_result) {
            $language_row_by_id = array();
            $language_row_by_name = array();
            while ($language_row = mysqli_fetch_assoc($language_result)) {
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
    if (!$org_row_by_id) {
        $org_result = contest_query("select_organizations");
        if ($org_result) {
            $org_row_by_id = array();
            $org_row_by_name = array();
            while ($org_row = mysqli_fetch_assoc($org_result)) {
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

function get_user_row($user) {
    global $memcache;

    $user_row_by_id = NULL;
    if ($memcache) {
        $user_row_by_id = $memcache->get('lookup:user_id');
        $user_row_by_name = $memcache->get('lookup:username');
    }
    if (!$user_row_by_id) {
        $user_result = contest_query("select_users");
        if ($user_result) {
            $user_row_by_id = array();
            $user_row_by_name = array();
            while ($user_row = mysqli_fetch_assoc($user_result)) {
                $user_row_by_id[$user_row['user_id']] = $user_row;
                $user_row_by_name[$user_row['username']] = $user_row;
            }
            if ($memcache) {
                $memcache->set('lookup:user_id', $user_row_by_id);
                $memcache->set('lookup:username', $user_row_by_name);
            }
        }
    }

    // search by id, then name
    if ($user_row_by_id) {
        if (array_key_exists($user, $user_row_by_id)) {
            return $user_row_by_id[$user];
        }
    }
    if ($user_row_by_name) {
        if (array_key_exists($user, $user_row_by_name)) {
            return $user_row_by_name[$user];
        }
    }
    return NULL;
}

function search_user_row($search=NULL) {
    global $memcache;

    $user_row_by_id = NULL;
    if ($memcache) {
        $user_row_by_id = $memcache->get('lookup:user_id');
        $user_row_by_name = $memcache->get('lookup:username');
    }
    if (!$user_row_by_id) {
        $user_result = contest_query("select_users");
        if ($user_result) {
            $user_row_by_id = array();
            $user_row_by_name = array();
            while ($user_row = mysqli_fetch_assoc($user_result)) {
                $user_row_by_id[$user_row['user_id']] = $user_row;
                $user_row_by_name[$user_row['username']] = $user_row;
            }
            if ($memcache) {
                $memcache->set('lookup:user_id', $user_row_by_id);
                $memcache->set('lookup:username', $user_row_by_name);
            }
        }
    }

    // search user name
    $user_rows = array();
    if ($user_row_by_name) {
        if ($search === NULL) {
            return array_values($user_row_by_name);
        } elseif ($search) {
            foreach ($user_row_by_name as $username => $user_row) {
                if (stripos($username, $search) !== FALSE) {
                    $user_rows[] = $user_row;
                }
            }
        }
    }
    return $user_rows;
}

function get_user_by_submission($submission_id) {
    global $memcache;

    $user_row_by_submission_id = NULL;
    if ($memcache) {
        $user_row_by_submission_id = $memcache->get('lookup:submission_id');
    }
    if (!$user_row_by_submission_id) {
        $user_result = contest_query("select_submission_users");
        if ($user_result) {
            $user_row_by_submission_id = array();
            while ($user_row = mysqli_fetch_assoc($user_result)) {
                $user_row_by_submission_id[$user_row['submission_id']] = $user_row;
            }
            if ($memcache) {
                $memcache->set('lookup:submission_id',
                    $user_row_by_submission_id);
            }
        }
    }

    // search by id
    if ($user_row_by_submission_id) {
        if (array_key_exists($submission_id, $user_row_by_submission_id)) {
            return $user_row_by_submission_id[$submission_id];
        }
    }
    return NULL;
}

function get_map_row($map) {
    global $memcache;

    $map_row_by_id = NULL;
    if ($memcache) {
        $map_row_by_id = $memcache->get('lookup:map_id');
        $map_row_by_name = $memcache->get('lookup:mapname');
    }
    if (!$map_row_by_id) {
        $map_result = contest_query("select_maps");
        if ($map_result) {
            $map_row_by_id = array();
            $map_row_by_name = array();
            while ($map_row = mysqli_fetch_assoc($map_result)) {
                $map_row_by_id[$map_row['map_id']] = $map_row;
                $map_row_by_name[$map_row['filename']] = $map_row;
            }
            if ($memcache) {
                $memcache->set('lookup:map_id', $map_row_by_id);
                $memcache->set('lookup:mapname', $map_row_by_name);
            }
        }
    }

    // search by id, then name
    if ($map_row_by_id) {
        if (array_key_exists($map, $map_row_by_id)) {
            return $map_row_by_id[$map];
        }
    }
    if ($map_row_by_name) {
        if (array_key_exists($map, $map_row_by_name)) {
            return $map_row_by_name[$map];
        }
    }
    return NULL;
}

?>

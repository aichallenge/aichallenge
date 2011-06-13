<?php

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

function get_user_row($user) {
    global $memcache;

    $user_row_by_id = NULL;
    if ($memcache) {
        $user_row_by_id = $memcache->get('lookup:user_id');
        $user_row_by_name = $memcache->get('lookup:username');
    }
    $user_row_by_id = NULL;
    if (!$user_row_by_id) {
        $user_result = contest_query("select_users");
        if ($user_result) {
            $user_row_by_id = array();
            $user_row_by_name = array();
            while ($user_row = mysql_fetch_assoc($user_result)) {
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

?>

<?php
include 'header.php';

$user_id = $_GET["user_id"];
if(!filter_var($user_id, FILTER_VALIDATE_INT)) {
  $user_id = NULL;
 }

#require_once 'Zend/Cache.php';

//$frontendOptions = array('lifeTime' => 120, 'automatic_serialization' => true, 'automatic_cleaning_factor' => 10);
//$backendOptions = array('cache_file_umask' => '777', 'file_name_prefix' => 'ai_contest', 'cacheDir' => '/tmp/');

//$cache = Zend_Cache::factory('Output', 'File', $frontendOptions, $backendOptions);

//$cacheID="profile_$user_id";

//if (!($cache->start($cacheID))) {
include_once 'profile_submissions_widget.php';
include_once 'profile_games_widget.php';

// Fetch Rank Data
$rankquery = <<<EOT
select
    r.rank
from
    rankings r
    inner join submissions s on s.submission_id = r.submission_id
where
    s.user_id = '$user_id' and
  leaderboard_id = (select max(leaderboard_id) from leaderboards)
EOT;
$rankresult = mysql_query($rankquery);

 // Fetch User Data
$userquery = <<<EOT
select
  u.username,
  date_format(u.created,'%b %D %Y') as created,
  u.bio,
  c.flag_filename,
  o.org_id,
  o.name as org_name,
  c.country_id,
  c.name as country_name
from
  users u
  left outer join organizations o on o.org_id = u.org_id
  left outer join countries c on c.country_id = u.country_id
where
  u.user_id = '$user_id'
EOT;
$userresult = mysql_query($userquery);
$userdata = mysql_fetch_assoc($userresult);
if ($rankresult) {
  $rankdata = mysql_fetch_assoc($rankresult);
  $rank = $rankdata["rank"];
}
$rank = ($rank == NULL)?"N/A. No ranking available":$rank;
$username = htmlentities($userdata["username"]);
$created = $userdata["created"];
$country_id = htmlentities($userdata["country_id"]);
$country_name = htmlentities($userdata["country_name"]);
$country_name = $country_name == NULL ?
  "Unknown" : htmlentities($country_name);
$flag_filename = $userdata["flag_filename"];
$flag_filename = $flag_filename == NULL ? "" : "<img alt=\"$country_name\" width=\"16\" height=\"11\" title=\"$country_name\" src=\"flags/$flag_filename\" />";
$org_id = htmlentities($userdata["org_id"]);
$org_name = htmlentities($userdata["org_name"]);
$bio = htmlentities($userdata["bio"]);
if ($org_name == NULL) {
  $org_name = "None";
}
if (logged_in_with_valid_credentials() && current_user_id() == $user_id) {
    $logged_in = true;
} else {
    $logged_in = false;
}
if (!$userresult) {
  echo "<p>Invalid User ID</p>";
} else {
echo <<<EOT
    <script>
    function toggle_change_org() {
       if (document.getElementById('orgchange').style.display == 'none') {
          document.getElementById('orgchange').style.display='inline';
          document.getElementById('org_ctxt').innerHTML = 'Cancel';
       } else {
          document.getElementById('orgchange').style.display='none';
          document.getElementById('org_ctxt').innerHTML = 'Change';
       }
    }
    function toggle_change_country() {
       if (document.getElementById('countrychange').style.display == 'none') {
          document.getElementById('countrychange').style.display='inline';
          document.getElementById('country_ctxt').innerHTML = 'Cancel';
       } else {
          document.getElementById('countrychange').style.display='none';
          document.getElementById('country_ctxt').innerHTML = 'Change';
       }
    }
    </script>
    <h2>Profile for $username</h2>
EOT;
if ($logged_in) {
  echo '   <form method="post" action="save_profile.php">';
}
echo <<<EOT
    <p><strong>Country:</strong>&nbsp;
    <a href="country_profile.php?country_id=$country_id">$flag_filename
      $country_name</a>
EOT;
if ($logged_in) {
echo <<<EOT
    <span style="padding-left: 1em; font-size: smaller">
      <a href="#" id="country_ctxt" onclick="toggle_change_country()">Change</a>
    <span id="countrychange" style="display: none">
      <select name="user_country" style="width:210px;">
EOT;
  $query = "SELECT * FROM countries ORDER BY country_id";
  $result = mysql_query($query);
  while ($row = mysql_fetch_assoc($result)) {
    $option_id = $row['country_id'];
    $option_name = $row['name'];
    if ($option_id == $country_id) {
      echo "<option selected value=$option_id>$option_name</option>";
    } else {
      echo "<option value=$option_id>$option_name</option>";
    }
    if ($option_id == 11) {
        echo "<option value=999>---</option>";
    }
  }
echo <<<EOT
      </select><input type="submit" value="Save" />
    </span></span></p>
EOT;
}
echo <<<EOT
    <p>
    <strong>Affiliation:</strong>&nbsp;
    <a href="organization_profile.php?org_id=$org_id">$org_name</a>
EOT;
if ($logged_in) {
echo <<<EOT
      <span style="padding-left: 1em; font-size: smaller">
        <a href="#" id="org_ctxt" onclick="toggle_change_org()">Change</a>
      <span id="orgchange" style="display: none">
      <select name="user_organization" style="width:210px;">
      <option value="0">Other</option>
      <option value="1">University of Waterloo</option>
      <option value="999">---</option>
EOT;
  $query = "SELECT * FROM organizations WHERE org_id > 1 ORDER BY name";
  $result = mysql_query($query);
  while ($row = mysql_fetch_assoc($result)) {
    $option_id = $row['org_id'];
    $option_name = $row['name'];
    if ($option_id == $org_id) {
      echo "<option selected value=$option_id>$option_name</option>";
    } else {
      echo "<option value=$option_id>$option_name</option>";
    }
  }
?>
     </select><input type="submit" value="Save" /></span></span>
     </form>
<?php }
echo <<<EOT
    </p>
    <p><strong>Joined:</strong>&nbsp;$created</p>
EOT;
if ($bio != NULL) {
echo <<<EOT
    <p><strong>About Me:</strong><br />
      $bio
    </p>
EOT;
}
echo <<<EOT
    </dl>
    <p><strong>Current Rank:</strong>&nbsp;$rank</p>
    </dl>

    <!--<h3><span>Statistics</span><div class=\"divider\" /></h3>
    <img width="600" height="280" alt="" src="profile_ranktime.php?user_id=$user_id" />-->
EOT;

    echo "<h3><span>Latest Games</span><div class=\"divider\" /></h3>";
    echo getGamesTableString($user_id, true, 15, "profile_games.php?user_id=$user_id");
    echo "<p></p>";
    echo "<h3><span>Recent Submissions</span><div class=\"divider\" /></h3>";
    echo getSubmissionTableString($user_id, true, 10, "profile_submissions.php?user_id=$user_id");

}
//$cache->end();
//}

include 'footer.php';
?>

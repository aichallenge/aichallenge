<?php
include('header.php');
include('mysql_login.php');

$user_id = $_GET["user"];
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
require_once('game_list.php');

// Fetch Rank Data
$rankquery = <<<EOT
select
    r.rank
from
    ranking r
    inner join submission s on s.submission_id = r.submission_id
    where
        s.user_id = '$user_id' and
        leaderboard_id = (select max(leaderboard_id) from leaderboard
            where complete=1)
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
  c.name as country_name,
  u.email,
  u.activation_code
from
  user u
  left outer join organization o on o.org_id = u.org_id
  left outer join country c on c.country_id = u.country_id
where
  u.user_id = $user_id;
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
    $sid = session_id();
    $update_key = sha1(
        $sid . $userdata["activation_code"] . $userdata["email"]);
} else {
    $logged_in = false;
}
if (!$userresult) {
  echo "<p>Invalid User ID</p>";
} else {
echo "    <h2>Profile for $username</h2>";
if ($logged_in) {
echo <<< EOT
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
    function toggle_change_bio() {
       if (document.getElementById('bio_submit').style.display == 'none') {
           document.getElementById('bio_submit').style.display = 'inline';
           document.getElementById('bio_span').style.display = 'none';
           document.getElementById('bio_edit').style.display = 'inline';
           document.getElementById('bio_ctxt').innerHTML = 'Cancel';
       } else {
           document.getElementById('bio_submit').style.display = 'none';
           document.getElementById('bio_edit').style.display = 'none';
           document.getElementById('bio_span').style.display = 'inline';
           document.getElementById('bio_ctxt').innerHTML = 'Edit';
       }
    }
    </script>
    <form method="post" action="save_profile.php">
        <input type="hidden" name="update_key" value="$update_key" />
EOT;
}
echo <<<EOT
    <p><strong>Country:</strong>&nbsp;
    <a href="country_profile.php?country=$country_id">$flag_filename
      $country_name</a>
EOT;
if ($logged_in) {
echo <<<EOT
    <span style="padding-left: 1em; font-size: smaller">
      <a href="#" id="country_ctxt" onclick="toggle_change_country()">Change</a>
    <span id="countrychange" style="display: none">
      <select name="user_country" style="width:210px;">
EOT;
  $query = "SELECT * FROM country ORDER BY country_id";
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
    <a href="organization_profile.php?org=$org_id">$org_name</a>
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
  $query = "SELECT * FROM organization WHERE org_id > 1 ORDER BY name";
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
echo <<<EOT
     </select><input type="submit" value="Save" /></span></span>
EOT;
}
echo <<<EOT
    </p>
    <p><strong>Joined:</strong>&nbsp;$created</p>
EOT;
if ($bio != NULL) {
echo <<<EOT
    <p><strong>About Me:</strong><br />
      <span id="bio_span">$bio</span>
EOT;
  if ($logged_in) {
echo <<<EOT
      <textarea id="bio_edit" style="display: none" name="user_bio" cols="40" rows="3">$bio</textarea>
    <input id="bio_submit" style="display: none" type="submit" value="Save" />
    <span style="padding-left: 1em; font-size: smaller">
      <a href="#" id="bio_ctxt" onclick="toggle_change_bio()">Edit</a>
    </span>
    </p>
    </form>
EOT;
  } else {
    echo "</p>";
  }
} elseif ($logged_in) {
echo <<<EOT
    <p><strong>About Me:</strong><br />
      <span id="bio_span">You currently have no information entered.</span>
      <textarea id="bio_edit" style="display: none" name="user_bio" cols="40" rows="3"></textarea>
    <input id="bio_submit" style="display: none" type="submit" value="Save" />
    <span style="font-size: smaller">
      <a href="#" id="bio_ctxt" onclick="toggle_change_bio()">Edit</a>
    </span>
    </p>
    </form>
EOT;
}
    echo "<p><strong>Current Rank:</strong>&nbsp;$rank</p>";

    $query = "SELECT * FROM submission
        WHERE user_id = '$user_id' AND status = 40 and latest = 1";
    $result = mysql_query($query);
    if ($row = mysql_fetch_assoc($result)) {
        $sub_id = $row['submission_id'];
        $query = "SELECT count(1) FROM submission
            WHERE last_game_timestamp < (SELECT last_game_timestamp
                FROM submission WHERE submission_id = '$sub_id')
            AND status = 40 AND latest = 1";
        $result = mysql_query($query);
        $row = mysql_fetch_assoc($result);
        $queue_size = $row['count(1)'];
        if ($queue_size > 50) {
            echo "<p>Most likely to play next game within the
                next $queue_size games.</p>";
        } else {
            echo "<p>Next game should be played soon.</p>";
        }
    }

    echo "<h3><span>Latest Games</span><div class=\"divider\" /></h3>";
    echo get_game_list_table(1, $user_id, NULL, NULL, TRUE, 'profile_games.php');
    //echo getGamesTableString($user_id, true, 15, "profile_games.php?user=$user_id");
    echo "<p></p>";
    echo "<h3><span>Recent Submissions</span><div class=\"divider\" /></h3>";
    echo getSubmissionTableString($user_id, true, 10, "profile_submissions.php?user=$user_id");

}
//$cache->end();
//}

if (logged_in_with_valid_credentials() && logged_in_as_admin()) {
    $username = current_username();
    echo <<<EOT
<form  method="post" action="disable_account.php"
 onSubmit='return confirm("Are you sure you want do disable this account?")'>
  <h2>Disable Account:</h2>
  <p>Reason: <input name="reason" type="text" />&nbsp;by&nbsp;$username.</p>
  <input type="submit" value="Disable Account" />
  <input type="hidden" name="user_id" value="$user_id" />
</form>
EOT;
}

include 'footer.php';
?>

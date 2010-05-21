<?php
include 'header.php';

$user_id = $_GET["user_id"];
if(!filter_var($user_id, FILTER_VALIDATE_INT)) {
    $user_id = NULL;
}

require_once 'Zend/Cache.php';

$frontendOptions = array('lifeTime' => 120, 'automatic_serialization' => true, 'automatic_cleaning_factor' => 10);
$backendOptions = array('cache_file_umask' => '777', 'file_name_prefix' => 'ai_contest', 'cacheDir' => '/tmp/');

$cache = Zend_Cache::factory('Output', 'File', $frontendOptions, $backendOptions);

$cacheID="profile_$user_id";

if (!($cache->start($cacheID))) {
include_once 'profile_submissions_widget.php';
include_once 'profile_games_widget.php';


// Fetch Rank Data
$rankquery = <<<EOT
select
    r.rank
from
    contest_rankings r
    inner join contest_submissions s on s.submission_id = r.submission_id
where
    s.user_id = '$user_id' and
    leaderboard_id = (select max(leaderboard_id) from contest_leaderboards)
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
    contest_users u
    left outer join contest_organizations o on o.org_id = u.org_id
    left outer join contest_countries c on c.country_id = u.country_id
where
    u.user_id = '$user_id'
EOT;

$userresult = mysql_query($userquery);
$userdata = mysql_fetch_assoc($userresult);

if ($rankresult) {
    $rankdata = mysql_fetch_assoc($rankresult);
    $rank = $rankdata["rank"];
}
$rank = ($rank == NULL)?"N/A - Latest Submission Suspended":$rank;

$username = htmlentities($userdata["username"]);
$created = $userdata["created"];
$country_id = htmlentities($userdata["country_id"]);
$country_name = htmlentities($userdata["country_name"]);
$country_name = $country_name == NULL ? "Unknown" : htmlentities($country_name);
$flag_filename = $userdata["flag_filename"];
$flag_filename = $flag_filename == NULL ? "" : "<img alt=\"$country_name\" width=\"16\" height=\"11\" title=\"$country_name\" src=\"flags/$flag_filename\" />";
$org_id = htmlentities($userdata["org_id"]);
$org_name = htmlentities($userdata["org_name"]);
$bio = htmlentities($userdata["bio"]);

if (!$userresult) {
    echo "<p>Invalid User ID</p>";
} else {
echo <<<EOT
    <h2>$username's Profile</h2>
    <h3><span>Personal Information</span><div class="divider" /></h3>
    <dl class="userinfo">
    <dt>Joined:</dt>
    <dd>$created</dd>
    <dt>Country:</dt>
    <dd><a href="country_profile.php?country_id=$country_id">$flag_filename $country_name</a></dd>
EOT;
    if ($org_name != NULL) {
echo <<<EOT
    <dt>Organization:</dt>
    <dd><a href="organization_profile.php?org_id=$org_id">$org_name</a></dd>
EOT;
    }

    if ($bio != NULL) {
echo <<<EOT
    <dt>About Me:</dt>
    <dd>$bio &nbsp;</dd>
EOT;
    }

echo <<<EOT
    </dl>
    <h3><span>Contest Information</span><div class="divider" /></h3>
    <dl class="userinfo">
    <dt>Current Rank:</dt>
    <dd>$rank</dd>
    </dl>

    <!--<h3><span>Statistics</span><div class=\"divider\" /></h3>
    <img width="600" height="280" alt="" src="profile_ranktime.php?user_id=$user_id" />-->
EOT;

    echo "<h3><span>Latest Games</span><div class=\"divider\" /></h3>";
    echo getGamesTableString($user_id, true, 15, "profile_games.php?user_id=$user_id");

    echo "<h3><span>Submissions</span><div class=\"divider\" /></h3>";
    echo getSubmissionTableString($user_id, true, 10, "profile_submissions.php?user_id=$user_id");

}
$cache->end();
}

include 'footer.php';
?>

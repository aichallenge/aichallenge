<?php
include 'header.php';
include_once 'rankings_widget.php';

$org_id = $_GET["org_id"];
$org_id = mysql_real_escape_string($org_id);

// Grab Data
$org_query = <<<EOT
select
    *
from
    contest_organizations
where
    org_id = '$org_id'
EOT;

$org_result = mysql_query($org_query);

if (!$org_result) {
    echo "<p>Invalid Organization ID</p>";
    echo $org_id;
} else {
    $org_data = mysql_fetch_assoc($org_result);
    $org_name = htmlentities($org_data["name"]);
/*
echo <<<EOT
<h2>$org_name's Profile</h2>
EOT;
 */

echo <<<EOT
<h2><span>$org_name's User Rankings</span><div class="divider" /></h2>
EOT;

echo getRankingsTableString(1, false, 100,"?org_id=$org_id&page=",0,"o.org_id",$org_id);

}

include 'footer.php';
?>

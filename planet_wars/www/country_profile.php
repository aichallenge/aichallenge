<?php
include 'header.php';
include_once 'rankings_widget.php';

$country_id = $_GET["country_id"];
$country_id = mysql_real_escape_string($country_id);

// Grab Data
$country_query = <<<EOT
select
    *
from
    countries
where
    country_id = '$country_id'
EOT;

$country_result = mysql_query($country_query);

if (!$country_result) {
    echo "<p>Invalid Country ID</p>";
} else {
    $country_data = mysql_fetch_assoc($country_result);
    $country_code = htmlentities($country_data["country_code"]);
    $country_name = htmlentities($country_data["name"]);
    /*
echo <<<EOT
<h2>$country_name's Profile</h2>
EOT;

<h3><span>Country Information</span><div class="divider" /></h3>
<dl class="userinfo">
<dt>Country Code:</dt>
<dd>$country_code</dd>
</dl>
     */
echo <<<EOT
<h2><span>$country_name's User Rankings</span><div class="divider" /></h2>
EOT;

echo getRankingsTableString(1, false, 100,"?country_id=$country_id&page=",0,"c.country_id",$country_id);

}

include 'footer.php';
?>

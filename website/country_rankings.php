<?php

$title="Country Rankings";
include 'header.php';
include_once 'country_rankings_widget.php';

echo <<<EOT
<h2>Countries by Participation</h2>
<p>Here are the participating countries ranked by number of entries in the top 200.</p>
EOT;

echo getCountryRankingsTableString(200);

include 'footer.php';
?>

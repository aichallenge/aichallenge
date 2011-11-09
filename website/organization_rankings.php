<?php
include 'header.php';
include_once 'organization_rankings_widget.php';

echo <<<EOT
<h2>Organizations by Participation</h2>
<p>Here are the participating organizations ranked by number of entries in the top 100.</p>
EOT;

echo getOrganizationRankingsTableString(100);

include 'footer.php';
?>

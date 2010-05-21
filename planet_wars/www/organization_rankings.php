<?php
include 'header.php';
include_once 'organization_rankings_widget.php';

echo <<<EOT
<h2>Organizations by Participation</h2>
<p>Here are the participating organizations ranked by number of entries in the top 100.</p>
<p>If you want to sign up your company or school, simply post in <a href="http://csclub.uwaterloo.ca/contest/forums/viewtopic.php?f=4&t=17">this forum thread</a>.
EOT;

echo getOrganizationRankingsTableString(100);

include 'footer.php';
?>

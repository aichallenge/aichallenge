<?php
include 'header.php';
include_once 'rankings_widget.php';

$lang = $_GET["lang"];
$lang_encoded = urlencode($lang);
$lang_display = htmlspecialchars($lang);

echo <<<EOT
<h2><span>$lang_display's User Rankings</span><div class="divider" /></h2>
EOT;

echo getRankingsTableString(1, false, 100,"?lang=$lang_encoded&page=",0,"programming_language",$lang);

include 'footer.php';
?>

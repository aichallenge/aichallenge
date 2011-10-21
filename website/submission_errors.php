<?php

$title="Submission Errors";
include 'header.php';

$error = $_GET["error"];
if(!filter_var($error, FILTER_VALIDATE_INT)) {
    $error = NULL;
} else {
    $error = intval($error);
}

//
// Define Error Messages
//

$errors["39"] = <<<EOT
<h3>Garbage Collected</h3>
<p class="err_desc">
You submitted a starter package, and it worked! We've disabled your entry
while you work on your algorithm to give other contestants more play time,
however you may resubmit at any time.
</p>
EOT;

$errors["91"] = <<<EOT
<h3>Submission Timeout</h3>
<p class="err_desc">
Submission did not respond for more than one second on a single move.
</p>
EOT;

$errors["92"] = <<<EOT
<h3>Write Failed</h3>
<p class="err_desc">
Submission is not reading, or died, or something else.
</p>
EOT;

$errors["93"] = <<<EOT
<h3>Read Failed</h3>
<p class="err_desc">
Submission is not writing, or died, or something else.
</p>
EOT;

$errors["94"] = <<<EOT
<h3>Read EOF</h3>
<p class="err_desc">
Submission closed standard output, or died, or something else.
</p>
EOT;

$errors["95"] = <<<EOT
<h3>Extra Output</h3>
<p class="err_desc">
Submission wrote '1', '2', '3', '4' (one of the possible moves) followed by extra text on the same line which was not understood.
</p>
EOT;

$errors["96"] = <<<EOT
<h3>Invalid Move</h3>
<p class="err_desc">
Submission wrote something else other then '1', '2', '3', '4' (one of the possible moves).
</p>
EOT;

$errors["97"] = <<<EOT
<h3>Early Exit</h3>
<p class="err_desc">
Submission crashed, took longer than 10 minutes to run, or something else.
</p>
EOT;

//
// Actual View
//

echo "<h2>Submission Errors</h2>";
echo <<<EOT
<p>
More detailed information about submission errors, what causes them, 
and how to fix them coming within the next 48 hours.</p>
<p>Meanwhile, check the forum for posts by other contestants with similar submission suspensions.</p>
EOT;
if ($error != NULL) {
    echo "<h3><span>Your Submission Error</span><div class=\"divider\" /></h3>";
    if ($errors[$error] != NULL) {
        echo $errors[$error];
    } else if ($error != NULL) {
        echo "<h3>Unknown Error</h3>";
        echo "<p class=\"err_desc\">";
        echo "The error associated with this submission is currently undocumented.";
        echo "</p>";
    }

}

echo "<h3><span>";
if ($error != NULL) {
    echo "Other ";
}
echo "Submission Errors</span><div class=\"divider\" /></h3>";

foreach ($errors as $err_info) {
    echo htmlentities($err_info, ENT_COMPAT, "UTF-8");
}

include 'footer.php';
?>

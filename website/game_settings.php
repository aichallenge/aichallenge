<?php

include 'header.php';
require_once('server_info.php');

?>

<h1>Game Settings</h1>

<p>These are the settings given to the engine specifying the various options for how the game will be played.</p>

<table>
  <?php foreach ($server_info['game_options'] as $option => $value): ?>
  <tr>
    <th><?=$option ?></th>
    <td><?php
        if (is_array($value)) {
            foreach($value as $v) {
                echo $v .", ";
            }
        } else if (is_bool($value)) {
            echo $value ? 'True' : 'False';
        } else {
            print_r($value);
        }
    ?></td>
  </tr>
  <?php endforeach ?>
</table>

<?php include 'footer.php' ?>

<?php include "header.php"; ?>

<div id="visualizer">
  <table id="players">
          <tr>
            <td width='40%' style="text-align:right" class="player1Name"></td>
            <td width='20%' style="text-align:center" class="playerVs">Loading</td>
            <td width='40%' style="text-align:left" class="player2Name"></td>
          </tr>
        </table>
        
        <canvas id="display" width="640" height="640"></canvas>
        <p id="turnCounter"></p>
        <p id="controls">
            <a href="#" id="start-button"><span class="small">|</span>&laquo;</a> | 
            <a href="#" id="prev-frame-button">&laquo;</a> | 
            <a href="#" id="play-button">&#9654;</a> | 
            <a href="#" id="next-frame-button">&raquo;</a> | 
            <a href="#" id="end-button">&raquo;<span class="small">|</span></a>
        </p>
    </div>
    
<?php include "footer.php"; ?>


  <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"></script>
  <script>!window.jQuery && document.write('<script src="visualizer/js/jquery-1.4.2.min.js"><\/script>')</script>
  
  <script>
  <?php
  ob_start();
  include('game_info.php');
  $data = ob_get_clean();
  echo 'var data = "' . addslashes(str_replace("\n", "\\n", $data)) . '"';
  ?>
  </script>
  <script src="visualizer/js/visualizer.js?v=1"></script>
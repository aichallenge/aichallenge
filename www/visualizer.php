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
  <p id="error_message"></p>
  <p id="turnCounter"></p>
  <p id="controls">
      <a href="#" id="start-button" title="To start"><span class="small">|</span>&laquo;</a> | 
      <a href="#" id="prev-frame-button" title="Step back (left arrow)">&laquo;</a> | 
      <a href="#" id="play-button" title="Play/Pause (spacebar)">&#9654;</a> | 
      <a href="#" id="next-frame-button" title="Step forward (right arrow)">&raquo;</a> | 
      <a href="#" id="end-button" title="To end">&raquo;<span class="small">|</span></a><br />
      <a href="#" id="speeddown" title="Slow down (down arrow)">-</a>
      <a href="#" id="speedup" title="Speed up (up arrow)">+</a>
  </p>
  <p>
    <canvas id="chart" width="640" height="100" ></canvas>
  </p>
        
</div>
    
<?php include "footer.php"; ?>


  <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"></script>
  <script>!window.jQuery && document.write('<script src="visualizer/js/jquery-1.4.2.min.js"><\/script>')</script>
  
  <script>
  $(function(){
    if(document.getElementById('display').getContext==undefined){
      $('#visualizer').html('<APPLET code="Viewer.class" width="500" height="500" archive="Visualizer.jar"> <param name="game_id" value="<?= htmlspecialchars($_GET["game_id"]); ?>"/> Java applet support required.</APPLET>')
    }
  })
  
  </script>
  
  
  <script>
  <?php
  ob_start();
  include('game_info.php');
  $data = ob_get_clean();
  echo 'var data = "' . htmlspecialchars(str_replace("\n", "\\n", $data)) . '"';
  ?>
  </script>
  <script src="visualizer/js/visualizer.js?v=3"></script>

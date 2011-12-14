                    </div>
                </div>
                <div class="right">
                    <?php include("menu.php"); ?>
                    <div id="sponsor_bar">
                        <p>Thanks to additional server sponsor</p>
                        <a href="http://www.tiw.nl/">
                            <img src="images/totaalnet.png" alt="Totaalnet" />
                        </a>
                    </div>
                </div>
            </div>
            <div class="columns">
                <div class="left">
                    <div id="footer">
                        <a href="server_stats.php">Server Statistics</a> |
                        <a href="credits.php">Credits</a> |
                        <a href="rules.php">Rules</a>
                    <div class="small right fade">
<?php
$page_render_total_time = intval((microtime(true) - $page_render_start_time) * 1000);
echo "Page rendered in $page_render_total_time milliseconds";
?>
                    </div>
                    </div>
                </div>
                <div class="right">
                </div>
            </div>
        </div>
    </body>
</html>

<?php 
$user_id = $_GET["user"];
if(!filter_var($user_id, FILTER_VALIDATE_INT)) {
    header("Location: blank.gif");
    exit;
}

require_once 'Zend/Cache.php';

$frontendOptions = array('lifeTime' => 3600, 'automatic_serialization' => true, 'automatic_cleaning_factor' => 100);
$backendOptions = array('cache_file_umask' => '777', 'file_name_prefix' => 'ai_contest', 'cacheDir' => '/tmp/');

$cache = Zend_Cache::factory('Output', 'File', $frontendOptions, $backendOptions);

$cacheID="pchart_$user_id";

if (!($cache->start($cacheID))) {

require_once('mysql_login.php');
include("pChart/pData.class");
include("pChart/pChart.class");
include("pChart/pCache.class");

$query = <<<EOT
select straight_join
    avg(r.rank) as avg_rank,
    min(r.rank) as min_rank,
    max(r.rank) as max_rank,
    unix_timestamp(l.timestamp) as timestamp,
    date_format(l.timestamp,'%b %D %h%p') as day,
    date_format(l.timestamp, '%k') as hour
from
    submission s
    inner join ranking r on r.submission_id = s.submission_id
    inner join leaderboard l on l.leaderboard_id = r.leaderboard_id
where 
    s.user_id = '$user_id'
group by
    date(l.timestamp),
    hour(l.timestamp)
order by
    timestamp asc
EOT;

$result = mysql_query($query);

$DataSet = new pData;
if ($result) {
    // Dynamic Splitting (12 hours vs. days)
    $hours = mysql_num_rows($result);
    $time_label_hours = floor(h/72) * 12 + 12;

    // Grab Data From Data Source
    $avg_rank = array();
    $max_rank = array();
    $min_rank = array();
    $time_days= array();
    for ($i = 1; $row = mysql_fetch_assoc($result); $i += 1) {
        // Pad Data For First Day
        if ($i == 1) {
            $padding = ($row["hour"]) % $time_label_hours;
            if ($padding == 0) { $padding = $time_label_hours; }
            for ($j = 1; $j < $padding; $j += 1) {
                array_push($avg_rank,"");
                array_push($min_rank,"");
                array_push($max_rank,"");
                array_push($time_days,"");
            }
        }
        // Push Data
        array_push($avg_rank,$row["avg_rank"]);
        array_push($min_rank,$row["min_rank"]);
        array_push($max_rank,$row["max_rank"]);
        array_push($time_days,$row["day"]);
    }


    // Create Data Set
    $DataSet->AddPoint($avg_rank,"Average Ranking");
    $DataSet->AddPoint($min_rank,"Highest Ranking");
    $DataSet->AddPoint($max_rank,"Lowest Ranking");
    $DataSet->AddPoint($time_days,"Days");
    $DataSet->AddSerie("Average Ranking");
    $DataSet->AddSerie("Highest Ranking");
    $DataSet->AddSerie("Lowest Ranking");
    $DataSet->SetYAxisName("Rank");
    $DataSet->SetAbsciseLabelSerie("Days");

    // Build Chart
    $Test = new pChart(600,280);
    $Test->loadColorPalette("pChart/hardtones.pal");

    // Make Evenly Disable Ranks (No Real Numbers) - optimizing by making sure $max is not prime
    $max = max($max_rank);
    $max = $max + (7-$max%7);
    for ($divs = 5; (($max-1) % $divs != 0 && ($max) % $divs != 0); $divs += 1);
    if ($max % $divs == 0) { $max = $max+1; }
    if ($divs > 15) { $divs = 15; }

    // Set Y Scale
    $Test->setFixedScale($max,1,$divs,0,0,1);

    // Draw Graph Area
    $Test->setFontProperties("Fonts/tahoma.ttf",10);
    $Test->setGraphArea(45,30,450,200);
    $Test->drawGraphArea(255,255,255,TRUE);
    $Test->setFontProperties("Fonts/tahoma.ttf",8);

    // Scale grid and background
    $Test->drawScale($DataSet->GetData(),$DataSet->GetDataDescription(),SCALE_NORMAL,80,80,80,TRUE,50,100,FALSE, $time_label_hours);
    $Test->drawGrid(4,FALSE,210,210,210);

    // Draw Data
    $Test->drawTreshold(1,100,100,100);
    $Test->drawLineGraph($DataSet->GetData(),$DataSet->GetDataDescription());
    //$Test->drawCubicCurve($DataSet->GetData(),$DataSet->GetDataDescription(),1);  

    // Draw Legend
    $Test->drawLegend(465,75,$DataSet->GetDataDescription(),225,225,225,0,0,0,100,100,100,FALSE);

    // Draw Title
    $Test->setFontProperties("Fonts/tahoma.ttf",12);
    $Test->drawTitle(-30,22,"Rankings",20,20,20,585,0);

    // Render to user
    $Test->Stroke();
    $cache->end();
}
}
?>


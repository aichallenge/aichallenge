<?php

require_once('server_info.php');
$skills_dir = $server_info["repo_path"] . "/manager/PHPSkills/Skills/";
require_once($skills_dir.'TrueSkill/FactorGraphTrueSkillCalculator.php');
require_once($skills_dir.'GameInfo.php');
require_once($skills_dir.'Player.php');
require_once($skills_dir.'Rating.php');
require_once($skills_dir.'Team.php');
require_once($skills_dir.'Teams.php');
require_once($skills_dir.'SkillCalculator.php');

use Moserware\Skills\TrueSkill\FactorGraphTrueSkillCalculator;
use Moserware\Skills\GameInfo;
use Moserware\Skills\Player;
use Moserware\Skills\Rating;
use Moserware\Skills\Team;
use Moserware\Skills\Teams;
use Moserware\Skills\SkillCalculator;

$calculator = new FactorGraphTrueSkillCalculator();
$gameInfo = new GameInfo(50.0,       // mu
                         50.0/3.0,   // sigma
                         50.0/6.0,   // beta
                         50.0/300.0, // tau
                         0.01);      // draw prob

$mu = array(50.042, 50.0415, 50.1962, 50.1956, 50.2264, 50.2051, 50.2169, 50.2387, 50.2435, 50.2516);
$sigma = array(16.6247, 16.6252, 16.4705, 16.4711, 16.4403, 16.4616, 16.4498, 16.428, 16.4232, 16.4151);

// $mu = array(50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0);
// $sigma = array(16.6667,16.6667,16.6667,16.6667,16.6667,16.6667,16.6667,16.6667,16.6667,16.6667);
$game_rank = array(2,3,1,4,5,6,7,7,0,7);

$players = array();
$ratings = array();
$teams = array();
$rank = array();
$count = 10;

for ($i = 1; $i < $count; ++$i) {
    $players[] = new Player($i);
    $teams[] = new Team($players[$i-1], new Rating($mu[$i-1], $sigma[$i-1]));
    $rank[] = $game_rank[$i-1];
}

$newRatings = $calculator->calculateNewRatings($gameInfo, $teams, $rank);

for ($i = 1; $i < $count; ++$i) {
    echo "player$i: ".$newRatings->getRating($players[$i-1])."\n";
}

?>

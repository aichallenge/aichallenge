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

echo 'starting trueskill test';

$calculator = new FactorGraphTrueSkillCalculator();
$gameInfo = new GameInfo(50.0,       // mu
                         50.0/3.0,   // sigma
                         50.0/6.0,   // beta
                         50.0/300.0, // tau
                         0.01);      // draw prob

$players = array();
$ratings = array();
$teams = array();
for ($i = 1; $i < 3; ++$i) {
    $player = new Player($i);
    $rating = new Rating(50.0, 16.66);
    $teams[] = new Team($player, $rating);
}

$newRatings = $calculator->calculateNewRatings($gameInfo, $teams, array(0, 1));

$player0NewRating = $newRatings->getRating($player0);
$player1NewRating = $newRatings->getRating($player1);

echo "player 0: $player0NewRating";
echo "player 1: $player1NewRating";

echo 'finished trueskill test';

?>

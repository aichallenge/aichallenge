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
$player0 = new Player(1);
$player1 = new Player(2);
$gameInfo = new GameInfo();

$player0Rating = new Rating(40.0, 8.333);
$player1Rating = new Rating(60.0, 8.333);

$team0 = new Team($player0, $player0Rating);
$team1 = new Team($player1, $player1Rating);

$teams = array();
$teams[] = $team0;
$teams[] = $team1;
// Teams::concat($team0, $team1);
$newRatings = $calculator->calculateNewRatings($gameInfo, $teams, array(1, 2));

$player0NewRating = $newRatings->getRating($player0);
$player1NewRating = $newRatings->getRating($player1);

echo "player 0: $player0NewRating";
echo "player 1: $player1NewRating";

echo 'finished trueskill test';

?>

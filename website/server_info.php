<?php

$server_info = array(
    "db_host" => "mysql",
    "db_username" => $_ENV['DATABASE_USER'],
    "db_password" => $_ENV['DATABASE_PASSWORD'],
    "db_name" => $_ENV['DATABASE_NAME'],
    "mailer_address" => "donotsend",
    "aws_accesskey" => "",
    "aws_secretkey" => "",
    "submissions_open" => True,
    "repo_path" => $_ENV['REPO_DIR'],
    "uploads_path" => $_ENV['UPLOAD_DIR'],
    "maps_path" => $_ENV['MAP_DIR'],
    "replay_path" => $_ENV['REPLAY_DIR'],
    "api_create_key" => "",
    "api_log" => $_ENV['LOG_DIR'] . "/php_api.log",
    "game_result_errors" => $_ENV['LOG_DIR'] . "/game_result_errors.log",
    "game_options" => array (
        "turns" => 1500,
        "loadtime" => 3000,
        "turntime" => 500,
        "viewradius2" => 77,
        "attackradius2" => 5,
        "spawnradius2" => 1,
        "location" => $_ENV['API_URL'],
        "serial" => 2,
        "food_rate" => array(5,11),
        "food_turn" => array(19,37),
        "food_start" => array(75,175),
        "food_visible" => array(3,5),
        "food" => "symmetric",
        "attack" => "focus",
        "kill_points" => 2,
        "cutoff_turn" => 150,
        "cutoff_percent" => 0.85
    )
);

?>

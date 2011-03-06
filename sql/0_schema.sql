DROP TABLE IF EXISTS `country`;
CREATE TABLE `country` (
  `country_id` int(11) NOT NULL,
  `country_code` varchar(8) DEFAULT NULL,
  `name` varchar(64) DEFAULT NULL,
  `flag_filename` varchar(16) DEFAULT NULL,
  PRIMARY KEY (`country_id`)
);

DROP TABLE IF EXISTS `game`;
CREATE TABLE `game` (
  `game_id` int(11) NOT NULL AUTO_INCREMENT,
  `map_id` int(11) NOT NULL,
  `timestamp` datetime NOT NULL,
  `worker` smallint(5) unsigned NOT NULL,
  `replay_path` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`game_id`),
  KEY `timestamp` (`timestamp`),
  KEY `worker` (`worker`,`timestamp`)
);

DROP TABLE IF EXISTS `game_archive`;
CREATE TABLE `game_archive` (
  `game_id` int(11) NOT NULL,
  `map_id` int(11) NOT NULL,
  `timestamp` datetime NOT NULL,
  `worker` int(11) NOT NULL,
  `replay_path` varchar(255) NOT NULL,
  UNIQUE KEY `game_id_UNIQUE` (`game_id`)
);

DROP TABLE IF EXISTS `game_player`;
CREATE TABLE `game_player` (
  `game_id` int(11) NOT NULL,
  `submission_id` int(11) NOT NULL,
  `rank` int(11) NOT NULL,
  `errors` varchar(1024) DEFAULT NULL,
  `stderr` varchar(1024) DEFAULT NULL,
  `sigma_before` float NOT NULL,
  `sigma_after` float DEFAULT NULL,
  `mu_before` float NOT NULL,
  `mu_after` float DEFAULT NULL,
  UNIQUE KEY `game_player_idx` (`game_id`,`submission_id`)
);

DROP TABLE IF EXISTS `language`;
CREATE TABLE `language` (
  `language_id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(64) NOT NULL,
  `main_code_file` varchar(64) NOT NULL,
  `command` varchar(128) NOT NULL,
  `platform_specific_compilation` tinyint(4) NOT NULL DEFAULT '0',
  PRIMARY KEY (`language_id`)
);

DROP TABLE IF EXISTS `leaderboard`;
CREATE TABLE `leaderboard` (
  `leaderboard_id` int(11) NOT NULL AUTO_INCREMENT,
  `timestamp` datetime NOT NULL,
  `algorithm_name` varchar(64) DEFAULT NULL,
  `calculation_time` bigint(20) DEFAULT '0',
  `complete` tinyint(4) NOT NULL DEFAULT '0',
  PRIMARY KEY (`leaderboard_id`),
  KEY `timestamp` (`timestamp`)
);

DROP TABLE IF EXISTS `login_attempt`;
CREATE TABLE `login_attempt` (
  `timestamp` datetime NOT NULL,
  `username` varchar(64) NOT NULL,
  `password` varchar(40) NOT NULL,
  `naive_ip` varchar(18) NOT NULL,
  `real_ip` varchar(18) NOT NULL,
  KEY `timestamp` (`timestamp`,`username`,`password`,`naive_ip`,`real_ip`)
);

DROP TABLE IF EXISTS `map`;
CREATE TABLE `map` (
  `map_id` int(11) NOT NULL AUTO_INCREMENT,
  `players` int(11) NOT NULL,
  `filename` varchar(256) DEFAULT NULL,
  `priority` int(11) DEFAULT NULL,
  PRIMARY KEY (`map_id`)
);

DROP TABLE IF EXISTS `matchup`;
CREATE TABLE `matchup` (
  `matchup_id` int(11) NOT NULL AUTO_INCREMENT,
  `player_one` int(11) NOT NULL,
  `player_two` int(11) NOT NULL,
  `map_id` int(11) NOT NULL,
  `dispatch_time` datetime DEFAULT NULL,
  PRIMARY KEY (`matchup_id`)
);

DROP TABLE IF EXISTS `organization`;
CREATE TABLE `organization` (
  `org_id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(128) DEFAULT NULL,
  PRIMARY KEY (`org_id`)
);

DROP TABLE IF EXISTS `player_rank`;
CREATE TABLE `player_rank` (
  `submission_id` int(11) NOT NULL,
  `sigma` float NOT NULL DEFAULT '16.6667',
  `mu` float NOT NULL DEFAULT '50',
  UNIQUE KEY `user_id_UNIQUE` (`submission_id`)
);

DROP TABLE IF EXISTS `ranking`;
CREATE TABLE `ranking` (
  `leaderboard_id` int(11) NOT NULL,
  `user_id` int(11) DEFAULT NULL,
  `submission_id` int(11) NOT NULL,
  `version` int(11) NOT NULL,
  `rank` int(11) NOT NULL,
  `seq` int(11) NOT NULL,
  `rank_change` int(11) NOT NULL,
  `skill` float NOT NULL,
  `skill_change` float NOT NULL,
  `latest` tinyint(4) NOT NULL,
  `age` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  KEY `leaderboard_id` (`leaderboard_id`),
  KEY `submission_id` (`submission_id`),
  KEY `leaderboard_id_2` (`leaderboard_id`,`submission_id`,`rank`)
);

DROP TABLE IF EXISTS `ranking_archive`;
CREATE TABLE `ranking_archive` (
  `leaderboard_id` int(11) NOT NULL,
  `submission_id` int(11) NOT NULL,
  `rank` int(11) NOT NULL,
  `wins` int(11) DEFAULT NULL,
  `losses` int(11) DEFAULT NULL,
  `draws` int(11) DEFAULT NULL,
  `score` double DEFAULT '13',
  KEY `leaderboard_id` (`leaderboard_id`),
  KEY `submission_id` (`submission_id`),
  KEY `leaderboard_id_2` (`leaderboard_id`,`submission_id`,`rank`)
);

DROP TABLE IF EXISTS `settings`;
CREATE TABLE `settings` (
  `name` varchar(20) NOT NULL,
  `number` int(11) NOT NULL DEFAULT '0',
  `string` varchar(255) NOT NULL DEFAULT '',
  PRIMARY KEY (`name`),
  UNIQUE KEY `name_UNIQUE` (`name`)
);

DROP TABLE IF EXISTS `submission`;
CREATE TABLE `submission` (
  `submission_id` int(11) NOT NULL AUTO_INCREMENT,
  `user_id` int(11) NOT NULL,
  `version` int(11) NOT NULL,
  `status` int(11) NOT NULL,
  `timestamp` datetime NOT NULL,
  `comments` varchar(4096) DEFAULT NULL,
  `language_id` int(11) NOT NULL,
  `last_game_timestamp` datetime DEFAULT NULL,
  `latest` tinyint(4) NOT NULL DEFAULT '1',
  `sigma` float NOT NULL DEFAULT '16.6667',
  `mu` float NOT NULL DEFAULT '50',
  PRIMARY KEY (`submission_id`),
  KEY `language_id` (`language_id`),
  KEY `submission_id` (`submission_id`,`user_id`),
  KEY `user_id` (`user_id`),
  KEY `timestamp` (`timestamp`),
  KEY `user_id_2` (`user_id`,`timestamp`),
  KEY `latest` (`latest`)
);

DROP TABLE IF EXISTS `user`;
CREATE TABLE `user` (
  `user_id` int(11) NOT NULL AUTO_INCREMENT,
  `username` varchar(128) NOT NULL,
  `password` varchar(256) NOT NULL,
  `email` varchar(256) NOT NULL,
  `status_id` int(11) NOT NULL,
  `activation_code` varchar(256) NOT NULL DEFAULT '',
  `org_id` int(11) NOT NULL,
  `bio` varchar(4096) DEFAULT NULL,
  `country_id` int(11) DEFAULT NULL,
  `created` datetime DEFAULT NULL,
  `activated` tinyint(1) NOT NULL,
  `admin` tinyint(1) NOT NULL,
  PRIMARY KEY (`user_id`),
  KEY `username` (`username`),
  KEY `user_id` (`user_id`,`username`)
);

DROP TABLE IF EXISTS `user_status_code`;
CREATE TABLE `user_status_code` (
  `status_id` int(11) NOT NULL,
  `name` varchar(256) NOT NULL,
  PRIMARY KEY (`status_id`)
);

DROP TABLE IF EXISTS `worker`;
CREATE TABLE `worker` (
  `worker_id` int(11) NOT NULL AUTO_INCREMENT,
  `ip_address` char(15) NOT NULL,
  `api_key` char(64) NOT NULL,
  PRIMARY KEY (`worker_id`),
  UNIQUE KEY `api_key` (`api_key`)
);

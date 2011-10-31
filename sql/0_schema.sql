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
  `seed_id` int(11) NOT NULL,
  `map_id` int(11) NOT NULL,
  `turns` int(11) NOT NULL,
  `game_length` int(11) NOT NULL,
  `cutoff` varchar(255) NOT NULL,
  `winning_turn` int(11) NOT NULL,
  `ranking_turn` int(11) NOT NULL,
  `timestamp` datetime NOT NULL,
  `worker_id` int(11) unsigned NOT NULL,
  `replay_path` varchar(255) NULL,
  PRIMARY KEY (`game_id`),
  KEY `game_map_id_idx` (`map_id`,`game_id`),
  KEY `game_seed_map_idx` (`seed_id`,`map_id`,`game_id`),
  KEY `game_worker_timestamp_idx` (`worker_id`, `timestamp`),
  KEY `game_timestamp_idx` (`timestamp`),
  KEY `game_map_id_timestamp_idx` (`map_id`, `timestamp`)
);

DROP TABLE IF EXISTS `game_player`;
CREATE TABLE `game_player` (
  `game_id` int(11) NOT NULL,
  `user_id` int(11) NOT NULL,
  `submission_id` int(11) NOT NULL,
  `player_id` int(11) NOT NULL,
  `errors` varchar(1024) DEFAULT NULL,
  `status` varchar(255) DEFAULT NULL,
  `game_rank` int(11) NOT NULL,
  `game_score` int(11) NOT NULL,
  `rank_before` int(11) DEFAULT NULL,
  `rank_after` int(11) DEFAULT NULL,
  `mu_before` float NULL,
  `mu_after` float NULL,
  `sigma_before` float NULL,
  `sigma_after` float NULL,
  `valid` tinyint(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`game_id`,`user_id`),
  UNIQUE KEY `game_player_idx` (`game_id`,`submission_id`),
  KEY `game_player_user_id_idx` (`user_id`, `game_id`),
  KEY `game_player_submission_id_idx` (`submission_id`, `game_id`)
);

DROP TABLE IF EXISTS `language`;
CREATE TABLE `language` (
  `language_id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(64) NOT NULL,
  PRIMARY KEY (`language_id`)
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
  `filename` varchar(256) NOT NULL,
  `priority` int(11) NOT NULL DEFAULT '1',
  `players` int(11) NOT NULL,
  `max_turns` int(11) NOT NULL,
  `timestamp` datetime NOT NULL,
  PRIMARY KEY (`map_id`)
);

DROP TABLE IF EXISTS `matchup`;
CREATE TABLE `matchup` (
  `matchup_id` int(11) NOT NULL AUTO_INCREMENT,
  `seed_id` int(11) NOT NULL,
  `map_id` int(11) NOT NULL,
  `max_turns` int(11) NOT NULL,
  `worker_id` int(11) DEFAULT NULL,
  `error` varchar(4000) NULL,
  `matchup_timestamp` datetime NULL,
  `deleted` tinyint(1) DEFAULT 0,
  PRIMARY KEY (`matchup_id`),
  KEY `matchup_seed_map_idx` (`seed_id`,`map_id`),
  KEY `matchup_map_id_idx` (`map_id`),
  KEY `matchup_deleted_idx` (`deleted`, `worker_id`)
);

DROP TABLE IF EXISTS `matchup_player`;
CREATE TABLE `matchup_player` (
  `matchup_id` int(11) NOT NULL,
  `user_id` int(11) NOT NULL,
  `submission_id` int(11) NOT NULL,
  `player_id` int(11) NOT NULL,
  `mu` float DEFAULT NULL,
  `sigma` float DEFAULT NULL,
  `deleted` tinyint(1) DEFAULT 0,
  PRIMARY KEY (`matchup_id`,`user_id`),
  UNIQUE KEY `matchup_player_idx` (`matchup_id`,`submission_id`),
  KEY `matchup_player_user_id_idx` (`user_id`),
  KEY `matchup_player_player_id_idx` (`matchup_id`, `player_id`)
);

DROP TABLE IF EXISTS `opponents`;
CREATE TABLE `opponents` (
  `game_id` int(11) NOT NULL,
  `user_id` int(11) NOT NULL,
  `opponent_id` int(11) NOT NULL,
  `timestamp` datetime NOT NULL,
  PRIMARY KEY (`game_id`, `user_id`, `opponent_id`),
  KEY `opponents_timestamp_idx` (`timestamp`),
  KEY `opponents_user_game_idx` (`user_id`, `game_id`),
  KEY `opponents_user_opponent_idx` (`user_id`, `opponent_id`)
);

DROP TABLE IF EXISTS `organization`;
CREATE TABLE `organization` (
  `org_id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(128) DEFAULT NULL,
  PRIMARY KEY (`org_id`),
  UNIQUE KEY (`name`)
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
  `errors` varchar(4096) DEFAULT NULL,
  `language_id` int(11) NOT NULL,
  `last_game_timestamp` datetime DEFAULT NULL,
  `latest` tinyint(1) NOT NULL DEFAULT '0',
  `rank` int(11) DEFAULT NULL,
  `rank_change` int(11) DEFAULT NULL,
  `mu` float NOT NULL DEFAULT '50',
  `mu_change` float DEFAULT NULL,
  `sigma` float NOT NULL DEFAULT '16.6667',
  `sigma_change` float DEFAULT NULL,
  `worker_id` int(11) DEFAULT NULL,
  `min_game_id` int(11) DEFAULT NULL,
  `max_game_id` int(11) DEFAULT NULL,
  `game_count` int(11) DEFAULT NULL,
  PRIMARY KEY (`submission_id`),
  KEY `language_id` (`language_id`),
  KEY `submission_id` (`submission_id`,`user_id`),
  KEY `user_id` (`user_id`,`submission_id`),
  KEY `latest` (`latest`,`user_id`),
  KEY `mu_idx` (`mu`),
  KEY `submission_timestamp_idx` (`timestamp`)
);

DROP TABLE IF EXISTS `user`;
CREATE TABLE `user` (
  `user_id` int(11) NOT NULL AUTO_INCREMENT,
  `username` varchar(128) NOT NULL,
  `password` varchar(256) NOT NULL,
  `reset` varchar(256) NULL,
  `email` varchar(256) NOT NULL,
  `status_id` int(11) NOT NULL,
  `activation_code` varchar(256) NOT NULL DEFAULT '',
  `org_id` int(11) NOT NULL,
  `bio` varchar(4096) DEFAULT NULL,
  `country_id` int(11) DEFAULT NULL,
  `created` datetime DEFAULT NULL,
  `activated` tinyint(1) NOT NULL,
  `admin` tinyint(1) NOT NULL,
  `shutdown_date` datetime DEFAULT NULL,
  `max_game_id` int(11) NULL,
  PRIMARY KEY (`user_id`),
  UNIQUE KEY (`username`),
  KEY `user_id` (`user_id`,`username`),
  KEY `user_created_idx` (`created`)
);

DROP TABLE IF EXISTS `user_cookie`;
CREATE TABLE `user_cookie` (
  `user_id` int(11) NOT NULL,
  `cookie` varchar(256) NOT NULL,
  `expires` datetime NOT NULL,
  `forgot` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`user_id`, `cookie`)
);

DROP TABLE IF EXISTS `user_status_code`;
CREATE TABLE `user_status_code` (
  `status_id` int(11) NOT NULL,
  `name` varchar(256) NOT NULL,
  PRIMARY KEY (`status_id`),
  UNIQUE KEY (`name`)
);

DROP TABLE IF EXISTS `worker`;
CREATE TABLE `worker` (
  `worker_id` int(11) NOT NULL AUTO_INCREMENT,
  `ip_address` varchar(15) NOT NULL,
  `api_key` varchar(256) NOT NULL,
  PRIMARY KEY (`worker_id`),
  UNIQUE KEY `api_key` (`api_key`)
);

-- MySQL dump 10.11
--
-- Host: localhost    Database: contest
-- ------------------------------------------------------
-- Server version	5.0.51a-3ubuntu5.7

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `countries`
--

DROP TABLE IF EXISTS `countries`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `countries` (
  `country_id` int(11) NOT NULL,
  `country_code` varchar(8) default NULL,
  `name` varchar(64) default NULL,
  `flag_filename` varchar(16) default NULL,
  PRIMARY KEY  (`country_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `errors`
--

DROP TABLE IF EXISTS `errors`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `errors` (
  `error_id` int(11) NOT NULL auto_increment,
  `submission_id` int(11) NOT NULL,
  `game_id` int(11) default NULL,
  `turn` int(11) default NULL,
  `error` enum('COMPILE_FAILURE','STARTUP_FAILURE','TIMEOUT','UNPARSEABLE_ORDER','BAD_ORDER','QUIT') default NULL,
  `timestamp` datetime NOT NULL,
  PRIMARY KEY  (`error_id`),
  KEY `submission_id` (`submission_id`,`timestamp`),
  KEY `game_id` (`game_id`),
  KEY `error` (`error`,`timestamp`),
  KEY `timestamp` (`timestamp`)
) ENGINE=MyISAM AUTO_INCREMENT=238167 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `games`
--

DROP TABLE IF EXISTS `games`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `games` (
  `game_id` int(11) NOT NULL auto_increment,
  `winner` int(11) default NULL,
  `loser` int(11) default NULL,
  `map_id` int(11) NOT NULL,
  `draw` tinyint(1) NOT NULL default '0',
  `timestamp` datetime NOT NULL,
  `player_one` int(11) NOT NULL,
  `player_two` int(11) NOT NULL,
  `worker` smallint(5) unsigned NOT NULL,
  PRIMARY KEY  (`game_id`),
  KEY `timestamp` (`timestamp`),
  KEY `player_one_2` (`player_one`,`timestamp`),
  KEY `player_two_2` (`player_two`,`timestamp`),
  KEY `winner_3` (`winner`,`timestamp`,`draw`,`game_id`,`loser`),
  KEY `loser_3` (`loser`,`timestamp`,`draw`,`game_id`,`winner`),
  KEY `player_one_all` (`player_one`,`timestamp`,`draw`,`game_id`,`winner`,`loser`,`player_two`),
  KEY `player_two_all` (`player_two`,`timestamp`,`draw`,`game_id`,`winner`,`loser`,`player_one`),
  KEY `worker` (`worker`,`timestamp`),
  KEY `player_one_player_two` (`player_one`,`player_two`)
) ENGINE=MyISAM AUTO_INCREMENT=7091709 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `games_archive`
--

DROP TABLE IF EXISTS `games_archive`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `games_archive` (
  `game_id` int(11) NOT NULL,
  `winner` int(11) NOT NULL,
  `loser` int(11) NOT NULL,
  `map_id` int(11) NOT NULL,
  `draw` tinyint(1) NOT NULL,
  `timestamp` datetime NOT NULL,
  `player_one` int(11) NOT NULL,
  `player_two` int(11) NOT NULL,
  `worker` int(11) NOT NULL,
  PRIMARY KEY  (`game_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `games_old`
--

DROP TABLE IF EXISTS `games_old`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `games_old` (
  `game_id` int(11) NOT NULL auto_increment,
  `winner` int(11) default NULL,
  `loser` int(11) default NULL,
  `map_id` int(11) NOT NULL,
  `draw` tinyint(1) NOT NULL default '0',
  `timestamp` datetime NOT NULL,
  `player_one` int(11) NOT NULL,
  `player_two` int(11) NOT NULL,
  `playback_string` mediumtext NOT NULL,
  PRIMARY KEY  (`game_id`),
  KEY `timestamp` (`timestamp`),
  KEY `loser` (`loser`),
  KEY `player_one` (`player_one`),
  KEY `player_two` (`player_two`),
  KEY `player_one_2` (`player_one`,`timestamp`),
  KEY `player_two_2` (`player_two`,`timestamp`),
  KEY `winner_2` (`winner`,`timestamp`),
  KEY `loser_2` (`loser`,`timestamp`),
  KEY `winner_3` (`winner`,`timestamp`,`draw`,`game_id`,`loser`),
  KEY `loser_3` (`loser`,`timestamp`,`draw`,`game_id`,`winner`)
) ENGINE=MyISAM AUTO_INCREMENT=4480529 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `jail_users`
--

DROP TABLE IF EXISTS `jail_users`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `jail_users` (
  `jail_user_id` int(11) NOT NULL auto_increment,
  `username` varchar(64) NOT NULL,
  `in_use` int(11) NOT NULL default '0',
  PRIMARY KEY  (`jail_user_id`),
  KEY `username` (`username`),
  KEY `in_use` (`in_use`)
) ENGINE=MyISAM AUTO_INCREMENT=193 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `languages`
--

DROP TABLE IF EXISTS `languages`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `languages` (
  `language_id` int(11) NOT NULL auto_increment,
  `name` varchar(64) NOT NULL,
  `main_code_file` varchar(64) NOT NULL,
  `command` varchar(128) NOT NULL,
  `platform_specific_compilation` tinyint(4) NOT NULL default '0',
  PRIMARY KEY  (`language_id`)
) ENGINE=MyISAM AUTO_INCREMENT=20 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `leaderboards`
--

DROP TABLE IF EXISTS `leaderboards`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `leaderboards` (
  `leaderboard_id` int(11) NOT NULL auto_increment,
  `timestamp` datetime NOT NULL,
  `algorithm_name` varchar(64) default NULL,
  `calculation_time` bigint(20) default '0',
  PRIMARY KEY  (`leaderboard_id`),
  KEY `timestamp` (`timestamp`)
) ENGINE=MyISAM AUTO_INCREMENT=157703 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `login_attempts`
--

DROP TABLE IF EXISTS `login_attempts`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `login_attempts` (
  `timestamp` datetime NOT NULL,
  `username` varchar(64) NOT NULL,
  `password` varchar(40) NOT NULL,
  `naive_ip` varchar(18) NOT NULL,
  `real_ip` varchar(18) NOT NULL,
  KEY `timestamp` (`timestamp`,`username`,`password`,`naive_ip`,`real_ip`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `maps`
--

DROP TABLE IF EXISTS `maps`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `maps` (
  `map_id` int(11) NOT NULL auto_increment,
  `name` varchar(64) default NULL,
  `path` varchar(256) default NULL,
  `priority` int(11) default NULL,
  PRIMARY KEY  (`map_id`)
) ENGINE=MyISAM AUTO_INCREMENT=771 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `matchups`
--

DROP TABLE IF EXISTS `matchups`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `matchups` (
  `matchup_id` int(11) NOT NULL auto_increment,
  `player_one` int(11) NOT NULL,
  `player_two` int(11) NOT NULL,
  `map_id` int(11) NOT NULL,
  `dispatch_time` datetime default NULL,
  PRIMARY KEY  (`matchup_id`)
) ENGINE=MyISAM AUTO_INCREMENT=153340 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `organizations`
--

DROP TABLE IF EXISTS `organizations`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `organizations` (
  `org_id` int(11) NOT NULL auto_increment,
  `name` varchar(256) default NULL,
  KEY `org_id` (`org_id`)
) ENGINE=MyISAM AUTO_INCREMENT=341 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `playback`
--

DROP TABLE IF EXISTS `playback`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `playback` (
  `game_id` int(11) NOT NULL,
  `playback_string` mediumblob,
  PRIMARY KEY  (`game_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `playback_test`
--

DROP TABLE IF EXISTS `playback_test`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `playback_test` (
  `game_id` int(11) NOT NULL,
  `playback_string` mediumblob,
  PRIMARY KEY  (`game_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `rankings`
--

DROP TABLE IF EXISTS `rankings`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `rankings` (
  `leaderboard_id` int(11) NOT NULL,
  `submission_id` int(11) NOT NULL,
  `rank` int(11) NOT NULL,
  `wins` int(11) default NULL,
  `losses` int(11) default NULL,
  `draws` int(11) default NULL,
  `score` double default '13',
  KEY `leaderboard_id` (`leaderboard_id`),
  KEY `submission_id` (`submission_id`),
  KEY `leaderboard_id_2` (`leaderboard_id`,`submission_id`,`rank`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `submissions`
--

DROP TABLE IF EXISTS `submissions`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `submissions` (
  `submission_id` int(11) NOT NULL auto_increment,
  `user_id` int(11) NOT NULL,
  `status` int(11) NOT NULL,
  `timestamp` datetime NOT NULL,
  `comments` varchar(4096) default NULL,
  `cleanup_status` int(11) default '0',
  `language_id` int(11) NOT NULL,
  `last_game_timestamp` datetime default NULL,
  `latest` tinyint(4) NOT NULL default '0',
  PRIMARY KEY  (`submission_id`),
  KEY `language_id` (`language_id`),
  KEY `submission_id` (`submission_id`,`user_id`),
  KEY `user_id` (`user_id`),
  KEY `timestamp` (`timestamp`),
  KEY `user_id_2` (`user_id`,`timestamp`),
  KEY `latest` (`latest`)
) ENGINE=MyISAM AUTO_INCREMENT=187547 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `user_status_codes`
--

DROP TABLE IF EXISTS `user_status_codes`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `user_status_codes` (
  `status_id` int(11) NOT NULL,
  `name` varchar(256) NOT NULL,
  PRIMARY KEY  (`status_id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `users`
--

DROP TABLE IF EXISTS `users`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `users` (
  `user_id` int(11) NOT NULL auto_increment,
  `username` varchar(128) NOT NULL,
  `password` varchar(256) NOT NULL,
  `email` varchar(256) NOT NULL,
  `status_id` int(11) NOT NULL,
  `activation_code` varchar(256) NOT NULL default '',
  `org_id` int(11) NOT NULL,
  `bio` varchar(4096) default NULL,
  `country_id` int(11) default NULL,
  `created` datetime default NULL,
  `theme_id` int(11) default NULL,
  `activated` tinyint(1) NOT NULL,
  `admin` tinyint(1) NOT NULL,
  PRIMARY KEY  (`user_id`),
  KEY `username` (`username`),
  KEY `user_id` (`user_id`,`username`)
) ENGINE=MyISAM AUTO_INCREMENT=13261 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `website_themes`
--

DROP TABLE IF EXISTS `website_themes`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `website_themes` (
  `theme_id` int(11) NOT NULL auto_increment,
  `name` varchar(256) default NULL,
  PRIMARY KEY  (`theme_id`)
) ENGINE=MyISAM AUTO_INCREMENT=3 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `workers`
--

DROP TABLE IF EXISTS `workers`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `workers` (
  `worker_id` int(11) NOT NULL auto_increment,
  `ip_address` char(15) NOT NULL,
  `api_key` char(64) NOT NULL,
  PRIMARY KEY  (`worker_id`),
  UNIQUE KEY `api_key` (`api_key`)
) ENGINE=MyISAM AUTO_INCREMENT=134 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2010-11-12 22:20:11

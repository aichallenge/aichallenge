DROP TABLE IF EXISTS `jail_users`;
CREATE TABLE `jail_users` (
  `jail_user_id` int(11) NOT NULL auto_increment,
  `username` varchar(64) NOT NULL,
  `in_use` int(11) NOT NULL default '0',
  PRIMARY KEY  (`jail_user_id`),
  KEY `username` (`username`),
  KEY `in_use` (`in_use`)
);

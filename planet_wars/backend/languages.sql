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
-- Dumping data for table `languages`
--

LOCK TABLES `languages` WRITE;
/*!40000 ALTER TABLE `languages` DISABLE KEYS */;
INSERT INTO `languages` VALUES (1,'Java','MyBot.java','java -jar MyBot.jar',0),(3,'C++','MyBot.cc','./MyBot',1),(4,'C','MyBot.c','./MyBot',1),(6,'Python','MyBot.py','python MyBot.py',0),(7,'C#','MyBot.cs','mono MyBot.exe',0),(8,'Haskell','MyBot.hs','./MyBot',1),(9,'Ruby','MyBot.rb','ruby MyBot.rb',0),(10,'Javascript','MyBot.js','node MyBot.js',0),(11,'PHP','MyBot.php','php MyBot.php',0),(12,'Perl','MyBot.pl','perl MyBot.pl',0),(13,'OCaml','MyBot.ml','./MyBot.native',1),(14,'CoffeeScript','MyBot.coffee','coffee MyBot.coffee',0),(15,'Lisp','MyBot.lisp','./MyBot',1),(0,'Unknown','','',0),(17,'Go','MyBot.go','./MyBot',1),(18,'Groovy','MyBot.groovy','java -cp MyBot.jar:/usr/share/groovy/embeddable/groovy-all-1.7.5.jar MyBot',0);
/*!40000 ALTER TABLE `languages` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2010-11-01 16:00:28

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
-- Dumping data for table `countries`
--

LOCK TABLES `countries` WRITE;
/*!40000 ALTER TABLE `countries` DISABLE KEYS */;
INSERT INTO `countries` VALUES (1,'BR','Brazil','br.png'),(2,'CA','Canada','ca.png'),(3,'CN','China','cn.png'),(4,'EU','European Union','eu.png'),(5,'IN','India','in.png'),(6,'JP','Japan','jp.png'),(7,'MX','Mexico','mx.png'),(8,'RU','Russian Federation','ru.png'),(9,'ZA','South Africa','za.png'),(10,'GB','United Kingdom','gb.png'),(11,'US','United States','us.png'),(12,'AD','Andorra','ad.png'),(13,'AE','United Arab Emirates','ae.png'),(14,'AF','Afghanistan','af.png'),(15,'AG','Antigua and Barbuda','ag.png'),(16,'AI','Anguilla','ai.png'),(17,'AL','Albania','al.png'),(18,'AM','Armenia','am.png'),(19,'AN','Netherlands Antilles','an.png'),(20,'AO','Angola','ao.png'),(21,'AQ','Antarctica','aq.png'),(22,'AR','Argentina','ar.png'),(23,'AS','American Samoa','as.png'),(24,'AT','Austria','at.png'),(25,'AU','Australia','au.png'),(26,'AW','Aruba','aw.png'),(27,'AX','Ã…land Islands','ax.png'),(28,'AZ','Azerbaijan','az.png'),(29,'BA','Bosnia','ba.png'),(30,'BB','Barbados','bb.png'),(31,'BD','Bangladesh','bd.png'),(32,'BE','Belgium','be.png'),(33,'BF','Burkina Faso','bf.png'),(34,'BG','Bulgaria','bg.png'),(35,'BH','Bahrain','bh.png'),(36,'BI','Burundi','bi.png'),(37,'BJ','Benin','bj.png'),(38,'BL','Saint BarthÃ©lemy','bl.png'),(49,'CD','Congo','cd.png'),(48,'CC','Cocos Islands','cc.png'),(47,'BZ','Belize','bz.png'),(46,'BY','Belarus','by.png'),(45,'BW','Botswana','bw.png'),(44,'BV','Bouvet Island','bv.png'),(43,'BT','Bhutan','bt.png'),(42,'BS','Bahamas','bs.png'),(41,'BO','Bolivia','bo.png'),(40,'BN','Brunei Darussalam','bn.png'),(39,'BM','Bermuda','bm.png'),(50,'CF','Cent Afr Republic','cf.png'),(51,'CG','Congo','cg.png'),(52,'CH','Switzerland','ch.png'),(53,'CI','Ivory Coast','ci.png'),(54,'CK','Cook Islands','ck.png'),(55,'CL','Chile','cl.png'),(56,'CM','Cameroon','cm.png'),(57,'CO','Colombia','co.png'),(58,'CR','Costa Rica','cr.png'),(59,'CU','Cuba','cu.png'),(60,'CV','Cape Verde','cv.png'),(61,'CX','Christmas Island','cx.png'),(62,'CY','Cyprus','cy.png'),(63,'CZ','Czech Republic','cz.png'),(64,'DE','Germany','de.png'),(65,'DJ','Djibouti','dj.png'),(66,'DK','Denmark','dk.png'),(67,'DM','Dominica','dm.png'),(68,'DO','Dominican Republic','do.png'),(69,'DZ','Algeria','dz.png'),(70,'EC','Ecuador','ec.png'),(71,'EE','Estonia','ee.png'),(72,'EG','Egypt','eg.png'),(73,'EH','Western Sahara','eh.png'),(74,'ER','Eritrea','er.png'),(75,'ES','Spain','es.png'),(76,'ET','Ethiopia','et.png'),(77,'FI','Finland','fi.png'),(78,'FJ','Fiji','fj.png'),(79,'FK','Falkland Islands','fk.png'),(80,'FM','Micronesia','fm.png'),(81,'FO','Faroe Islands','fo.png'),(82,'FR','France','fr.png'),(83,'GA','Gabon','ga.png'),(84,'GD','Grenada','gd.png'),(85,'GE','Georgia','ge.png'),(86,'GF','French Guiana','gf.png'),(87,'GG','Guernsey','gg.png'),(88,'GH','Ghana','gh.png'),(89,'GI','Gibraltar','gi.png'),(90,'GL','Greenland','gl.png'),(91,'GM','Gambia','gm.png'),(92,'GN','Guinea','gn.png'),(93,'GP','Guadeloupe','gp.png'),(94,'GQ','Equatorial Guinea','gq.png'),(95,'GR','Greece','gr.png'),(96,'GS','South Georgia','gs.png'),(97,'GT','Guatemala','gt.png'),(98,'GU','Guam','gu.png'),(99,'GW','Guinea-Bissau','gw.png'),(100,'GY','Guyana','gy.png'),(101,'HK','Hong Kong','hk.png'),(102,'HM','Heard Island','hm.png'),(103,'HN','Honduras','hn.png'),(104,'HR','Croatia','hr.png'),(105,'HT','Haiti','ht.png'),(106,'HU','Hungary','hu.png'),(107,'ID','Indonesia','id.png'),(108,'IE','Ireland','ie.png'),(109,'IL','Israel','il.png'),(110,'IM','Isle of Man','im.png'),(111,'IQ','Iraq','iq.png'),(112,'IR','Iran','ir.png'),(113,'IS','Iceland','is.png'),(114,'IT','Italy','it.png'),(115,'JE','Jersey','je.png'),(116,'JM','Jamaica','jm.png'),(117,'JO','Jordan','jo.png'),(118,'KE','Kenya','ke.png'),(119,'KG','Kyrgyzstan','kg.png'),(120,'KH','Cambodia','kh.png'),(121,'KI','Kiribati','ki.png'),(122,'KM','Comoros','km.png'),(123,'KN','Saint Kitts and Nevis','kn.png'),(124,'KP','North Korea','kp.png'),(125,'KR','South Korea','kr.png'),(126,'KW','Kuwait','kw.png'),(127,'KY','Cayman Islands','ky.png'),(128,'KZ','Kazakhstan','kz.png'),(129,'LA','Laos','la.png'),(130,'LB','Lebanon','lb.png'),(131,'LC','Saint Lucia','lc.png'),(132,'LI','Liechtenstein','li.png'),(133,'LK','Sri Lanka','lk.png'),(134,'LR','Liberia','lr.png'),(135,'LS','Lesotho','ls.png'),(136,'LT','Lithuania','lt.png'),(137,'LU','Luxembourg','lu.png'),(138,'LV','Latvia','lv.png'),(139,'LY','Libya','ly.png'),(140,'MA','Morocco','ma.png'),(141,'MC','Monaco','mc.png'),(142,'MD','Moldova, Republic of','md.png'),(143,'ME','Montenegro','me.png'),(144,'MF','Saint Martin','mf.png'),(145,'MG','Madagascar','mg.png'),(146,'MH','Marshall Islands','mh.png'),(147,'MK','Macedonia','mk.png'),(148,'ML','Mali','ml.png'),(149,'MM','Myanmar','mm.png'),(150,'MN','Mongolia','mn.png'),(151,'MO','Macao','mo.png'),(152,'MQ','Martinique','mq.png'),(153,'MR','Mauritania','mr.png'),(154,'MS','Montserrat','ms.png'),(155,'MT','Malta','mt.png'),(156,'MU','Mauritius','mu.png'),(157,'MV','Maldives','mv.png'),(158,'MW','Malawi','mw.png'),(159,'MY','Malaysia','my.png'),(160,'MZ','Mozambique','mz.png'),(161,'NA','Namibia','na.png'),(162,'NC','New Caledonia','nc.png'),(163,'NE','Niger','ne.png'),(164,'NF','Norfolk Island','nf.png'),(165,'NG','Nigeria','ng.png'),(166,'NI','Nicaragua','ni.png'),(167,'NL','Netherlands','nl.png'),(168,'NO','Norway','no.png'),(169,'NP','Nepal','np.png'),(170,'NR','Nauru','nr.png'),(171,'NU','Niue','nu.png'),(172,'NZ','New Zealand','nz.png'),(173,'OM','Oman','om.png'),(174,'PA','Panama','pa.png'),(175,'PE','Peru','pe.png'),(176,'PF','French Polynesia','pf.png'),(177,'PG','Papua New Guinea','pg.png'),(178,'PH','Philippines','ph.png'),(179,'PK','Pakistan','pk.png'),(180,'PL','Poland','pl.png'),(181,'PN','Pitcairn','pn.png'),(182,'PR','Puerto Rico','pr.png'),(183,'PS','Palestine','ps.png'),(184,'PT','Portugal','pt.png'),(185,'PW','Palau','pw.png'),(186,'PY','Paraguay','py.png'),(187,'QA','Qatar','qa.png'),(188,'RE','RÃ©union','re.png'),(189,'RO','Romania','ro.png'),(190,'RS','Serbia','rs.png'),(191,'RW','Rwanda','rw.png'),(192,'SA','Saudi Arabia','sa.png'),(193,'SB','Solomon Islands','sb.png'),(194,'SC','Seychelles','sc.png'),(195,'SD','Sudan','sd.png'),(196,'SE','Sweden','se.png'),(197,'SG','Singapore','sg.png'),(198,'SH','Saint Helena','sh.png'),(199,'SI','Slovenia','si.png'),(200,'SJ','Svalbard','sj.png'),(201,'SK','Slovakia','sk.png'),(202,'SL','Sierra Leone','sl.png'),(203,'SM','San Marino','sm.png'),(204,'SN','Senegal','sn.png'),(205,'SO','Somalia','so.png'),(206,'SR','Suriname','sr.png'),(207,'ST','Sao Tome + Principe','st.png'),(208,'SV','El Salvador','sv.png'),(209,'SY','Syrian Arab Republic','sy.png'),(210,'SZ','Swaziland','sz.png'),(211,'TC','Turks and Caicos','tc.png'),(212,'TD','Chad','td.png'),(213,'TG','Togo','tg.png'),(214,'TH','Thailand','th.png'),(215,'TJ','Tajikistan','tj.png'),(216,'TK','Tokelau','tk.png'),(217,'TL','Timor-Leste','tl.png'),(218,'TM','Turkmenistan','tm.png'),(219,'TN','Tunisia','tn.png'),(220,'TO','Tonga','to.png'),(221,'TR','Turkey','tr.png'),(222,'TT','Trinidad and Tobago','tt.png'),(223,'TV','Tuvalu','tv.png'),(224,'TW','Taiwan','tw.png'),(225,'TZ','Tanzania','tz.png'),(226,'UA','Ukraine','ua.png'),(227,'UG','Uganda','ug.png'),(228,'UY','Uruguay','uy.png'),(229,'UZ','Uzbekistan','uz.png'),(230,'VC','Saint Vincent','vc.png'),(231,'VE','Venezuela','ve.png'),(232,'VG','Virgin Islands, British','vg.png'),(233,'VI','Virgin Islands, U.S.','vi.png'),(234,'VN','Viet Nam','vn.png'),(235,'VU','Vanuatu','vu.png'),(236,'WF','Wallis and Futuna','wf.png'),(237,'WS','Samoa','ws.png'),(238,'YE','Yemen','ye.png'),(239,'YT','Mayotte','yt.png'),(240,'ZM','Zambia','zm.png'),(241,'ZW','Zimbabwe','zw.png');
/*!40000 ALTER TABLE `countries` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2010-10-24  3:22:21

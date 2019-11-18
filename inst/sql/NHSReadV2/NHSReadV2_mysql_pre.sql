drop table if exists read_version2;

CREATE TABLE `read_version2` (
  `read_code` varchar(5) NOT NULL COLLATE utf8_bin,
  `term_30` varchar(30) DEFAULT NULL,
  `term_60` varchar(60) DEFAULT NULL,
  `term_198` varchar(198) DEFAULT NULL,
  `X1` varchar(50) DEFAULT NULL,
  `X2` varchar(50) DEFAULT NULL,
  `X3` varchar(50) DEFAULT NULL,
  `X4` varchar(50) DEFAULT NULL,
  `X5` varchar(50) DEFAULT NULL,
  `X6` varchar(50) DEFAULT NULL,
  `X7` varchar(50) DEFAULT NULL,
  `X8` varchar(50) DEFAULT NULL,
  `X9` varchar(50) DEFAULT NULL,
  `term` varchar(198) DEFAULT NULL,
  `synonym` bit(1) DEFAULT 0,
  PRIMARY KEY (`read_code`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

load data local infile '<data-file-path>/V2/Unified/Corev2.all'
into table read_version2
fields terminated by ',' enclosed by '"'
LINES TERMINATED BY '\r\n';

load data local infile '/Users/rcf8/Docker/add-container/local/dict/readdrugs/NHS_READDRUGS/Derived/v2/Cv2drug.all'
into table read_version2
fields terminated by ',' enclosed by '"'
LINES TERMINATED BY '\r\n';

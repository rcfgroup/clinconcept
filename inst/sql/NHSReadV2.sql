drop table if exists read_version2;

CREATE TABLE `read_version2` (
  `code` varchar(5) NOT NULL,
  `term_30` varchar(30) DEFAULT NULL,
  `term_60` varchar(60) DEFAULT NULL,
  `term_198` varchar(198) DEFAULT NULL,
  PRIMARY KEY (`code`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

load data local infile '<read-code-path>/V2/Unified/Corev2.all'
into table read_version2
fields terminated by ',' enclosed by '"';

update read_version2 set term_30=null where term_30='' or  term_30='\r';
update read_version2 set term_60=null where term_60='' or term_60='\r';
update read_version2 set term_198=null where term_198='' or term_198='\r';

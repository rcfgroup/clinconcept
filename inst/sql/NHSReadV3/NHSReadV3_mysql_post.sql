load data local infile '<data-file-path>/V3/Terms.v3'
into table read_terms_version3
fields terminated by '|'
LINES TERMINATED BY '\r\n';


load data local infile '<data-file-path>/V3/Descrip.v3'
into table read_link_version3
fields terminated by '|' (`read_code`,`term_id`,`desc_type`)
set id = null;


load data local infile '<data-file-path>/V3/Concept.v3'
into table read_concept_version3
fields terminated by '|' (`read_code`,`status`,`ling_role`)
set id = null;


update read_terms_version3 set term_30=null where term_30='' or term_30='\r';
update read_terms_version3 set term_60=null where term_60='' or term_60='\r';
update read_terms_version3 set term_198=null where term_198='' or term_198='\r';

create table `read_version3`
select link.`read_code`,
coalesce(terms.term_198,terms.term_60,terms.term_30) as term,
terms.`term_30`,
terms.`term_60`,
terms.`term_198`,
terms.`term_id`,
link.`desc_type`,
0 as synonym,
concept.status
from
read_terms_version3 terms
left join read_link_version3 link on terms.term_id = link.term_id
left join read_concept_version3 concept on concept.read_code = link.read_code
where link.`read_code` is not null;

alter table `read_version3` modify `term` varchar(198) character set utf8 collate utf8_unicode_ci;

update `read_version3` set synonym=1 where `read_version3`.`desc_type`='S';
alter table `read_version3` drop column `desc_type`;
alter table `read_version3` add index read_code(`read_code`);

drop table read_link_version3;
drop table read_terms_version3;
drop table read_concept_version3;

drop table if exists read_parents_version3;
CREATE TABLE `read_parents_version3` (
`id` integer signed auto_increment,
  `read_code` varchar(5) NOT NULL DEFAULT '',
  `parent_read_code` varchar(5) DEFAULT NULL,
  `list_order` varchar(2) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;

load data local infile '<data-file-path>/V3/V3hier.v3'
into table read_parents_version3
fields terminated by '|' (`read_code`,`parent_read_code`,`list_order`)
set id = null;


alter table `read_parents_version3` add index read_code(`read_code`), add index parent_read_code(`parent_read_code`);


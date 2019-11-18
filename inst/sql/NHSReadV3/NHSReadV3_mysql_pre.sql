drop table if exists read_terms_version3;
drop table if exists read_link_version3;
drop table if exists read_parents_version3;
drop table if exists read_concept_version3;
drop table if exists read_version3;

CREATE TABLE `read_terms_version3` (
  `term_id` varchar(5) NOT NULL DEFAULT '',
  `term_status` char(1) DEFAULT NULL,
  `term_30` varchar(30) DEFAULT NULL,
  `term_60` varchar(60) DEFAULT NULL,
  `term_198` varchar(198) DEFAULT NULL,
  KEY (`term_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;

CREATE TABLE `read_concept_version3` (
`id` integer unsigned auto_increment,
  `read_code` varchar(5) DEFAULT NULL,
  `status` varchar(1) DEFAULT NULL,
  `ling_role` varchar(1) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY(`read_code`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;

CREATE TABLE `read_link_version3` (
`id` integer unsigned auto_increment,
  `read_code` varchar(5) DEFAULT NULL,
  `term_id` varchar(5) DEFAULT NULL,
  `desc_type` varchar(1) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY(`read_code`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;

CREATE TABLE `read_parents_version3` (
`id` integer unsigned auto_increment,
  `read_code` varchar(5) NOT NULL DEFAULT '',
  `parent_read_code` varchar(5) DEFAULT NULL,
  `list_order` varchar(2) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;

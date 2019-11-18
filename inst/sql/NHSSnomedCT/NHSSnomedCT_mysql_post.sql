alter table snomed_description modify column snomed_code VARCHAR(20) not null;
alter table snomed_relationship modify column snomed_code VARCHAR(20) not null, modify column parent_snomed_code varchar(20) not null;

create index s_snomed_code on snomed_description(snomed_code);
create index sp_snomed_code on snomed_relationship(snomed_code);
create index sp_parent_snomed_code on snomed_relationship(parent_snomed_code);

create index s_snomed_code on snomed_description(snomed_code);
create index sp_snomed_code on snomed_relationship(snomed_code);
create index sp_parent_snomed_code on snomed_relationship(parent_snomed_code);

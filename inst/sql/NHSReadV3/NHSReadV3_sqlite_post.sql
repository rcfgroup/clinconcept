update read_terms_version3 set term_30=null where term_30='' or term_30='\r';
update read_terms_version3 set term_60=null where term_60='' or term_60='\r';
update read_terms_version3 set term_198=null where term_198='' or term_198='\r';

create table read_version3 (
read_code COLLATE BINARY,
term,
term_30,
term_60,
term_198,
term_id COLLATE BINARY,
desc_type,
synonym,
status
);

insert into read_version3
select link.read_code as read_code,
coalesce(term_198,term_60,term_30) as term,
terms.term_30 as term_30,
terms.term_60 as term_60,
terms.term_198 as term_198,
terms.term_id as term_id,
link.desc_type as desc_type,
'0' as synonym,
concept.status as status
from
read_terms_version3 terms
left join read_link_version3 as link on terms.term_id = link.term_id
left join read_concept_version3 as concept on concept.read_code = link.read_code
where link.read_code is not null;

update read_version3 set synonym='1' where read_version3.desc_type='S';
create index r_read_code on read_version3(read_code);
create index rp_read_code on read_parents_version3(read_code);
create index rp_parent_read_code on read_parents_version3(parent_read_code);

drop table read_link_version3;
drop table read_terms_version3;
drop table read_concept_version3;


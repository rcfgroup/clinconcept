update read_version2 set term_30=null where term_30='' or  term_30='\r';
update read_version2 set term_60=null where term_60='' or term_60='\r';
update read_version2 set term_198=null where term_198='' or term_198='\r';
update read_version2 set term=coalesce(term_198,term_60,term_30), synonym=0;


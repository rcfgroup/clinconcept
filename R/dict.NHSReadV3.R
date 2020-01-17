utils::globalVariables(c("status","active","read_code","synonym"))

get_ctable_name.NHSReadV3<-function(dict) {"read_version3"}
get_ctable_code_field.NHSReadV3<-function(dict) {"read_code"}
get_ctable_term_field.NHSReadV3<-function(dict) {"term"}
get_ptable_name.NHSReadV3<-function(dict) {"read_parents_version3"}
get_ptable_code_field.NHSReadV3<-function(dict) {"read_code"}
get_ptable_parent_field.NHSReadV3<-function(dict) {"parent_read_code"}

build_concept_tables.sqlite.NHSReadV3 <- function(dict,replacements) {
  #read CSV data into SQLite table
  data_file_path = replacements[['data-file-path']]
  sqlite<-dict$src

  terms_fields<-c("term_id","term_status","term_30","term_60","term_198")
  terms_types<-list(term_id="COLLATE BINARY",term_status="",term_30="",term_60="",term_198="")
  link_fields<-c("read_code","term_id","desc_type")
  link_types<-list(read_code="COLLATE BINARY",term_id="COLLATE BINARY",desc_type="")
  concept_fields<-c("read_code","status","ling_role","X1")
  concept_types<-list(read_code="COLLATE BINARY",status="",ling_role="",X1="")
  parents_fields<-c("read_code","parent_read_code","list_order")
  parents_types<-list(read_code="COLLATE BINARY",parent_read_code="COLLATE BINARY")

  write_table_from_file(sqlite,"read_terms_version3",paste0(data_file_path,"/V3/Terms.v3"),col_names=terms_fields,col_types=terms_types,delim="|")
  write_table_from_file(sqlite,"read_link_version3",paste0(data_file_path,"/V3/Descrip.v3"),link_fields,link_types,delim="|")
  write_table_from_file(sqlite,"read_concept_version3",paste0(data_file_path,"/V3/Concept.v3"),concept_fields,concept_types,delim="|")
  write_table_from_file(sqlite,"read_parents_version3",paste0(data_file_path,"/V3/V3Hier.v3"),parents_fields,parents_types,delim="|")
}

get_child_codes.NHSReadV3<-function(dict,code,immediate_children=F,current_only=F) {
  codes<-extract_relations_from_dag(dict,code,immediate_children,children=T)
  if(current_only) {
    code_tbl<-dplyr::tbl(dict$src,get_ctable_name(dict)) %>% dplyr::filter(read_code %in% codes & status=='C') %>% dplyr::collect()
    return(unique(code_tbl$read_code))
  }
  codes
}
get_parent_codes.NHSReadV3<-function(dict,code,immediate_parents=F,current_only=F) {
  codes<-extract_relations_from_dag(dict,code,immediate_parents,children=F)

  if(current_only) {
    code_tbl<-dplyr::tbl(dict$src,get_ctable_name(dict)) %>% dplyr::filter(read_code %in% codes & status=='C') %>% dplyr::collect()
    return(unique(code_tbl$read_code))
  }
  codes
}

get_relationships.NHSReadV3<-function(dict,code,children) {
  dplyr::collect(extract_relations_from_dag(dict,code,immediate_relations=F,children=children))
}

# ' @importFrom rlang .data

is_code_present.NHSReadV3<-function(dict,code) {
  read_tbl <- dict$src %>% dplyr::tbl("read_version3") %>% dplyr::select(c("read_code","synonym")) %>% dplyr::filter(read_code==code & synonym!='1')
  codes<-read_tbl %>% dplyr::count() %>% dplyr::collect()
  codes$n>0
}


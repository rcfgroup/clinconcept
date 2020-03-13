utils::globalVariables(c("active","snomed_code"))

get_ctable_name.NHSSnomedCT<-function(dict) {"snomed_description"}
get_ctable_code_field.NHSSnomedCT<-function(dict) {"snomed_code"}
get_ctable_term_field.NHSSnomedCT<-function(dict) {"term"}
get_ptable_name.NHSSnomedCT<-function(dict) {"snomed_relationship"}
get_ptable_code_field.NHSSnomedCT<-function(dict) {"snomed_code"}
get_ptable_parent_field.NHSSnomedCT<-function(dict) {"parent_snomed_code"}

build_concept_tables.NHSSnomedCT <- function(dict,replacements) {
  data_file_path = replacements[['data-file-path']]
  file_path<-paste0(data_file_path,"/Full/Terminology")
  write_table_from_file(dict$src,"snomed_description",find_matching_file(file_path,"_Description"),delim="\t",file_col_types="ccccccccc", col_names = c("id", "effectiveTime", "active", "moduleId", "snomed_code",
                                                                                                                                                       "languageCode", "typeId", "term", "caseSignificanceId"),skip_rows=1)
  write_table_from_file(dict$src,"snomed_relationship",find_matching_file(file_path,"_Relationship"),delim="\t",file_col_types="cccccccccc",col_names = c("id", "effectiveTime", "active", "moduleId", "snomed_code",                                                                                                                                                "parent_snomed_code", "relationshipGroup", "typeId", "characteristicTypeId",                                                                                                                                                        "modifierId"),skip_rows=1)
}

# ' @importFrom rlang .data
get_child_codes.NHSSnomedCT<-function(dict,code,immediate_children=F,active_only=F) {
  codes<-extract_relations_from_dag(dict,code,immediate_children,children=T)

  if(active_only) {
    code_tbl<-dplyr::tbl(dict$src,get_ctable_name(dict)) %>% dplyr::filter(snomed_code %in% codes & active=='1') %>% dplyr::collect()
    return(unique(code_tbl$snomed_code))
  }
  codes
}

# ' @importFrom rlang .data
get_parent_codes.NHSSnomedCT<-function(dict,code,immediate_parents=F,active_only=F) {
  codes<-extract_relations_from_dag(dict,code,immediate_parents,children=F)

  if(active_only) {
    code_tbl<-dplyr::tbl(dict$src,get_ctable_name(dict)) %>% dplyr::filter(snomed_code %in% codes & active=='1') %>% dplyr::collect()
    return(unique(code_tbl$snomed_code))
  }
  codes
}



get_ctable_name.NHSICD10<-function(dict) {
  "icd10_edition5"
}

get_ctable_code_field.NHSICD10<-function(dict) {
  "icd10_code"
}

get_ctable_term_field.NHSICD10<-function(dict) {
  "term"
}

build_concept_tables.NHSICD10 <- function(dict,replacements) {
  #read CSV data into table
  file_path<-paste0(replacements[['data-file-path']],"/Content/")
  icd10<-readr::read_tsv(find_matching_file(file_path,"ICD10_Edition5_CodesAndTitlesAndMetadata"),
                         col_names=c("icd10_code","alt_icd10_code","usage","usage_uk","term","modifier_4","modifier_5","qualifiers","gender_mask","min_age","max_age","tree_description"),skip=1)
  DBI::dbWriteTable(dict$src,"icd10_edition5",icd10)
}

get_child_codes.NHSICD10<-function(dict,code,immediate_children=F,active_only=F) {
  if(grepl(".", code, fixed=TRUE)) {
    stop(paste("Provided code",code,"cannot have children"));
  }
  query<-paste0(code,".%")
  codes<-dplyr::tbl(dict$src,get_ctable_name(dict))
  parsed<-parse(text = paste0("dplyr::filter(codes,",get_ctable_code_field(dict)," %like% query)"))

  codes<-eval(parsed)
  codes<-dplyr::collect(codes)
  codes$icd10_code
}

get_parent_codes.NHSICD10<-function(dict,code,immediate_parents=F,active_only=F) {
  if(!grepl(".", code, fixed=TRUE)) {
    stop(paste("Provided code",code,"cannot have parent"));
  }
  bits<-strsplit(code,".",fixed=T)[[1]]
  query<-bits[1]
  codes<-dplyr::tbl(dict$src,get_ctable_name(dict))
  parsed<-parse(text = paste0("dplyr::filter(codes,",get_ctable_code_field(dict)," == query)"))

  codes<-eval(parsed)
  codes<-dplyr::collect(codes)
  codes$icd10_code
}

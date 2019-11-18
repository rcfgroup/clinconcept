library(dplyr)
library(magrittr)
get_child_codes<-function(dict,parent_code,immediate_children=F,output_codes=F,current_only=F) {
  UseMethod("get_child_codes", dict)
}

#' Internal function user to get descendent read v2 codes from the database (called explicitly by get_child_readcodes)
#'
#' @param config rexceed config object (see new_config)
#' @param read_code READ code to obtain descendents for
#' @param immediate_children indicates if only one level (e.g. just children) should be returned
#' @return character vector including descendent codes (excluding read_code provided)
#' @examples
#' config<-rexceed::rexceed_config_from_home("20160103")
#' h3_v2_children<-rexceed::ref_get_child_v2_readcodes(config,"H3...",TRUE)
#
get_child_codes.NHSReadV2<-function(dict,parent_code,immediate_children=F,output_codes=F,current_only=F) {
  #obtain positions of dots in parent_code
  bits<-lapply(strsplit(parent_code, ''), function(x) which(x == '.'))
  if(length(bits)==1 && length(bits[[1]])==0) {
    stop(paste("Provided parent_code",parent_code,"will not have children"));
  }
  firstDot<-bits[[1]][1]

  if(immediate_children) {
    #retrieve immediate descendents by querying only first batch of codes (e.g. given 'H32..' use 'H32%.')
    query<-paste0(substring(parent_code,1,firstDot[1]-1),"%",paste0(rep(".",nchar(parent_code)-firstDot),collapse=""))
  }
  else {
    #retrieve all descendents by querying all batches of codes (e.g. given 'H32..' use 'H32%')
    query<-paste0(substring(parent_code,1,firstDot[1]-1),"%")
  }

  codes<-dplyr::tbl(dict$src,"read_version2")
  if(dict$type=="sqlite") {
    dbExecute(dict$src,"pragma case_sensitive_like = yes")
  }
  filter(codes,code %like% query)
  if(dict$type=="sqlite") {
    dbExecute(dict$src,"pragma case_sensitive_like = no")
  }
}

#' Internal function user to get descendent read v2 codes from the database (called explicitly by get_child_readcodes)
#'
#' @param config rexceed config object (see new_config)
#' @param read_code READ code to obtain descendents for
#' @param immediate_children indicates if only one level (e.g. just children) should be returned
#' @return character vector including descendent codes (excluding read_code provided)
#' @examples
#' config<-rexceed::rexceed_config_from_home("20160103")
#' h3_v2_children<-rexceed::ref_get_child_v2_readcodes(config,"H3...",TRUE)
#
get_child_codes.NHSReadV3<-function(dict,parent_code,immediate_children=F,current_only=F) {
  child_codes<-get_read_v3_descendent_codes(dict$src,parent_code,immediate_children);

  codes<-dplyr::tbl(dict$src,"read_version3")
  if(length(child_codes)==0) {
    return(NA)
  }
  else if(length(child_codes)==1) {
    codes<-filter(codes,read_code == child_codes & synonym != '1')
  }
  else {
    codes<-filter(codes,read_code %in% child_codes & synonym != '1')
  }
  if(current_only) {
    codes<-filter(codes,status == 'C')
  }
  collect(codes)$read_code;
}


is_code_present<-function(dict,code) {
  UseMethod("is_code_present")
}
is_code_present.NHSReadV3<-function(dict,code) {
  read_tbl <- dict$src %>% dplyr::tbl("read_version3") %>% dplyr::select(c("read_code","synonym")) %>% dplyr::filter(read_code==code & synonym!='1')
  codes<-read_tbl %>% count() %>% collect()
  codes$n>0
}


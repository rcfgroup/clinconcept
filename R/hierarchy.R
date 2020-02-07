#' @importFrom methods setRefClass
setRefClass("code_data",fields=list(relations="vector",last_relations="vector",relations_by_code="list"))

#' Get parent codes from the supplied clinical dictionary
#'
#' @param dict Clinical dictionary object
#' @param code Clinical code to get parent codes for
#' @param immediate_parents T/F flag to return only parents from only the level above (default FALSE)
#' @param active_only T/F flag to return only active codes (default TRUE)
#'
#' @export
#'
#' @examples
#'\dontrun{
#' dict<-cc_from_file("/path/to/dictconfig")
#' h3_parent_codes<-get_parent_codes(dict,"H3...",immediate_parents=F)
#'
#'}
get_parent_codes<-function(dict,code,immediate_parents=F,active_only=T) {
  UseMethod("get_parent_codes")
}
#' Get child codes from the supplied clinical dictionary
#'
#' @param dict Clinical dictionary object
#' @param code Clinical code to get child codes for
#' @param immediate_children T/F flag to return only children from the first level below (default FALSE)
#' @param active_only T/F flag to return only active codes (default TRUE)
#'
#' @export
#'
#' @examples
#'\dontrun{
#' dict<-cc_from_file("/path/to/dictconfig")
#' h3_child_codes<-get_parent_codes(dict,"H31..",immediate_children=F)
#'}
get_child_codes<-function(dict,code,immediate_children=F,active_only=T) {
  UseMethod("get_child_codes")
}

#' Get child/parent pairs in a data.frame for a specific code
#'
#' @param dict Clinical dictionary object
#' @param code Clinical code to check
#' @param children T/F flag to search child codes (T) or parent codes (F)
#'
#' @return data.frame of child/parent codes
#' @export
#'
#' @examples
#'\dontrun{
#' dict<-cc_from_file("/path/to/dictconfig")
#' h3_rels<-get_relationships(dict,"H31..",children=F)
#'}
get_relationships<-function(dict,code,children) {
  UseMethod("get_relationships")
}

fetch_relation_codes<-function(src,rel_code,tbl_name, code_field, parent_code_field,children=T) {
  rel_tbl<-dplyr::tbl(src,tbl_name) %>% dplyr::select(c(code_field,parent_code_field))

  if(children) {
    option_str<-paste0(parent_code_field,"== rel_code");
  }
  else {
    option_str<-paste0(code_field,"== rel_code");
  }

  rel_tbl<-eval(parse(text = paste0("dplyr::filter(rel_tbl,", option_str, ")")))

  relation_rows<-dplyr::collect(rel_tbl)
  if(nrow(relation_rows)==0) {
    return(NULL);
  }

  if(children) {
    relation_codes<-as.vector(relation_rows[[code_field]]);
  }
  else {
    relation_codes<-as.vector(relation_rows[[parent_code_field]]);
  }
  relation_codes<-unique(relation_codes)
  relation_codes
}
#' Internal recursive function used to get all relation codes from the database (called explicitly by get_child_codes)
#'
#' @param src dplyr src object
#' @param rel_code Code to obtain relations for
#' @param immediate_relations indicates if only one level (e.g. just immediate children) should be returned
#' @param children indicates if children should be returned
#' @param tbl_name name of parent/child table to use
#' @param code_field name of child code field in table
#' @param parent_code_field name of parent code field in table
#' @param code_data_obj a 'code_data' object instance which contains previously stored relations
#' @return Character vector containing relation codes (this excludes the code provided to the function)
#
get_relation_codes<-function(src,rel_code,immediate_relations=F,children=T,tbl_name,code_field,parent_code_field,code_data_obj) {
  relation_codes<-fetch_relation_codes(src,rel_code,tbl_name,code_field,parent_code_field,children)

  if(length(relation_codes)==0 | is.null(relation_codes)) {
    code_data_obj$last_relations<-c(NA)
    return()
  }

  #if immediate_relations flag is TRUE then do not carry out recursion
  if(immediate_relations) {
    code_data_obj$last_relations<-relation_codes
    code_data_obj$relations<-c(code_data_obj$relations,relation_codes)
    return()
  }

  #loop through each child code
  code_data_obj$last_relations<-relation_codes
  code_data_obj$relations_by_code[[rel_code]]<-relation_codes
  code_data_obj$relations<-unique(c(code_data_obj$relations,relation_codes))

  for(relation_code in relation_codes) {
    if(relation_code %in% names(code_data_obj$relations_by_code)) {
      code_data_obj$last_relations<-c(NA)
      return()
    }
    get_relation_codes(src,relation_code,F,children,tbl_name,code_field,parent_code_field, code_data_obj)
  }

}

#' Internal function used to get relations from a diacyclic graph-type dictionary such as READ v3 or SNOMED-CT.
#'
#' @param dict Clinical dictionary object
#' @param code Code to use a basis for relationship extraction
#' @param immediate_relations T/F flag to return just immediate relations or all relations
#' @param children T/F flag to return children (T) or parents (F)
#' @importFrom methods "new"
extract_relations_from_dag<-function(dict,code,immediate_relations,children){
  code_data<-methods::new("code_data")
  get_relation_codes(dict$src,code,immediate_relations,get_ptable_name(dict),code_field=get_ptable_code_field(dict),parent_code_field=get_ptable_parent_field(dict),children=children, code_data_obj=code_data);

  rel_codes<-code_data$relations
  if(length(rel_codes)==0) {
    return(NA)
  }
  rel_codes
}
#' Internal function used to create query parameters for getting children from a hierachical dictionary such as READ v2 or ICD10.
#'
#' @param code Code to use as a basis for query parameters
#' @param immediate T/F flag to return just immediate children or all children
#'
#' @return List containing dplyr::filter query op and value
#'
create_query_params_for_children_from_hierarchy<-function(code,immediate=F) {
  dot_pos<-get_dot_position(code)
  code_length=nchar(code)

  if(immediate) {
    return(list(
      op=" %like% ",
      value=paste0(substring(code,1,dot_pos-1),"%",paste0(rep(".",code_length-dot_pos),collapse=""))
    ))
  }
  #retrieve all relations by querying all batches of codes (e.g. given 'H32..' use 'H32%')
  return(list(
    op="%like%",
    value=paste0(substring(code,1,dot_pos-1),"%")
  ))
}
#' Internal function used to create query parameters for getting children from a hierachical dictionary such as READ v2 or ICD10.
#'
#' @param code Code to use as a basis for query parameters
#' @param immediate T/F flag to return just immediate parents or all parents
#'
#' @return List containing dplyr::filter query op and value
#'
create_query_params_for_parents_from_hierarchy<-function(code,immediate=F) {
  dot_pos<-get_dot_position(code)
  code_length=nchar(code)
  if(is.na(dot_pos)) {
    stop(paste("Provided code",code,"cannot have parents"));
  }
  if(immediate) {
    #retrieve immediate relations by querying only first batch of codes (e.g. given 'H32..' use 'H32%.')
    return(list(
      op="==",
      value=c(paste0(substring(code,1,dot_pos-2),paste0(rep(".",code_length-(dot_pos-2)),collapse="")))
    ))
  }

  #retrieve all relations by querying each possible code (e.g. given 'H32..' use c('H3...','H....')
  query<-c()
  for(i in seq(3,dot_pos)) {
    query<-c(query,paste0(substring(code,1,i-2),paste0(rep(".",code_length-(i-2)),collapse="")))
  }

  list(op="%in%", value=query)
}

#' Internal function which gets the position of first dot in code
#'
#' @param code Code to search
#'
#' @return An integer specifying the location of the first dot
get_dot_position<-function(code) {
  #obtain positions of dots in parent_code
  bits<-lapply(strsplit(code, ''), function(x) which(x == '.'))

  firstDot<-bits[[1]][1]
  firstDot[1]
}


#' Internal function used to get relations from a hierarchical dictionary such as READ v2 or ICD10
#'
#' @param dict Clinical dictionary object
#' @param code Code to use a basis for relationship extraction
#' @param immediate_relations T/F flag to return just immediate relations or all relations
#' @param children T/F flag to return children (T) or parents (F)
#'
extract_relations_from_hierarchy<-function(dict,code,immediate_relations=F,children=T) {
  if(!is_code_present(dict,code)) {
    stop(paste0("Code '",code,"' is not present"))
  }
  if(children) {
    params<-create_query_params_for_children_from_hierarchy(code,immediate_relations)
  }
  else {
    params<-create_query_params_for_parents_from_hierarchy(code,immediate_relations)
  }
  codes<-dplyr::tbl(dict$src,get_ctable_name(dict))
  if(cc_debug()) {
    print(paste("params",params))
  }
  rels<-params$value

  data<-eval(parse(text = paste("dplyr::filter(codes,",get_ctable_code_field(dict),params$op,"rels &",get_ctable_code_field(dict)," != code)")))

  data
}

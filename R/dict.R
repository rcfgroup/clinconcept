
#' Get dictionary-specific concept table name
#'
#' @param dict Clinical dictionary object
#'
#' @return table name
#' @export
get_ctable_name<-function(dict) {
  UseMethod("get_ctable_name");
}

#' Get dictionary-specific concept code field name
#'
#' @param dict Clinical dictionary object
#'
#' @return code field name
#' @export
get_ctable_code_field<-function(dict) {
  UseMethod("get_ctable_code_field");
}

#' Get dictionary-specific concept term field name
#'
#' @param dict Clinical dictionary object
#'
#' @return term field name
#' @export
get_ctable_term_field<-function(dict) {
  UseMethod("get_ctable_term_field");
}

#' Get dictionary-specific parent table name
#'
#' @param dict Clinical dictionary object
#'
#' @return table name
#' @export
get_ptable_name<-function(dict) {
  UseMethod("get_ptable_name");
}

#' Get dictionary-specific parent table code field name
#'
#' @param dict Clinical dictionary object
#'
#' @return code field name
#' @export
get_ptable_code_field<-function(dict) {
  UseMethod("get_ptable_code_field");
}
#' Get dictionary-specific parent table parent code field name
#'
#' @param dict Clinical dictionary object
#'
#' @return parent code field name
#' @export
get_ptable_parent_field<-function(dict) {
  UseMethod("get_ptable_parent_field");
}

#' Determine if a code is present in the supplied clinical dictionary
#'
#' @param dict Clinical dictionary object
#' @param code Clinical code to check
#'
#' @return TRUE if present, FALSE if not
#' @importFrom magrittr "%>%"
#' @importFrom rlang ".data"
#' @export
#'
#' @examples
#'\dontrun{
#' dict<-cc_from_file("/path/to/dictconfig")
#' h3_child_codes<-get_parent_codes(dict,"H31..",immediate_children=F)
#'}
is_code_present<-function(dict,code) {
  UseMethod("is_code_present")
}


is_code_present.clinconcept<-function(dict,code) {
  code_tbl <- dict$src %>% dplyr::tbl(get_ctable_name(dict));
  fct<-paste0("dplyr::filter(code_tbl,",get_ctable_code_field(dict)," == code)")
  code_tbl<-eval(parse(text=fct))
  codes<-code_tbl %>% dplyr::count() %>% dplyr::collect()
  codes$n>0
}

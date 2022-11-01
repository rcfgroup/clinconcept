#' Search clinical codes in the specified dictionary using dplyr filter options
#'
#' @param dict dictionary object (see cc_from_list/cc_from_file)
#' @param ... Query terms passed to dplyr::filter function
#' @param include_synonyms Flag to include synonyms in filtering
#' @param output Output type. tbl is the default. Can also return codes or terms
#' @return Depends on output parameter. Default is a dplyr::tbl which can undergo additional processing. Alternatives include a vector of terms or codes.
#' @export
#' @examples
#'\dontrun{
#' config_file<-paste0(system.file("clinconcept"),"/extdata/dictconfig.json")
#' config<-cc_from_file("NHSReadV3",config_file)
#'
#' #return all read codes containing 'asthma' and 'lung'
#'
#'  asthma_lung_codes<-search_concepts(config,term %like% "asthma" |
#'  term %like% "lung",include_synonyms=F)
#'}
search_concepts<-function(dict, ..., include_synonyms=F,output="tbl") {
  UseMethod("search_concepts")
}

search_concepts.clinconcept<-function(dict, ..., include_synonyms=F, output="tbl") {
  code_field<-get_ctable_code_field(dict);
  term_field<-get_ctable_term_field(dict);
  read_tbl<-dplyr::tbl(dict$src,get_ctable_name(dict))
  all_columns<-colnames(read_tbl)
  sel_columns<-c(code_field,term_field)
  sel_columns<-c(sel_columns,setdiff(all_columns,sel_columns))

  read_tbl<-dplyr::select(read_tbl,sel_columns)
  fcall<-match.call(expand.dots = T)
  search_concept_table(read_tbl,fcall,include_synonyms,output)
}

search_concept_table<-function(tbl,fcall,include_synonyms,output) {
  if(include_synonyms==F) {
    tbl<-dplyr::filter(tbl,synonym=='0')
  }
  fcall$.data<-tbl
  fcall[[1]] <- dplyr::filter
  fcall$dict<-NULL
  fcall$include_synonyms<-NULL
  fcall$output<-NULL

  concepts<-eval(fcall)
  if(output=="terms") {
    return(dplyr::collect(concepts)$term)
  }
  else if(output=="codes") {
    codes<-dplyr::collect(concepts)
    return(unique(codes[[1]]))
  }
  else if(output=="tbl") {
    return(concepts)
  }
  stop(paste("Output type",output,"not recognised"))
}

#' Enable SQLite database query case-sensitivity
#'
#' @param dict dictionary object (see cc_from_list/cc_from_file)
#' @export
#
enable_case_sensitivity<-function(dict) {
  if(!methods::is(dict$src, "SQLiteConnection")) {
    return();
  }
  DBI::dbExecute(dict$src,"pragma case_sensitive_like = yes")
}

#' Disable SQLite database query case-sensitivity
#'
#' @param dict dictionary object (see cc_from_list/cc_from_file)
#' @importFrom methods "is" "new"
#' @export
#
disable_case_sensitivity<-function(dict) {
  if(!methods::is(dict$src, "SQLiteConnection")) {
    return();
  }
  DBI::dbExecute(dict$src,"pragma case_sensitive_like = no")
}

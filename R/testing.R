setup_test_dict<-function(dict_type,force_create=F) {
  test_path = getwd()

  if(test_mysql()) {
    print("Testing mysql");
    dict<-cc_from_home(dict_type,"cc_db_config.json")
    dict$sql_path=system.file("sql",package="clinconcept")

    if(force_create) {
      build_concept_tables(dict,system.file("test_data",dict_type,package="clinconcept"))
    }
  }
  else {
    db_path<-paste0(test_path,"/test_",dict_type,".sqlite")

    dict<-cc_from_list(dict_type,list(type="sqlite",dbname=db_path))
    dict$sql_path=system.file("sql", package="clinconcept")

    if(!file.exists(db_path) | force_create) {
      build_concept_tables(dict,system.file("test_data",dict_type, package="clinconcept"))
    }
  }

  dict;
}
cleanup_test_dict<-function(dict) {
  cc_disconnect(dict)
  dict_type = class(dict)[1]

  if(cc_debug()==T) {
    print(paste0("DEBUG:Not deleting test database ",dict_type))
    return()
  }
  test_path = getwd()
  unlink(paste0(test_path,paste0("/test_",dict_type,".sqlite")))
}

expect_child_codes<-function(dict,code,exp_child_codes, immediate=F, current=F) {
  obs_child_codes<-get_child_codes(dict,code,immediate_children=immediate,current_only=current)
  if(cc_debug()) {
    print(paste("obs code",code,"current=",current,"immediate=",immediate,paste(sort(obs_child_codes),collapse=",")))
    print(paste("exp code",code,"current=",current,"immediate=",immediate,paste(sort(exp_child_codes),collapse=",")))
  }
  testthat::expect_equal(sort(obs_child_codes),sort(exp_child_codes))
}
expect_parent_codes<-function(dict,code,exp_parent_codes, immediate=F, current=F) {
  obs_parent_codes<-get_parent_codes(dict,code,immediate_parents=immediate,current_only=current)
  if(cc_debug()) {
    print(paste("obs code",code,"current=",current,"immediate=",immediate,paste(sort(obs_parent_codes),collapse=",")))
    print(paste("exp code",code,"current=",current,"immediate=",immediate,paste(sort(exp_parent_codes),collapse=",")))
  }
  testthat::expect_equal(sort(obs_parent_codes),sort(exp_parent_codes))
}
is_sqlite_available<-function() {
  options(cc_sqlite=TRUE)
  tryCatch(system2(c("sqlite3","--version"),stdout=TRUE,stderr=TRUE),
           error=function(err) { options(cc_sqlite=FALSE) }
  )
  getOption("cc_sqlite")
}
#' Utility function which switches on/off mysql testing
#'
#' @param value Boolean (T/F or TRUE/FALSE)
#' @export
#' @return TRUE if mysql testing is on
#' @examples
#'
#' #switch debugging on
#' test_mysql(TRUE)
#'
#' #log something if debugging is on
#' if(test_mysql()) {
#'   print("mysql testing is on")
#' }
#'
test_mysql<-function(value=NULL) {
  if(is.null(getOption("cc_test_mysql"))) {
    options(cc_test_mysql=F)
  }
  if(is.null(value)) {
    return(getOption("cc_test_mysql"))
  }
  options(cc_test_mysql=value)
}

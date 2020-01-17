#' Create a clinical dictionary object using a list of options. This is used by subsequent functions
#' to direct building and querying.
#'
#' The preferred approach is to use \code{\link{cc_from_file}} as this is more secure.
#' @param dict_type Clinical concept dictionary type. Currently supported are NHSReadV2, NHSReadV3, NHSICD10 and NHSSnomedCT
#' @param options List of options to use to create a clinical dictionary object
#'
#' Global options (* specifies required options)
#'   type - specifies the database management system being used. Currently supported are mysql and sqlite [default])
#'   dbname* - when mysql this specifies the database name; when sqlite specifies the location of the database file
#'
#  MySQL only options
#'   user - database user
#'   pass - database password
#'   host - database host (default localhost)
#'   port - database port (default 3306)
#'
#' @export
#' @seealso \code{\link{cc_from_file}}, \code{\link{cc_from_home}}
#' @examples
#' \dontrun{
#' dict<-cc_from_list(list(type="sqlite",dbname="/path/to/sqlitedb.sqlite"))
#'
#' #clinical dictionary object then passed to other functions
#' ...
#'}
#'
cc_from_list <- function(dict_type,options) {
  if(class(options)!="list") {
    stop("Options must be a list");
  }
  if(!"sql_path" %in% names(options)) {
    options$sql_path=system.file("sql",package="clinconcept")
  }
  class(options) <- c(dict_type,"clinconcept")
  if(!"dbname" %in% names(options)) {
    stop("dbname option must be provided")
  }
  if(!"port" %in% names(options)) {
    options$port = 3306;
  }

  if(!"host" %in% names(options)) {
    options$host = "localhost";
  }
  if(!"type" %in% names(options)) {
    options$type = "sqlite"
  }

  dbname = options$dbname
  user = options$user
  pass = options$pass
  host = options$host
  port = options$port
  type = options$type

  if(type=="mysql") {
    if(requireNamespace("RMySQL", quietly=T)) {
      options$src<-DBI::dbConnect(RMySQL::MySQL(),user=user,pass=pass,host=host,port=port,dbname=dbname)
    }
  }
  else {
    if(requireNamespace("RSQLite", quietly=T)) {
      options$src<-DBI::dbConnect(RSQLite::SQLite(),dbname=dbname,create=T)
    }
  }
  options
}
#' This function will read a JSON file containing options used to build a clinical dictionary object. This is used by subsequent functions
#' to direct building and querying. For security reasons, this function and \code{\link{cc_from_home}} are preferred to \code{\link{cc_from_list}}, as this prevents usernames and passwords being included in source code.
#' @param dict_type Clinical concept dictionary type. Currently supported are NHSReadV2, NHSReadV3, NHSICD10 and NHSSnomedCT
#' @param json_file Location of JSON file containing options to use to create a clinical dictionary object
#'
#' @export
#' @seealso \code{\link{cc_from_list}}, \code{\link{cc_from_home}}
#' @examples
#' \dontrun{
#' dict<-cc_from_file("NHSReadV3","/path/to/dictconfig.json")
#'
#' #clinical dictionary object then passed to other functions
#' ...
#'
#' # /path/to/dictconfig.json file
#'  {
#'    "type":"sqlite"
#'    "dbname":"/path/to/sqlitedb.sqlite"
#'  }
#'}
#'
cc_from_file<-function(dict_type, json_file) {
  if(!file.exists(json_file)) {
    stop(paste0("Dictionary config file '",json_file,"' does not exist"))
  }
  json_data<-jsonlite::fromJSON(json_file)
  if(length(names(json_data))==0) {
    stop(paste0("Dictionary config file '",json_file,"' contains no options"))
  }

  cc_from_list(dict_type,json_data)
}
#' This function will use a JSON file in the users home directory to build a clinical dictionary object. This is used by subsequent functions
#' to direct building and querying.
#' The location of the JSON file will depend on the operating system being used.
#' On Linux/OS X this will be /Users/<user>/. On newer versions of Windows it should be the same, although it may change.
#' To determine where the file will need to be placed in R you can execute: \code{Sys.getenv('USERPROFILE')}
#'
#' For security reasons, this function and \code{\link{cc_from_file}} are preferred to \code{\link{cc_from_list}}, as this prevents usernames and passwords being included in source code.
#'
#'
#' @param dict_type Clinical concept dictionary type. Currently supported are NHSReadV2, NHSReadV3, NHSICD10 and NHSSnomedCT
#' @param filename  Name of JSON file in user's home directory containing options to use to create a clinical dictionary object
#'
#' @export
#' @seealso \code{\link{cc_from_list}}, \code{\link{cc_from_file}}
#' @examples
#' \dontrun{
#' dict<-cc_from_home("NHSReadV3","dictconfig.ini")
#'
#' #clinical dictionary object then passed to other functions
#' ...
#'}
cc_from_home <- function(dict_type,filename) {
  home<-Sys.getenv('USERPROFILE')

  #deal with non-Windows system
  if(nchar(home)==0) {
    home<-home<-Sys.getenv('HOME')
  }
  filepath<-paste(home,filename,sep = "/")

  cc_from_file(dict_type,filepath)
}

#' Utility function to determine if clinical dictionary connection is available/connects
#'
#' @param dict Clinical dictionary object
#' @param output_error When TRUE will output error message if is not available
#'
#' @return TRUE if connection is available
#' @importFrom utils "capture.output"
#' @export
cc_is_available<-function(dict,output_error=F)
{
  avail <- tryCatch({
    dict$src
    test <-utils::capture.output(print(dict$src))
    return(T)
  }, error = function(e) {
    if(cc_debug()) {
      print(e)
    }
    return(F)
  })
  avail
}

#' Utility function to reconnect if clinical dictionary connection has timed out
#'
#' @param dict Clinical dictionary object
#' @export
cc_reconnect<-function(dict)
{
  classes<-class(dict)
  dict$src<-NULL
  class(dict)<-"list"
  options<-as.list(dict)
  cc_from_list(classes[1],options)
}

#' Utility function which switches on/off debugging messages and returns the current status of debugging
#'
#' @param value Boolean (T/F or TRUE/FALSE)
#' @export
#' @return TRUE if debugging is on
#' @examples
#' #switch debugging on
#' cc_debug(TRUE)
#'
#' #log something if debugging is on
#' if(cc_debug()) {
#'   print("debugging is on")
#' }
#'
cc_debug<-function(value=NULL) {
  if(is.null(getOption("rcc.debug"))) {
    options(rcc.debug=F)
  }
  if(is.null(value)) {
    return(getOption("rcc.debug"))
  }
  options(rcc.debug=value)
}

#' Utility function used to disconnect database connection
#'
#' @param dict Clinical dictionary object
#' @export
cc_disconnect<-function(dict) {
  DBI::dbDisconnect(dict$src)
}

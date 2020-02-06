#' Builds chosen dictionary concept tables in a database. Uses the supplied dictionary object to
#' determine how to do this. The concept_file_path specifies the location of the downloaded clinical code files.
#' <data-file-path> placeholders in the SQL file are replaced with the supplied concept_file_path.
#'
#' Internally the function executes specific SQL files before and after a dictionary/database specific function (if it exists).
#' The form of this function is build_concept_tables.db_type.dict_type. It is passed the dictionary object and the list of replacements.
#'
#' @param dict Dictionary object
#' @param concept_file_path Path to the downloaded concept files
#'
#' @export
build_concept_tables <- function(dict,concept_file_path) {
  #replacements are strings replaced in the SQL files with the list value
  #e.g. <data-file-path> will get replaced with the concept_file_path variable
  replacements = list("data-file-path"=concept_file_path)
  dict_class = class(dict)[1]
  db_class = dict$type
  pre_file<-paste0(dict$sql_path,"/",dict_class,"/",dict_class,"_",db_class,"_pre.sql")
  post_file<-paste0(dict$sql_path,"/",dict_class,"/",dict_class,"_",db_class,"_post.sql")
  if(file.exists(pre_file)) {
    execute_sql_statements(dict,load_sql_statements_from_file(pre_file,replacements))
  }

  #check first if a function exists with the name build_concept_tables.dict_type
  #e.g. build_concept_tables.NHSReadV3

  build_func_name<-as.character(paste0("build_concept_tables",".",dict_class))
  if(exists(build_func_name,mode="function")) {
    func<-call(build_func_name, dict,replacements)
    eval(func)
  }

  #then see if a specific DBMS function exists with the name build_concept_tables.db_type.dict_type
  #e.g. build_concept_tables.mysql.NHSReadV3
  build_func_name<-as.character(paste0("build_concept_tables",".",db_class,".",dict_class))
  if(exists(build_func_name,mode="function")) {
    func<-call(build_func_name, dict,replacements)
    eval(func)
  }

  if(file.exists(post_file)) {
    execute_sql_statements(dict,load_sql_statements_from_file(post_file,replacements))
  }
}
#' Utility function which reads SQL statements from a file and does basic placeholder replacement of
#' <placeholder> type strings in the statements.
#'
#' @param filename SQL file to load and process
#' @param replacements List of key-value replacements using the placeholder names as key.
#'
load_sql_statements_from_file <- function(filename,replacements) {
  if(cc_debug()) {
    print(paste("Load SQL file",filename))
  }
  conn = file(filename,"r")
  lines<-readLines(conn)
  close(conn)
  statements = list()
  statement_count=1
  statement = c()

  for(line in lines) {
    if(nchar(line)==0) {
      next;
    }

    statement<-append(statement,line)
    if(substring(line,nchar(line))==";") {
      statement_text<-paste(statement,collapse=" ")
      #substitute strings in the SQL statements with the replacements list value
      for(rep in names(replacements)) {
        rep_text<-replacements[[rep]]
        statement_text<-sub(paste0("<",rep,">",collapse=""),rep_text,statement_text,fixed=T)
      }
      statements[[statement_count]]<-statement_text
      statement_count=statement_count+1;
      statement = c()
    }
  }
  statements
}

#' Utility function which loops through a vector of SQL statements and executes them
#'
#' @param dict Dictionary object
#' @param statements Vector of SQL statements
#'
execute_sql_statements <-function(dict, statements) {
  for(statement in statements) {
    if(cc_debug()) {
      print(paste("SQL:",statement))
    }
    res<-DBI::dbSendQuery(dict$src,statement)
    DBI::dbClearResult(res)
  }

}
#' Wrapper function which simplifies reading a delimited file and writing it to a database table. Generally used with SQLite.
#'
#' @param src Database dplyr::src/connection
#' @param table_name Name of table in database
#' @param filename Filename of delimited file
#' @param col_names Column names passed directly to readr::read_delim
#' @param col_types Column types passed directly to DBI::dbWriteTable
#' @param delim Field delimiter to use to read file passed directly to readr::read_delim
#' @param file_col_types File column types passed directly to readr::read_delim
#' @param header Column header parameter passed directly to dbWriteTable
#' @param skip_rows Number of rows to skip (default = 0)
#'
write_table_from_file<-function(src,table_name,filename,col_names=TRUE,col_types=NULL,delim=",",file_col_types=NULL,header=F, skip_rows=0) {
  rows<-readr::read_delim(filename,delim=delim,
                          col_names=col_names,quote="",col_types=file_col_types, skip=skip_rows)
  DBI::dbWriteTable(src,name=table_name,value=rows, row.names=FALSE, field.types=col_types,header=header,overwrite=T)
}

#' Find file matching regular expression in directory. Used to retrieve code files which vary depending on version.
#'
#' @param file_path Path to search for file
#' @param match Regular expression to use
#'
#' @return File name
#'
#' @examples
#'\dontrun{
#' find_matching_file("/path/to/data","/description/")
#'}
find_matching_file<-function(file_path,match) {
  files_in_path=Sys.glob(paste0(file_path,"/*"));
  file<-grep(match,files_in_path,value=T)
  if(is.null(file)) {
    stop(paste0("File matching '",match,"' was not found in ",file_path))
  }
  if(!file.exists(file[1])) {
    stop(paste0("File matching '",match,"' was not found in ",file_path))
  }
  file[1]
}




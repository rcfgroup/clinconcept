% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.R
\name{cc_from_file}
\alias{cc_from_file}
\title{This function will read a JSON file containing options used to build a clinical dictionary object. This is used by subsequent functions
to direct building and querying. For security reasons, this function and \code{\link{cc_from_home}} are preferred to \code{\link{cc_from_list}}, as this prevents usernames and passwords being included in source code.}
\usage{
cc_from_file(dict_type, json_file)
}
\arguments{
\item{dict_type}{Clinical concept dictionary type. Currently supported are NHSReadV2, NHSReadV3, NHSICD10 and NHSSnomedCT}

\item{json_file}{Location of JSON file containing options to use to create a clinical dictionary object}
}
\description{
This function will read a JSON file containing options used to build a clinical dictionary object. This is used by subsequent functions
to direct building and querying. For security reasons, this function and \code{\link{cc_from_home}} are preferred to \code{\link{cc_from_list}}, as this prevents usernames and passwords being included in source code.
}
\examples{
\dontrun{
dict<-cc_from_file("NHSReadV3","/path/to/dictconfig.json")

#clinical dictionary object then passed to other functions
...

# /path/to/dictconfig.json file
 {
   "type":"sqlite"
   "dbname":"/path/to/sqlitedb.sqlite"
 }
}

}
\seealso{
\code{\link{cc_from_list}}, \code{\link{cc_from_home}}
}

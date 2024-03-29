% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dict.R
\name{is_code_present}
\alias{is_code_present}
\title{Determine if a code is present in the supplied clinical dictionary}
\usage{
is_code_present(dict, code)
}
\arguments{
\item{dict}{Clinical dictionary object}

\item{code}{Clinical code to check}
}
\value{
TRUE if present, FALSE if not
}
\description{
Determine if a code is present in the supplied clinical dictionary
}
\examples{
\dontrun{
config_file<-paste0(system.file("clinconcept"),"/extdata/dictconfig.json")
dict<-cc_from_file("NHSReadV3",config_file)
h3_child_codes<-get_parent_codes(dict,"H31..",immediate_children=F)
}
}

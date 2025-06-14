% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/salm.R
\name{read_salm}
\alias{read_salm}
\title{Read data from Jobs & Skills Australia's Small Area Labour Markets (SALM)}
\usage{
read_salm(tables = "all", geog_type = "LGA", path = tempdir())
}
\arguments{
\item{tables}{A vector of tables to read, or "all" to read all tables.
Possible options are \code{c("lf_persons", "unemp_persons", "unemp_rate", "all")}}

\item{geog_type}{A character specifying geography data type
Possible options are \code{c("LGA", "SA2")}}

\item{path}{Path to the directory where the file(s) should be saved}
}
\value{
A tibble with the SALM data, in 'long'/tidy format
}
\description{
Read data from Jobs & Skills Australia's Small Area Labour Markets (SALM)
}
\examples{
\dontrun{
# Get a single table
read_salm(tables = "lf_persons", geog_type = "LGA")

# Get multiple tables
read_salm(c("lf_persons", "unemp_persons"))

# Get all tables
read_salm("all")
}
}

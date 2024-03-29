% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reos.R
\name{read_reos}
\alias{read_reos}
\title{Read data from Jobs & Skills Australia's
Recruitment Experiences and Outlook Survey (REOS)}
\usage{
read_reos(tables = "all", file = tempfile(fileext = ".xlsx"))
}
\arguments{
\item{tables}{A vector of table numbers to read, or "all" to read all tables}

\item{file}{Path to the file where the data should be saved}
}
\value{
A tibble with the REOS data, in 'long'/tidy format
}
\description{
Read data from Jobs & Skills Australia's
Recruitment Experiences and Outlook Survey (REOS)
}
\examples{
\dontrun{
# Get a single table

read_reos(tables = "1.1")

# Get multiple tables

read_reos(tables = c("1.1", "1.2"))

# Get all tables

read_reos(tables = "all")
}
}

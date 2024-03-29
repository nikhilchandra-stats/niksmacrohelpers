% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aus_tender.R
\name{get_aus_tender_data}
\alias{get_aus_tender_data}
\title{Get Aus Tender data from the aus tender website.}
\usage{
get_aus_tender_data(start_year = 2016, wanted_data = "tender extensions")
}
\arguments{
\item{start_year}{(numeric; 2016) How far back do you want to go to, by default
it goes back to 2016.}

\item{wanted_data}{(character; tender extensions) What data from the website
are you looking for. Options are:

"tender extensions": This will extract the contract extension report from the
webstite.

"unspsc": This will extract uspsc data  from the website.

"consultancy": This will extract the consultancy report from the website.

"published contracts": Published contracts report.}
}
\value{
(tibble)
}
\description{
Get Aus Tender data from the aus tender website.
}
\examples{
\dontrun {

data <- get_aus_tender_data(start_year =2016, wanted_data = "tender extensions")

}
}

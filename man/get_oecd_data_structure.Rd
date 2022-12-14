% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oecd_sdmx_utilities.R
\name{get_oecd_data_structure}
\alias{get_oecd_data_structure}
\title{This function preserves extracts variable details for the chosen dataset.
For example if you give it QNA, it will return a list that provides
information on what each of the codes in the QNA dataset means. Eg; One of
the returned list elements will be subject that returns details for what each
of the coded values are
ie; CP00 = "National Consumer Price Index (CPI) by COICOP 1999 classification".}
\usage{
get_oecd_data_structure(dataset)
}
\arguments{
\item{dataset}{A string that is a dataset present in the OECD database. Use
OECD::get_datasets() to determine the possible options.}
}
\value{
(list) Returns a list that functions as a data dictionary for the
different variables in the data.
}
\description{
This function preserves extracts variable details for the chosen dataset.
For example if you give it QNA, it will return a list that provides
information on what each of the codes in the QNA dataset means. Eg; One of
the returned list elements will be subject that returns details for what each
of the coded values are
ie; CP00 = "National Consumer Price Index (CPI) by COICOP 1999 classification".
}

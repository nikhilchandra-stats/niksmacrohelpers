% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/daily_fx_data.R
\name{get_cpi_events_fx_calender}
\alias{get_cpi_events_fx_calender}
\title{Extracts CPI data from \code{get_macro_event_data()}}
\usage{
get_cpi_events_fx_calender(
  .data = get_macro_event_data(),
  column_affix = "cpi",
  country_symbol = NULL,
  wide = FALSE
)
}
\arguments{
\item{.data}{(tibble;get_macro_event_data()) Daily fx macro economic calendar
data}

\item{column_affix}{(character;"cpi") This is what is added to the variables
to signify that they are CPI measures.}

\item{country_symbol}{(character; NULL) filters for a currency symbol in the
data. Ie; "AUD", "CHF", etc.}

\item{wide}{(Boolean; FALSE) Pivots the data wider.}
}
\value{
(tibble)
}
\description{
Extracts CPI data from \code{get_macro_event_data()}
}

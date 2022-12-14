% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OECD_data_helpers.R
\name{get_oecd_data_DP_LIVE}
\alias{get_oecd_data_DP_LIVE}
\title{This function will look inside the 'DP_LIVE' data catalog and return a tibble
of data for the indicators chosen by the user.
Use \code{get_oecd_avlble_indctors_DPLIVE} to find out what is available. 'DP_LIVE'
contains various different economic, social and government indicators
reported by countries to the OECD. These are not harmonized statistics,
so when looking at things like GDP, CPI you will be viewing the data as
reported by the Country. These figures should align with reported numbers
on various FX calender websites or the relevant statistics agencies for the
relevant country.}
\usage{
get_oecd_data_DP_LIVE(
  indicators = c("EMP", "G_EMP", "LF", "G_UNEMP", "HUR", "LTUNEMP", "LFPR", "QGDP",
    "HHSAV"),
  countries = c("USA", "GBR"),
  start_time = 2020,
  end_time = 2022,
  remove_abbrev_cols = TRUE
)
}
\arguments{
\item{indicators}{(character vector) A vector of indicators requested by the
user. Example: c("EMP", "G_EMP", "LF") this would return various employment
indicators.}

\item{countries}{(character) Country abbreviations used by the function to
determine which countries will be extracted. Use \code{get_oecd_country_abbrevs}
to get the different abbreviations and associated country names.}

\item{start_time}{(numeric) The earliest year you want to go back to. The
function will attempt to go back as far as it can for the countries chosen.}

\item{end_time}{(numeric) Latest date you wish to extract data for.}

\item{remove_abbrev_cols}{(Boolean; TRUE) Will remove unnecessary abbreviation
columns if set to TRUE.}
}
\value{
(tibble)
}
\description{
This function will look inside the 'DP_LIVE' data catalog and return a tibble
of data for the indicators chosen by the user.
Use \code{get_oecd_avlble_indctors_DPLIVE} to find out what is available. 'DP_LIVE'
contains various different economic, social and government indicators
reported by countries to the OECD. These are not harmonized statistics,
so when looking at things like GDP, CPI you will be viewing the data as
reported by the Country. These figures should align with reported numbers
on various FX calender websites or the relevant statistics agencies for the
relevant country.
}
\examples{
\dontrun{

available_indicators <- get_oecd_avlble_indctors_DPLIVE()

GDP_based_indicators <- available_indicators \%>\%
   filter(str_detect(indicator_desc, "GDP|gdp")) \%>\%
   pull(INDICATOR)

dat <- get_oecd_data_DP_LIVE(
  indicators = GDP_based_indicators,
   countries = c("USA"),
   start_time = 2020,
   end_time = 2021
)

}
}

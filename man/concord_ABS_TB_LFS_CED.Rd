% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abs_CED_concord.R
\name{concord_ABS_TB_LFS_CED}
\alias{concord_ABS_TB_LFS_CED}
\title{get_ABS_TB_data_LFS}
\usage{
concord_ABS_TB_LFS_CED(
  path =
    "https://raw.githubusercontent.com/nikhilchandra-stats/macrodatasetsraw/refs/heads/master/data/SA1_LFS.csv",
  MB_pop = get_ABS_MB_pop(),
  POA_data = get_abs_region_allocation(geo_type = "POA"),
  CED_data = get_abs_region_allocation(geo_type = "CED"),
  MB_allocation_file = get_abs_region_allocation(),
  summarise_cols = c("Dwelling", "Person", "Employed, worked full-time",
    "Employed, worked part-time", "Employed, away from work",
    "Unemployed, looking for full-time work", "Unemployed, looking for part-time work",
    "Not in the labour force", "Not stated", "Not applicable")
)
}
\arguments{
\item{path}{(character) url for where the TB data is stored}

\item{MB_pop}{(tibble) Meshblock Population. By Default it will draw this from the ABS
website using an internal function in this package.}

\item{POA_data}{(tibble) POA Allocation Data. By Default it will draw this from the ABS
website using an internal function in this package.}

\item{CED_data}{(tibble) CED Allocation Data. By Default it will draw this from the ABS
website using an internal function in this package.}

\item{MB_allocation_file}{(tibble) ASGS Allocation data. By Default it will draw this from the ABS
website using an internal function in this package.}

\item{summarise_cols}{(character vector) This is the columns from the main TB data set you
want to summarise.}
}
\value{
(tibble) Returns TB data summarised by CED
}
\description{
This function concords ABS Table Builder data on Labour Force Status from
Table Builder to CED.The TB data is stored in my Github Repo.
}
\details{
TB = ABS Table Builder
}

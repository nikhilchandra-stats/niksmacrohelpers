#' Returns data scraped from Macro economic calender releases from dailyfx.com
#' The exact sources for each of the reported data can be found from the dailyfx
#' macroeconomic calenders. This includes lots of releases from GDP, to PMI
#' and other various sentiment indicators from various countries.
#'
#' @return (tibble) A tibble of economic data releases for various countries.
#' Includes
#' @export
#'
#' @examples \dontrun{
#'
#' macro_data <- get_macro_event_data()
#'
#' }
get_macro_event_data <- function(){

  dat <- readr::read_csv("https://raw.githubusercontent.com/nikhilchandra-stats/macrodatasetsraw/master/data/daily_fx_macro_data.csv")
  return(dat)

}

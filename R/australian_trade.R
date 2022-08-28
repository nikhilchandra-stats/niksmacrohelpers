#' This function obtains a local copy of DFAT Australian monthly trade data by
#' country and type of trade good.
#'
#' @return (tibble) Returns detailed monthly trade data for Australia.
#' @export
#'
#' @examples \dontrun{
#'
#' dat <- get_trade_DFAT_monthly()
#'
#' }
get_trade_DFAT_monthly <- function(){

  dat <- readr::read_csv("https://raw.githubusercontent.com/nikhilchandra-stats/macrodatasetsraw/master/data/DFAT_trade_monthly_detailed.csv")

  dat_cleaned <- dat %>%
    tidyr::pivot_longer(cols = -c(.data$Type, .data$Units, .data$Date),
                        names_to = "Country",
                        values_to = "Values")

  return(dat_cleaned)

}

#' This function obtains a local copy of DFAT Australian yearly trade data by
#' country and type of trade good.
#'
#' @return (tibble) Returns detailed yearly trade data for Australia.
#' @export
#'
#' @examples \dontrun{
#'
#' dat <- get_trade_DFAT_yearly()
#'
#' }
get_trade_DFAT_yearly <- function(){

  dat <- readr::read_csv("https://raw.githubusercontent.com/nikhilchandra-stats/macrodatasetsraw/master/data/DFAT_trade_yearly_detailed.csv")

  return(dat)

}

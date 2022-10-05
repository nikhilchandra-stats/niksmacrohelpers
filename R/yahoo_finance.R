#' This function will extract data from Yahoo finance based on the asset symbol
#' provided. The user specifies the start date and end date either as a character
#' or a date variable type. The function will return data for the asset between
#' the date ranges.
#'
#' @param asset (character; "GBPUSD") Character value for the symbol of the
#' asset the user wishes to exrtact.
#' @param start_date (Character or date; "2008-01-01")
#' @param end_date (Character or date; lubridate::today())
#'
#' @return (tibble) Returns data extracted from Yahoo finance.
#' @export
#'
#' @examples \dontrun{
#'
#' asset <- "AUDUSD"
#' dat <- get_yahoo_finance(asset = "AUDUSD")
#'
#' }
get_yahoo_finance <- function(asset = "GBPUSD",
                              start_date = "2008-01-01",
                              end_date = lubridate::today()){

start_date_num <- lubridate::as_datetime(start_date) %>% as.integer()
end_date_num <- lubridate::as_datetime(end_date) %>% as.integer()

url_base <- glue::glue("https://query1.finance.yahoo.com/v7/finance/download/{asset}=X?period1={start_date_num}&period2={end_date_num}&interval=1d&events=history&includeAdjustedClose=true")

dat1 <- readr::read_csv(url_base) %>%
  dplyr::mutate(asset = asset)

return(dat1)

}

#' This function will extract data from Market Watch  based on the asset symbol
#' provided. The user specifies the start date and end date either as a character
#' or a date variable type. The function will return data for the asset between
#' the date ranges.
#'
#' @param asset (character; "GBPUSD") Character value for the symbol of the
#' asset the user wishes to exrtact.
#' @param start_date (Character or date; "2008-01-01")
#' @param end_date (Character or date; lubridate::today())
#'
#' @return (tibble) Returns data extracted from market watch.
#' @export
#'
#' @examples \dontrun{
#'
#' asset <- "AUDUSD"
#' dat <- get_market_watch(asset = "AUDUSD")
#'
#' }
get_market_watch <- function(asset = "tmbmkgb-10y",
                              start_date = lubridate::today() - lubridate::years(1),
                              end_date = lubridate::today()){

  asset <- tolower(asset)
  start_date_num <- lubridate::as_date(start_date) %>% format("%m/%d/%Y")
  end_date_num <- lubridate::as_date(end_date) %>% format("%m/%d/%Y")

  url_base <- glue::glue("https://www.marketwatch.com/investing/bond/{asset}/downloaddatapartial?startdate={start_date_num}%2000:00:00&enddate={end_date_num}%2000:00:00&daterange=d30&frequency=p1d&csvdownload=true&downloadpartial=false&newdates=false&countrycode=bx")
  dat1 <- readr::read_csv(url_base) %>%
    dplyr::mutate(asset = asset)

  return(dat1)

}


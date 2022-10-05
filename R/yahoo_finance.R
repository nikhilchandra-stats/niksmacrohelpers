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

dat1 <- readr::read_csv(url_base)

return(dat1)

}

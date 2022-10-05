help_obr_public_finances <- function(month_lag = 2){

  current_month <- lubridate::month(lubridate::today() - lubridate::dmonths(month_lag), label = TRUE, abbr = FALSE) %>%
    as.character() %>%
    tolower()

  current_year <- lubridate::year(lubridate::today() - lubridate::dmonths(month_lag))

  url <-  glue::glue("https://obr.uk/download/public-finances-databank-{current_month}-{current_year}/")

  httr::GET(url = url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

  dat <- readxl::read_excel(sheet = 6, path = tf, skip = 5 )

  dat2 <- dat %>%
    tidyr::separate(col = .data$Years, sep = "-", into = c("start_period", "end_period")) %>%
    dplyr::filter(!is.na(.data$`Total receipts\r\n(PSCR)`)) %>%
    dplyr::mutate(
      end_period = paste0(stringr::str_extract(.data$start_period, "^.{2}"), .data$end_period)
    ) %>%
    dplyr::mutate(
      end_period = glue::glue("{.data$end_period}-06-30"),
      start_period = glue::glue("{.data$start_period}-06-30")
    ) %>%
    dplyr::mutate(financial_year = lubridate::year(.data$end_period))

  return(dat2)

}


#' This function will go to the OBR website and attempt to the the latest data kept
#' in the public finances since 1900 sheet. These values are expressed as a % of
#' GDP.
#' OBR: Office of budget responsibility, a publically funded organisation of
#' the UK government.
#'
#' @return (tibble) Returns a tibble containing Total Receipts as % of GDP,
#' Total Spending as % of GDP, Public sector net borrowing, Public sector net debt
#' @export
#'
#' @examples \dontrun{
#' dat <- get_obr_public_finances()
#' }
get_obr_public_finances <- function(){

  safe_help_obr_public_finances <- purrr::safely(.f = help_obr_public_finances, otherwise = NULL)

  dat <- suppressWarnings(safe_help_obr_public_finances(month_lag = 1))

  if(is.null(dat$result)) {

    dat <- suppressWarnings(safe_help_obr_public_finances(month_lag = 2))

  }

  return(dat$result)

}

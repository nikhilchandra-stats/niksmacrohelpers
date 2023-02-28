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

#' Extracts confidence data from `get_macro_event_data()`, this includes things
#' like Australian Westpac Confidence, Australian AIG indexes
#'
#' @param .data (tibble;get_macro_event_data()) Daily fx macro economic calendar
#' data
#' @param column_affix (character;"conf") This is what is added to the variables
#' to signify that they are confidence measures.
#' @param country_symbol (character; NULL) filters for a currency symbol in the
#' data. Ie; "AUD", "CHF", etc.
#' @param wide (Boolean; FALSE) Pivots the data wider.
#'
#' @return (tibble)
#' @export
#'
#' @examples
get_confidence_events_fx_calender <- function(.data = get_macro_event_data(),
                                              column_affix = "conf",
                                              country_symbol = NULL,
                                              wide = FALSE){

  months_abbrev <- months_of_year(abbrev = T) %>%
    paste(collapse = "|") %>% as.character()

  conf_only <- .data %>%
    dplyr::select(time,date_time,date,symbol,event,actual) %>%
    dplyr::filter(stringr::str_detect(event, "confidence|Confidence|PMI|confi|Confi|Economic Sentiment Indicator|Sentiment|sentiment|Ivey Purchasing Managers Index|Raw Material Price Index|SECO Consumer|ZEW Survey|Purchasing Managers|UBS Consumption Indicator|KOF|Eco Watchers Survey|Service|Corporate Service Price|Leading Economic Index")) %>%
    dplyr::filter(!stringr::str_detect(event, "Trade|trade")) %>%
    dplyr::mutate(event = stringr::str_remove(event, pattern = months_abbrev)) %>%
    dplyr::mutate(event = stringr::str_remove_all(.data$event, pattern = "Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec"  )) %>%
    dplyr::mutate(event = stringr::str_remove_all(.data$event, pattern = "\\( [0-9]+\\)|\\( [0-9]+ \\)|\\([0-9]+\\)"  )) %>%
    dplyr::mutate(
      event =
        dplyr::case_when(
          stringr::str_detect(event, "Q1|Q2|Q3|Q4") ~ stringr::str_remove(event, "Q1|Q2|Q3|Q4"),
          # str_detect(event, "Inflation") ~ str_remove(event, "Q1|Q2|Q3|Q4"),
          TRUE ~ event
        )
    ) %>%
    dplyr::group_by(symbol, event, time,date_time,date) %>%
    dplyr::summarise(actual = min(actual, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(event = paste0(event, "_", symbol, "_", column_affix))

    if(wide) {


      conf_only <- conf_only %>%
        tidyr::pivot_wider(names_from = event, values_from = actual) %>%
        dplyr::group_by(symbol) %>%
        dplyr::arrange(date_time, .by_group = T) %>%
        dplyr::group_by(symbol) %>%
        tidyr::fill(tidyselect::vars_select_helpers$everything() , .direction = "down") %>%
        dplyr::rename(event_time = time)

    }


  if(!is.null(country_symbol)) {

    conf_only <- conf_only %>%
      dplyr::filter(symbol == country_symbol)

  }

  return(conf_only)

}


#' Extracts CPI data from `get_macro_event_data()`
#'
#' @param .data (tibble;get_macro_event_data()) Daily fx macro economic calendar
#' data
#' @param column_affix (character;"cpi") This is what is added to the variables
#' to signify that they are CPI measures.
#' @param country_symbol (character; NULL) filters for a currency symbol in the
#' data. Ie; "AUD", "CHF", etc.
#' @param wide (Boolean; FALSE) Pivots the data wider.
#'
#' @return (tibble)
#' @export
#'
#' @examples
get_cpi_events_fx_calender <- function(.data = get_macro_event_data(),
                                       column_affix = "cpi",
                                       country_symbol = NULL,
                                       wide = FALSE){

  months_abbrev <- months_of_year(abbrev = T)%>%
    paste(collapse = "|") %>% as.character()

  cpi_only <- .data %>%
    dplyr::select(time,date_time,date,symbol,event,actual) %>%
    dplyr::filter(stringr::str_detect(event, "inflation|Inflation|cpi|CPI|consumer price index|consumer|Consumer|Consumer Price Index")) %>%
    dplyr::filter(!stringr::str_detect(event, "(?i)(confidence)|(?i)(survey)")) %>%
    dplyr::mutate(event = stringr::str_remove(event, pattern = months_abbrev)) %>%
    dplyr::mutate(event = stringr::str_remove_all(.data$event, pattern = "Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec"  )) %>%
    dplyr::mutate(event = stringr::str_remove_all(.data$event, pattern = "\\( [0-9]+\\)|\\( [0-9]+ \\)|\\([0-9]+\\)"  )) %>%
    dplyr::mutate(
      event =
        dplyr::case_when(
          stringr::str_detect(event, "Q1|Q2|Q3|Q4") ~ stringr::str_remove(event, "Q1|Q2|Q3|Q4"),
          # str_detect(event, "Inflation") ~ str_remove(event, "Q1|Q2|Q3|Q4"),
          TRUE ~ event
        )
    ) %>%
    dplyr::group_by(symbol, event, time,date_time,date) %>%
    dplyr::summarise(actual = min(actual, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(event = paste0(event, "_", symbol, "_", column_affix))

    if(wide) {

      cpi_only <- cpi_only %>%
        tidyr::pivot_wider(names_from = event, values_from = actual) %>%
        dplyr::group_by(symbol) %>%
        dplyr::arrange(date_time, .by_group = T) %>%
        dplyr::group_by(symbol) %>%
        tidyr::fill(tidyselect::vars_select_helpers$everything() , .direction = "down") %>%
        dplyr::rename(event_time = time)

    }

  return(cpi_only)

}

#' Get Interest Rate and finance data data from `get_macro_event_data()`
#'
#' @param .data (tibble;get_macro_event_data()) Daily fx macro economic calendar
#' data
#' @param column_affix (character;"fin_int") This is what is added to the variables
#' to signify that they are Interest Rate and finance data measures.
#' @param country_symbol (character; NULL) filters for a currency symbol in the
#' data. Ie; "AUD", "CHF", etc.
#' @param wide (Boolean; FALSE) Pivots the data wider.
#'
#' @return (tibble)
#' @export
#'
#' @examples
get_finance_events_fx_calender <- function(.data = get_macro_event_data(),
                                           column_affix = "fin_int",
                                           country_symbol = NULL,
                                           wide = FALSE){

  months_abbrev <- months_of_year(abbrev = T) %>%
    purrr::map(
      ~ glue::glue("{.x}") %>%
        as.character()
    ) %>%
    unlist()


  interest_only <- .data %>%
    dplyr::select(time,date_time,date,symbol,event,actual) %>%
    dplyr::filter(stringr::str_detect(event, "Interest|interest|Finance|finance|Money|Monetary")) %>%
    dplyr::filter(!stringr::str_detect(event, "Trade|trade")) %>%
    dplyr::mutate(event = stringr::str_remove_all(.data$event, pattern = "Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec"  )) %>%
    dplyr::mutate(event = stringr::str_remove_all(.data$event, pattern = "\\( [0-9]+\\)|\\( [0-9]+ \\)|\\([0-9]+\\)"  )) %>%
    dplyr::mutate(
      event =
        dplyr::case_when(
          stringr::str_detect(event, "Q1|Q2|Q3|Q4") ~ str_remove(event, "Q1|Q2|Q3|Q4"),
          # str_detect(event, "Inflation") ~ str_remove(event, "Q1|Q2|Q3|Q4"),
          TRUE ~ event
        )
    ) %>%
    dplyr::group_by(symbol, event, time,date_time,date) %>%
    dplyr::summarise(actual = min(actual, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(event = paste0(event, "_", symbol, "_", column_affix))


  if(wide) {
    interest_only <- interest_only %>%
      tidyr::pivot_wider(names_from = event, values_from = actual) %>%
      dplyr::group_by(symbol) %>%
      dplyr::arrange(date_time, .by_group = T) %>%
      dplyr::group_by(symbol) %>%
      tidyr::fill(tidyselect::vars_select_helpers$everything() , .direction = "down") %>%
      dplyr::rename(event_time = time)
  }


  return(interest_only)

}


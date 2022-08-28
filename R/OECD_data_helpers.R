#' This function will return a tibble containg country and names and abbrevations
#' used in the OECD database. The other OECD get functions require country
#' abbreviations to be provided as an input.
#'
#' @return (tibble) A tibble containing all country abbreviations and names.
#' @export
#'
#' @examples \dontrun{
#'
#' country_abbrevs <- get_oecd_country_abbrevs() %>%
#'  pull(.data$id)
#'
#' GDP_dat <-
#'   get_oecd_gdp(
#'     variables = "QNA",
#'     countries = country_abbrevs,
#'     start_time = 2006,
#'     end_time = 2022,
#'     remove_abbrev_cols = TRUE
#'   )
#'
#' }
get_oecd_country_abbrevs <- function(){

  dstruc <- OECD::get_data_structure("QNA")

  country_tibble <- dstruc$LOCATION

  return(country_tibble)

}

#' This function will use the package `OECD` to extract GDP data for select
#' countries. This returns a tibble of various reported GDP data.
#'
#' @param variables (character; "QNA") This variables which database table the
#' function will extract data from. The default is "QNA" which is the quarterly
#' national account table.
#' @param countries (character) Country abbreviations used by the function to
#' determine which countries will be extracted. Use `get_oecd_country_abbrevs`
#' to get the different abbreviations and associated country names.
#' @param start_time (numeric) The earliest year you want to go back to. The
#' function will attempt to go back as far as it can for the countries chosen.
#' @param end_time (numeric) Latest date you wish to extract data for.
#' @param remove_abbrev_cols (Boolean; TRUE) Will remove unncessary abbreviation
#' columns if set to TRUE.
#'
#' @return (tibble) Tibble containing the GDP data you wanted extracted from the
#' specified table.
#' @export
#'
#' @examples \dontrun{
#'
#' country_abbrevs <- get_oecd_country_abbrevs() %>%
#'  pull(.data$id)
#'
#' GDP_dat <-
#'   get_oecd_gdp(
#'     variables = "QNA",
#'     countries = country_abbrevs,
#'     start_time = 2006,
#'     end_time = 2022,
#'     remove_abbrev_cols = TRUE
#'   )
#'
#' }
get_oecd_gdp_live <- function(
  variables = "QNA",
  countries = c("CHN", "USA", "GBR", "OECD", "G-7",
                "EU27_2020", "JPN", "ITA", "AUS", "CAN", "FRA", "DEU", "NZL", "NLD", "KOR", "IDN", "CHE"),
  start_time = 2006,
  end_time = 2022,
  remove_abbrev_cols = TRUE
){

  dstruc <- OECD::get_data_structure(variables)

  country_list <- dstruc$LOCATION

  variable_details <- dstruc$SUBJECT %>%
    dplyr::rename(SUBJECT = .data$id,
                  var_description = .data$label)

  unit_details <- dstruc$UNIT %>%
    dplyr::rename(UNIT = .data$id,
                  unit_desc = .data$label)

  measure_details <- dstruc$MEASURE %>%
    dplyr::rename(MEASURE = .data$id,
                  measure_name = .data$label)

  full_country <- dstruc$LOCATION %>%
    dplyr::rename(LOCATION = .data$id,
                  country = .data$label)

  time_info <- dstruc$TIME_FORMAT %>%
    dplyr::rename(TIME_FORMAT = .data$id,
                  time_details = .data$label)

  GDP_dat <- OECD::get_dataset(dataset = variables,
                               filter = list(countries),
                               start_time = start_time,
                               endtime = end_time)


  GDP_dat_filter <- GDP_dat %>%
    dplyr::mutate(year = stringr::str_extract(.data$obsTime,"[0-9][0-9][0-9][0-9]")) %>%
    dplyr::mutate(quarter_date =
                    dplyr::case_when(
                      stringr::str_detect(.data$obsTime,"Q1") ~ glue::glue("{year}-03-01"),
                      stringr::str_detect(.data$obsTime,"Q2") ~ glue::glue("{year}-06-01"),
                      stringr::str_detect(.data$obsTime,"Q3") ~ glue::glue("{year}-09-01"),
                      stringr::str_detect(.data$obsTime,"Q4") ~ glue::glue("{year}-12-01")
                    )
    ) %>%
    dplyr::mutate(
      date = lubridate::as_date(.data$obsTime)
    ) %>%
    dplyr::left_join(
      variable_details, by = "SUBJECT"
    ) %>%
    dplyr::left_join(
      unit_details, by = "UNIT"
    ) %>%
    dplyr::left_join(
      measure_details, by = "MEASURE"
    ) %>%
    dplyr::left_join(
      full_country, by = "LOCATION"
    ) %>%
    dplyr::left_join(
      time_info, by = "TIME_FORMAT"
    ) %>%
    dplyr::mutate(
      date = ifelse(!is.na(.data$quarter_date) & is.na(.data$date),
                    .data$quarter_date, .data$date)
    )

  if(remove_abbrev_cols){
    GDP_dat_filter <- GDP_dat_filter %>%
      dplyr::select(-.data$MEASURE, -.data$UNIT,
                    -.data$SUBJECT, -.data$obsTime,
                    -.data$OBS_STATUS)
  }

  return(GDP_dat_filter)

}

#' This function will use the package `OECD` to extract CPI data for select
#' countries. This returns a tibble of various reported CPI data.
#'
#' @param variables (character; "PRICES_CPI") This variables which database table the
#' function will extract data from. The default is "PRICES_CPI" which is the
#' CPI data table.
#' @param countries (character) Country abbreviations used by the function to
#' determine which countries will be extracted. Use `get_oecd_country_abbrevs`
#' to get the different abbreviations and associated country names.
#' @param start_time (numeric) The earliest year you want to go back to. The
#' function will attempt to go back as far as it can for the countries chosen.
#' @param end_time (numeric) Latest date you wish to extract data for.
#' @param remove_abbrev_cols (Boolean; TRUE) Will remove unnecessary abbreviation
#' columns if set to TRUE.
#'
#' @return (tibble) Tibble containing the CPI data you wanted extracted from the
#' specified table.
#' @export
#'
#' @examples \dontrun{
#'
#' country_abbrevs <- get_oecd_country_abbrevs() %>%
#'  pull(.data$id)
#'
#' GDP_dat <-
#'   get_oecd_CPI(
#'     variables = "PRICES_CPI",
#'     countries = country_abbrevs,
#'     start_time = 2006,
#'     end_time = 2022,
#'     remove_abbrev_cols = TRUE
#'   )
#'
#' }
get_oecd_CPI_live <- function( variables = "PRICES_CPI",
                          countries = c("CHN", "USA", "GBR", "OECD", "G-7",
                                        "EU27_2020", "JPN", "ITA", "AUS", "CAN", "FRA", "DEU", "NZL", "NLD", "KOR", "IDN", "CHE"),
                          start_time = 2006,
                          end_time = 2022,
                          remove_abbrev_cols = TRUE
){

  dstruc <- OECD::get_data_structure(variables)

  country_list <- dstruc$LOCATION

  variable_details <- dstruc$SUBJECT %>%
    dplyr::rename(SUBJECT = .data$id,
                  var_description = .data$label)

  unit_details <- dstruc$UNIT %>%
    dplyr::rename(UNIT = .data$id,
                  unit_desc = .data$label)

  measure_details <- dstruc$MEASURE %>%
    dplyr::rename(MEASURE = .data$id,
                  measure_name = .data$label)

  full_country <- dstruc$LOCATION %>%
    dplyr::rename(LOCATION = .data$id,
                  country = .data$label)

  time_info <- dstruc$TIME_FORMAT %>%
    dplyr::rename(TIME_FORMAT = .data$id,
                  time_details = .data$label)

  CPI_dat <- OECD::get_dataset(dataset = variables,
                               filter = list(countries),
                               start_time = start_time,
                               endtime = end_time)

  cpi_filter <- CPI_dat %>%
    dplyr::mutate(year = stringr::str_extract(.data$obsTime,"[0-9][0-9][0-9][0-9]")) %>%
    dplyr::mutate(quarter_date =
                    dplyr::case_when(
                      stringr::str_detect(.data$obsTime,"Q1") ~ glue::glue("{year}-03-01"),
                      stringr::str_detect(.data$obsTime,"Q2") ~ glue::glue("{year}-06-01"),
                      stringr::str_detect(.data$obsTime,"Q3") ~ glue::glue("{year}-09-01"),
                      stringr::str_detect(.data$obsTime,"Q4") ~ glue::glue("{year}-12-01")
                    )
    ) %>%
    dplyr::mutate(
      date = lubridate::as_date(.data$obsTime)
    ) %>%
    dplyr::left_join(
      variable_details, by = "SUBJECT"
    ) %>%
    dplyr::left_join(
      unit_details, by = "UNIT"
    ) %>%
    dplyr::left_join(
      measure_details, by = "MEASURE"
    ) %>%
    dplyr::left_join(
      full_country, by = "LOCATION"
    ) %>%
    dplyr::left_join(
      time_info, by = "TIME_FORMAT"
    ) %>%
    dplyr::mutate(
      date = ifelse(!is.na(.data$quarter_date) & is.na(.data$date),
                    .data$quarter_date, .data$date)
    )

  if(remove_abbrev_cols){
    cpi_filter <- cpi_filter %>%
      dplyr::select(-.data$MEASURE, -.data$UNIT,
                    -.data$SUBJECT, -.data$obsTime)
  }

  return(cpi_filter)

}

#' This function will return a tibble containing country and names and
#' abbreviations used in the OECD database. The other OECD get functions
#' require country abbreviations to be provided as an input.
#'
#' This function draws in a local copy of the data set for speed. Update the
#' function for updated local copies of the data.
#'
#' @return (tibble) A tibble containing all country abbreviations and names.
#' @export
#'
#' @examples \dontrun{
#'
#' dat <- get_oecd_GDP_local()
#'
#' }
get_oecd_GDP_local <- function(){

  dat <- readr::read_csv("https://raw.githubusercontent.com/nikhilchandra-stats/macrodatasetsraw/master/data/OECD_GDP_data.csv")
  return(dat)

}

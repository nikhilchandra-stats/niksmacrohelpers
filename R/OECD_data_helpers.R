#' This function will return a tibble containing country and names and abbreviations
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

  dstruc <- suppressMessages(get_dstrucs("QNA"))

  country_tibble <- dstruc$LOCATION

  return(country_tibble)

}

#' This function will use the package `OECD` to extract GDP data for select
#' countries. This returns a tibble of all detailed GDP data which has been
#' harmonized the OECD for allow for country to country comparisons.
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

  dstruc <- suppressMessages(get_dstrucs(variables))

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
                               endtime = end_time) %>%
    oecd_rename_obsTime()


  GDP_dat_filter <- GDP_dat %>%
    dplyr::mutate(year = stringr::str_extract(.data$Time,"[0-9][0-9][0-9][0-9]")) %>%
    dplyr::mutate(quarter_date =
                    dplyr::case_when(
                      stringr::str_detect(.data$Time,"Q1") ~ glue::glue("{year}-03-01"),
                      stringr::str_detect(.data$Time,"Q2") ~ glue::glue("{year}-06-01"),
                      stringr::str_detect(.data$Time,"Q3") ~ glue::glue("{year}-09-01"),
                      stringr::str_detect(.data$Time,"Q4") ~ glue::glue("{year}-12-01")
                    )
    ) %>%
    dplyr::mutate(
      date = lubridate::as_date(.data$Time)
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
                    -.data$SUBJECT, -.data$Time,
                    -.data$OBS_STATUS)
  }

  return(GDP_dat_filter)

}

#' This function will use the package `OECD` to extract CPI data for select
#' countries. This returns a tibble of detailed CPI data which has been
#' harmonized by the OECD to enable like for like comparisons.
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

  dstruc <- suppressMessages(get_dstrucs(variables))

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
                               endtime = end_time) %>%
    oecd_rename_obsTime()

  cpi_filter <- CPI_dat %>%
    dplyr::mutate(year = stringr::str_extract(.data$Time,"[0-9][0-9][0-9][0-9]")) %>%
    dplyr::mutate(quarter_date =
                    dplyr::case_when(
                      stringr::str_detect(.data$Time,"Q1") ~ glue::glue("{year}-03-01"),
                      stringr::str_detect(.data$Time,"Q2") ~ glue::glue("{year}-06-01"),
                      stringr::str_detect(.data$Time,"Q3") ~ glue::glue("{year}-09-01"),
                      stringr::str_detect(.data$Time,"Q4") ~ glue::glue("{year}-12-01")
                    )
    ) %>%
    dplyr::mutate(
      date = lubridate::as_date(.data$Time)
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
                    -.data$SUBJECT, -.data$Time)
  }

  return(cpi_filter)

}

#' This function will return a tibble containing country and names and
#' abbreviations used in the OECD database. The other OECD get functions
#' require country abbreviations to be provided as an input.
#'
#' This function draws in a local copy of the data set for speed.
#'
#' @return (tibble) A tibble containing all country abbreviations and names.
#' @export
#'
#' @examples \dontrun{
#'
#' dat <- get_oecd_GDP_local()
#'
#' }
get_oecd_GDP_local <- function(

){

  dat <- readr::read_csv("https://raw.githubusercontent.com/nikhilchandra-stats/macrodatasetsraw/master/data/OECD_GDP_data.csv")
  return(dat)

}


#' This function extracts relevant Employment data from the OECD using
#' "DP_LIVE" table. This function will return the following:
#'
#' G_EMP: Employment
#' EMP: Employment Rate
#' LF: Labour force
#' G_UNEMP: Unemployment
#' HUR: Unemployment rate
#' LTUNEMP: Long-term unemployment rate
#'
#' @param variables (character; "DP_LIVE") This variables which database table the
#' function will extract data from. The default is "DP_LIVE" which is the
#' the data table where labour force data is kept.
#' @param countries (character) Country abbreviations used by the function to
#' determine which countries will be extracted. Use `get_oecd_country_abbrevs`
#' to get the different abbreviations and associated country names.
#' @param start_time (numeric) The earliest year you want to go back to. The
#' function will attempt to go back as far as it can for the countries chosen.
#' @param end_time (numeric) Latest date you wish to extract data for.
#' @param remove_abbrev_cols (Boolean; TRUE) Will remove unnecessary abbreviation
#' columns if set to TRUE.
#'
#' @return (tibble)
#' @export
#'
#' @examples \dontrun{
#'
#' dat <- get_oecd_labour_force_live(
#' countries = c("USA"),
#' start_time = 2020,
#' end_time = 2022
#' )
#'
#' }
get_oecd_labour_force_live <- function(
  variables = "DP_LIVE",
  countries = c("CHN", "USA", "GBR", "OECD", "G-7",
                "EU27_2020", "JPN", "ITA", "AUS", "CAN", "FRA", "DEU", "NZL", "NLD", "KOR", "IDN", "CHE"),
  start_time = 2020,
  end_time = 2022,
  remove_abbrev_cols = TRUE
){

  dstruc <- suppressMessages(get_dstrucs(variables))

  country_list <- dstruc$LOCATION

  measure_details <- dstruc$MEASURE %>%
    dplyr::rename(MEASURE = .data$id,
                  measure_desc = .data$label)

  series_details <- dstruc$SUBJECT %>%
    dplyr::rename(SUBJECT = .data$id,
                  subject_desc = .data$label)

  indicator_details <- dstruc$INDICATOR %>%
    dplyr::rename(INDICATOR = .data$id,
                  indicator_desc = .data$label)

  full_country <- dstruc$LOCATION %>%
    dplyr::rename(LOCATION = .data$id,
                  country = .data$label)

  time_info <- dstruc$TIME_FORMAT %>%
    dplyr::rename(TIME_FORMAT = .data$id,
                  time_details = .data$label)

  frequency_info <- dstruc$FREQUENCY %>%
    dplyr::rename(FREQUENCY = .data$id,
                  frequency = .data$label)

  LFS_dat <- OECD::get_dataset(dataset = variables,
                               filter = list(countries,
                                             c("EMP", "G_EMP", "LF",
                                               "G_UNEMP", "HUR", "LTUNEMP",
                                               "LFPR")),
                               start_time = start_time,
                               endtime = end_time) %>%
    oecd_rename_obsTime()

  LFS_filter <- LFS_dat %>%
    dplyr::mutate(year = stringr::str_extract(.data$Time,"[0-9][0-9][0-9][0-9]")) %>%
    dplyr::mutate(quarter_date =
                    dplyr::case_when(
                      stringr::str_detect(.data$Time,"Q1") ~ glue::glue("{year}-03-01"),
                      stringr::str_detect(.data$Time,"Q2") ~ glue::glue("{year}-06-01"),
                      stringr::str_detect(.data$Time,"Q3") ~ glue::glue("{year}-09-01"),
                      stringr::str_detect(.data$Time,"Q4") ~ glue::glue("{year}-12-01")
                    )
    ) %>%
    dplyr::mutate(
      date = lubridate::as_date(.data$Time)
    ) %>%
    dplyr::left_join(
      full_country, by = "LOCATION"
    ) %>%
    dplyr::left_join(
      time_info, by = "TIME_FORMAT"
    )  %>%
    dplyr::left_join(
      series_details, by = "SUBJECT"
    )  %>%
    dplyr::left_join(
      indicator_details, by = "INDICATOR"
    ) %>%
    dplyr::left_join(
      measure_details, by = "MEASURE"
    ) %>%
    dplyr::left_join(
      frequency_info, by = "FREQUENCY"
    ) %>%
    dplyr::mutate(
      date = ifelse(!is.na(.data$quarter_date) & is.na(.data$date),
                    .data$quarter_date, .data$date)
    )

  if(remove_abbrev_cols){
    LFS_filter <- LFS_filter %>%
      dplyr::select(-.data$MEASURE, -.data$TIME_FORMAT, -.data$INDICATOR,
                    -.data$SUBJECT, -.data$Time,
                    -.data$FREQUENCY)
  }

  return(LFS_filter)



}

#' This function will look inside the 'DP_LIVE' data catalog and return a tibble
#' of indicators that are available inside of the OECD database. 'DP_LIVE'
#' contains various different economic, social and government indicators
#' reported by countries to the OECD. These are not harmonized statistics,
#' so when looking at things like GDP, CPI you will be viewing the data as
#' reported by the Country. These figures should align with reported numbers
#' on various FX calender websites or the relevant statistics agencies for the
#' relevant country.
#'
#' @return (tibble)
#' @export
#'
#' @examples \dontrun{
#'
#' available_indicators <- get_oecd_avlble_indctors_DPLIVE()
#'
#' GDP_based_indicators <- available_indicators %>%
#'    filter(str_detect(indicator_desc, "GDP|gdp")) %>%
#'    pull(INDICATOR)
#'
#' dat <- get_oecd_data_DP_LIVE(
#'   indicators = GDP_based_indicators,
#'    countries = c("USA"),
#'    start_time = 2020,
#'    end_time = 2021
#' )
#'
#' }
get_oecd_avlble_indctors_DPLIVE <- function(){

  dstruc <- suppressMessages(get_dstrucs("DP_LIVE"))


  indicator_details <- dstruc$INDICATOR %>%
    dplyr::rename(INDICATOR = .data$id,
                  indicator_desc = .data$label)

  return(indicator_details)

}


#' This function will look inside the 'DP_LIVE' data catalog and return a tibble
#' of data for the indicators chosen by the user.
#' Use `get_oecd_avlble_indctors_DPLIVE` to find out what is available. 'DP_LIVE'
#' contains various different economic, social and government indicators
#' reported by countries to the OECD. These are not harmonized statistics,
#' so when looking at things like GDP, CPI you will be viewing the data as
#' reported by the Country. These figures should align with reported numbers
#' on various FX calender websites or the relevant statistics agencies for the
#' relevant country.
#'
#' @param indicators (character vector) A vector of indicators requested by the
#' user. Example: c("EMP", "G_EMP", "LF") this would return various employment
#' indicators.
#' @param countries (character) Country abbreviations used by the function to
#' determine which countries will be extracted. Use `get_oecd_country_abbrevs`
#' to get the different abbreviations and associated country names.
#' @param start_time (numeric) The earliest year you want to go back to. The
#' function will attempt to go back as far as it can for the countries chosen.
#' @param end_time (numeric) Latest date you wish to extract data for.
#' @param remove_abbrev_cols (Boolean; TRUE) Will remove unnecessary abbreviation
#' columns if set to TRUE.
#'
#' @return (tibble)
#' @export
#'
#' @examples \dontrun{
#'
#' available_indicators <- get_oecd_avlble_indctors_DPLIVE()
#'
#' GDP_based_indicators <- available_indicators %>%
#'    filter(str_detect(indicator_desc, "GDP|gdp")) %>%
#'    pull(INDICATOR)
#'
#' dat <- get_oecd_data_DP_LIVE(
#'   indicators = GDP_based_indicators,
#'    countries = c("USA"),
#'    start_time = 2020,
#'    end_time = 2021
#' )
#'
#' }
get_oecd_data_DP_LIVE <- function(
  indicators = c("EMP", "G_EMP", "LF",
                 "G_UNEMP", "HUR", "LTUNEMP",
                 "LFPR", "QGDP", "HHSAV"),
  countries = c("USA", "GBR"),
  start_time = 2020,
  end_time = 2022,
  remove_abbrev_cols = TRUE
){

  dstruc <- suppressMessages(get_dstrucs("DP_LIVE"))

  country_list <- dstruc$LOCATION

  measure_details <- dstruc$MEASURE %>%
    dplyr::rename(MEASURE = .data$id,
                  measure_desc = .data$label)

  series_details <- dstruc$SUBJECT %>%
    dplyr::rename(SUBJECT = .data$id,
                  subject_desc = .data$label)

  indicator_details <- dstruc$INDICATOR %>%
    dplyr::rename(INDICATOR = .data$id,
                  indicator_desc = .data$label)

  full_country <- dstruc$LOCATION %>%
    dplyr::rename(LOCATION = .data$id,
                  country = .data$label)

  time_info <- dstruc$TIME_FORMAT %>%
    dplyr::rename(TIME_FORMAT = .data$id,
                  time_details = .data$label)

  frequency_info <- dstruc$FREQUENCY %>%
    dplyr::rename(FREQUENCY = .data$id,
                  frequency = .data$label)

  dat <- OECD::get_dataset(dataset = "DP_LIVE",
                               filter = list(countries, indicators),
                               start_time = start_time,
                               endtime = end_time) %>%
    oecd_rename_obsTime()

  dat_filter <- dat %>%
    dplyr::mutate(year = stringr::str_extract(.data$Time,"[0-9][0-9][0-9][0-9]")) %>%
    dplyr::mutate(quarter_date =
                    dplyr::case_when(
                      stringr::str_detect(.data$Time,"Q1") ~ glue::glue("{year}-03-01"),
                      stringr::str_detect(.data$Time,"Q2") ~ glue::glue("{year}-06-01"),
                      stringr::str_detect(.data$Time,"Q3") ~ glue::glue("{year}-09-01"),
                      stringr::str_detect(.data$Time,"Q4") ~ glue::glue("{year}-12-01")
                    )
    ) %>%
    dplyr::mutate(
      date = lubridate::as_date(.data$Time)
    ) %>%
    dplyr::left_join(
      full_country, by = "LOCATION"
    ) %>%
    dplyr::left_join(
      time_info, by = "TIME_FORMAT"
    )  %>%
    dplyr::left_join(
      series_details, by = "SUBJECT"
    )  %>%
    dplyr::left_join(
      indicator_details, by = "INDICATOR"
    ) %>%
    dplyr::left_join(
      measure_details, by = "MEASURE"
    ) %>%
    dplyr::left_join(
      frequency_info, by = "FREQUENCY"
    ) %>%
    dplyr::mutate(
      date = ifelse(!is.na(.data$quarter_date) & is.na(.data$date),
                    .data$quarter_date, .data$date)
    )

  if(remove_abbrev_cols){
    dat_filter <- dat_filter %>%
      dplyr::select(-.data$MEASURE, -.data$TIME_FORMAT, -.data$INDICATOR,
                    -.data$SUBJECT, -.data$Time,
                    -.data$FREQUENCY)
  }

  return(dat_filter)

}

oecd_rename_obsTime <- function(.data){

  dat_names <- names(.data)

  if (any(dat_names == "obsTime")) {

    dat_names <- stringr::str_replace(dat_names, pattern = "obsTime", replacement = "Time")

  }

  names(.data) <- dat_names

  return(.data)

}

get_dstrucs <- function(data_set) {

  dest <- paste0(tempdir(), "\\temp.rds")

  if (data_set == "DP_LIVE") {

    suppressMessages(download.file(url = "https://github.com/nikhilchandra-stats/macrodatasetsraw/raw/master/data/dstruc_DP_LIVE.rds",
                  destfile = dest))

    dat <- readRDS(file = dest)

  }

  if (data_set == "PRICES"|data_set == "PRICES_CPI") {

    suppressMessages(download.file(url = "https://github.com/nikhilchandra-stats/macrodatasetsraw/raw/master/data/dstruc_PRICES.rds",
                  destfile = dest))

    dat <- readRDS(file = dest)

  }

  if (data_set == "QNA") {

    suppressMessages(download.file(url = "https://github.com/nikhilchandra-stats/macrodatasetsraw/raw/master/data/dstruc_QNA.rds",
                  destfile = dest))

    dat <- readRDS(file = dest)

  }

  return(dat)

}

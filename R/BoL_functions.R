#' This function will retrieve data based on the provided seriesid, start year
#' and end year variables from the public BLS API. To find the seriesid for the
#' data you wish to query find the appropriate seriesid here:
#' https://data.bls.gov/cgi-bin/surveymost?bls
#' Other seriesid can be found on the BLS website by seraching through different
#' data topics. Please note that the v1 API is limited and some larger data
#' sources might not be possible to retrieve with this API.
#'
#'
#' @param url (character; "https://api.bls.gov/publicAPI/v1/timeseries/data/")
#' Change v1 to v2 if you have an API key that you can use to access the data.
#' @param seriesid (character) The series ID of the data you wish to retrieve.
#' @param startyear (character) The starting year for the data you want to
#' retrieve.
#' @param endyear (character) The ending year for the data you want to
#' retrieve.
#'
#' @return (tibble) Returns a tibble of the data you queried.
#' @export
#'
#' @examples \dontrun{
#'
#' dat <- make_bls_query(
#'    url = "https://api.bls.gov/publicAPI/v1/timeseries/data/",
#'    seriesid = c('CES0000000001'),
#'    startyear = 2010,
#'    endyear = 2019
#' )
#'
#' }
make_bls_query <- function(url = "https://api.bls.gov/publicAPI/v1/timeseries/data/",
                           seriesid = c('CES0000000001'),
                           startyear = '2010',
                           endyear = '2020'){

query_body <-  list('seriesid'= list(seriesid),
                      "startyear" = as.character(startyear),
                      "endyear" = as.character(endyear))

json_body <- jsonlite::toJSON(query_body , auto_unbox = T)

resp <-  httr::POST(url = url, body = json_body, httr::content_type_json())

converted <- jsonlite::fromJSON(rawToChar(resp$content))

dat <- converted$Results$series$data[[1]]

return(dat)

}

#' This function will retrieve stack multiple v1 queries together to
#' get data starting from the year 2000. This is required because of the
#' limited nature of the v1 API. Querying any 'popular series'
#' is limited to 10 years using the v1 API.
#'
#' @param series (character) The series ID of the data you wish to retrieve.
#' https://data.bls.gov/cgi-bin/surveymost?bls to find out what seriesid's are
#' available for use with this function and what data sets they retrieve from BLS.
#'
#' @return (tibble) Returns a data frame
#' @export
#'
#' @examples \dontrun{
#'
#' get_bls_2000_to_now(series = 'CES0000000001')
#'
#' }
get_bls_2000_to_now<- function(series){

  dat1 <- make_bls_query(
    url = "https://api.bls.gov/publicAPI/v1/timeseries/data/",
    seriesid = series,
    startyear = '2000',
    endyear = '2005'
  )

  Sys.sleep(0.1)

  dat2 <- make_bls_query(
    url = "https://api.bls.gov/publicAPI/v1/timeseries/data/",
    seriesid = series,
    startyear = '2006',
    endyear = '2010'
  )

  Sys.sleep(0.1)

  dat3 <- make_bls_query(
    url = "https://api.bls.gov/publicAPI/v1/timeseries/data/",
    seriesid = series,
    startyear = '2011',
    endyear = '2015'
  )

  Sys.sleep(0.1)

  current_year <- lubridate::year(lubridate::today()) %>% as.character()

  dat4 <- make_bls_query(
    url = "https://api.bls.gov/publicAPI/v1/timeseries/data/",
    seriesid = series,
    startyear = '2016',
    endyear = '2020'
  )

  Sys.sleep(0.1)

  dat5 <- make_bls_query(
    url = "https://api.bls.gov/publicAPI/v1/timeseries/data/",
    seriesid = series,
    startyear = '2020',
    endyear = current_year
  )

  returned_data <- dat1 %>%
    dplyr::bind_rows(dat2) %>%
    dplyr::bind_rows(dat3) %>%
    dplyr::bind_rows(dat4) %>%
    dplyr::bind_rows(dat5)

 return(returned_data)

}

#' This function will retrieve Labour Force Seasonally adjusted data from BLS
#' starting from 2000 to the current year.
#'
#' @return (tibble) Returns a data frame
#' @export
#'
#' @examples \dontrun{
#'
#' dat <- get_bls_total_LF()
#'
#' }
get_bls_total_LF <- function(){

  dat <- get_bls_2000_to_now("LNS11000000")

  return(dat)

}

#' This function will retrieve Civilian Employment Seasonally adjusted data from BLS
#' starting from 2000 to the current year.
#'
#' @return (tibble) Returns a data frame
#' @export
#'
#' @examples \dontrun{
#'
#' dat <- get_bls_total_civ_emp()
#'
#' }
get_bls_total_civ_emp <- function(){

  dat <- get_bls_2000_to_now("LNS12000000")

  return(dat)

}

#' This function will retrieve Civilian Unemployment Seasonally adjusted data from BLS
#' starting from 2000 to the current year.
#'
#' @return (tibble) Returns a data frame
#' @export
#'
#' @examples \dontrun{
#'
#' dat <- get_bls_total_civ_unemp()
#'
#' }
get_bls_total_civ_unemp <- function(){

  dat <- get_bls_2000_to_now("LNS14000000")

  return(dat)

}

#' This function will retrieve Private average weekly hours Seasonally adjusted
#' data from BLS starting from 2000 to the current year.
#'
#' @return (tibble) Returns a data frame
#' @export
#'
#' @examples \dontrun{
#'
#' dat <- get_bls_total_avg_wkly_hours()
#'
#' }
get_bls_total_avg_wkly_hours <- function(){

  dat <- get_bls_2000_to_now("CES0500000002")

  return(dat)

}

#' This function will retrieve Total Private Average Weekly Hours of Prod.
#' and Nonsup. Employees - Seasonally Adjusted - CES0500000007 data from BLS
#' starting from 2000 to the current year.
#'
#' @return (tibble) Returns a data frame
#' @export
#'
#' @examples \dontrun{
#'
#' dat <- get_bls_total_LF()
#'
#' }
get_bls_total_avg_wkly_hours_of_prod <- function(){

  dat <- get_bls_2000_to_now("CES0500000007")

  return(dat)

}

#' This function will retrieve Total Private Average Hourly Earnings of All Employees
#' - Seasonally Adjusted - CES0500000003 data from BLS starting from 2000 to
#' the current year.
#'
#' @return (tibble) Returns a data frame
#' @export
#'
#' @examples \dontrun{
#'
#' dat <- get_bls_total_avg_wkly_hrly_earnings()
#'
#' }
get_bls_total_avg_wkly_hrly_earnings <- function(){

  dat <- get_bls_2000_to_now("CES0500000003")

  return(dat)

}

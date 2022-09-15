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

#' This function will retrieve Total Non-Farm Employment data from BLS starting
#' from 2000 to the current year.
#'
#' @return (tibble) Returns a data frame of non-farm employment numbers.
#' @export
#'
#' @examples
get_bls_total_non_farm_emp <- function(){

  dat1 <- make_bls_query(
    url = "https://api.bls.gov/publicAPI/v1/timeseries/data/",
    seriesid = c('CES0000000001'),
    startyear = '2000',
    endyear = '2005'
  )

  Sys.sleep(0.25)

  dat2 <- make_bls_query(
    url = "https://api.bls.gov/publicAPI/v1/timeseries/data/",
    seriesid = c('CES0000000001'),
    startyear = '2006',
    endyear = '2010'
  )

  Sys.sleep(0.25)

  dat3 <- make_bls_query(
    url = "https://api.bls.gov/publicAPI/v1/timeseries/data/",
    seriesid = c('CES0000000001'),
    startyear = '2011',
    endyear = '2015'
  )

  Sys.sleep(0.25)

  current_year <- lubridate::year(lubridate::today()) %>% as.character()

  dat4 <- make_bls_query(
    url = "https://api.bls.gov/publicAPI/v1/timeseries/data/",
    seriesid = c('CES0000000001'),
    startyear = '2016',
    endyear = current_year
  )

  returned_data <- dat1 %>%
    dplyr::bind_rows(dat2) %>%
    dplyr::bind_rows(dat3) %>%
    dplyr::bind_rows(dat4)

 return(returned_data)

}

#' This function will retrieve data based on the provided seriesid, start year
#' and end year variables.
#'
#' @param url
#' @param seriesid
#' @param startyear
#' @param endyear
#'
#' @return
#' @export
#'
#' @examples
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

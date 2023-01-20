#' Extracts the table of geopolitical events from Wikipedia.
#'
#' @return (tibble)
#' @export
#'
#' @examples \dontrun{
#'
#' geo_events <- get_geo_pol_events()
#'
#' }
get_geo_pol_events <- function() {

  urlx <- "https://en.wikipedia.org/wiki/Timeline_of_geopolitical_changes_(2000%E2%80%93present)"

  dat <- xml2::read_html(urlx) %>%
    rvest::html_table()

  relavent_tables <- list(dat[[2]], dat[[3]], dat[[4]]) %>%
    purrr::map_dfr(dplyr::bind_rows) %>%
    dplyr::mutate(date =
                    glue::glue("{Year} {Date}") %>% as.character()) %>%
    dplyr::mutate(date = lubridate::as_date(date, format = "Y d B")) %>%
    dplyr::select(-Date)

  return(relavent_tables)

}

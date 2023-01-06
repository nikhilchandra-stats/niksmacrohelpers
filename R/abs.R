#' Download ABS quickstats with your chosen year, geography and
#' geo code. The geo code corrosponds to the code for the
#' region you are interested in ie; 10180 for Armadale Region LGA.
#'
#' @param year (character; 2021) The year of data you want
#' @param geography (character; "LGA") Geography type, ie; SA2, SA3
#' LGA, etc
#' @param geo_code (character; "10180") The code corrosponding to
#' the region you want ie; "10180" for Armadale Region, and
#' "116011303" for Blacktown east SA2 etc.
#' get region files here:
#' https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/allocation-files
#'
#' @return (tibble)
#' @export
#'
#' @examples \dontrun{
#'
#'dat <- get_abs_quick_stats(
#' year = "2016",
#' geography = "LGA",
#' geo_code = "10180"
#')
#'
#'
#'
#' }
get_abs_quick_stats <- function(year = "2021",
                                geography = "LGA",
                                geo_code = "10180") {


  geography <- toupper(geography)

  url <-
    glue::glue("https://www.abs.gov.au/census/find-census-data/quickstats/{year}/{geography}{geo_code}")

  dat_raw <- xml2::read_html(url) %>%
    rvest::html_table()

  col_7_dat <- dat_raw %>%
    purrr::keep(~dim(.x)[2] == 7) %>%
    purrr::map(
      ~ .x %>%
        dplyr::select(1:3) %>%
        dplyr::mutate(across(.fns = as.character)) %>%
        tidyr::pivot_longer(-1, names_to = "region", values_to = "value")%>%
        dplyr::mutate(temp = names(.x)[1]) %>%
        dplyr::rename("variable" = 1) %>%
        dplyr::mutate(
          variable = glue::glue("{temp};{variable}")
        )
    ) %>%
    purrr::map_dfr(bind_rows) %>%
    dplyr::mutate(
      value =
        dplyr::case_when(
          stringr::str_detect(.data$region, "%") ~ as.numeric(.data$value)/100,
          TRUE ~ readr::parse_number(.data$value)
        )
    ) %>%
    dplyr::mutate(
      variable =
        dplyr::case_when(
          stringr::str_detect(.data$region, "%") ~ glue::glue("{.data$variable} (%)"),
          TRUE ~ .data$variable
        )
    ) %>%
    dplyr::mutate(
      region = stringr::str_remove(.data$region, "%") %>% stringr::str_trim()
    ) %>%
    dplyr::select(-temp)

  families <-
    dat_raw[[2]] %>%
    dplyr::mutate(variable = "Number of families",
                  region = col_7_dat$region[1],
                  value = X2) %>%
    dplyr::select(-X1,-X2) %>%
    dplyr::slice(n = 1)%>%
    dplyr::mutate(
      value = readr::parse_number(value)
    )

  households <-
    dat_raw[[3]] %>%
    dplyr::mutate(
                  region = col_7_dat$region[1],
                  value = X2) %>%
    dplyr::rename(variable = X1) %>%
    dplyr::select(-X2) %>%
    dplyr::mutate(
      value = readr::parse_number(value)
    )

  returned <- col_7_dat %>%
    dplyr::bind_rows(households) %>%
    dplyr::bind_rows(families) %>%
    dplyr::mutate(
      geo_code = geo_code,
      geo_type = geography
    )

  return(returned)

}

get_abs_region_allocation <- function(geo_type = "SA2") {


  dest <- paste0(tempdir(), "\\temp.XLSX")

  if(str_detect(geo_type, "SA")) {

    url1 = "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/allocation-files/MB_2021_AUST.xlsx"

    httr::GET(url1, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

    dat <- readxl::read_excel(tf)

  }

  if(str_detect(geo_type, "POA|postcode|Postcode")) {

    url1 = "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/allocation-files/POA_2021_AUST.xlsx"

    httr::GET(url1, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

    dat <- readxl::read_excel(tf)

  }

  if(str_detect(geo_type, "LGA|lga|local")) {

    url1 = "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/allocation-files/LGA_2021_AUST.xlsx"

    httr::GET(url1, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

    dat <- readxl::read_excel(tf)

  }

  return(dat)

}



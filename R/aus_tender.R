#' Get Aus Tender data from the aus tender website.
#'
#' @param start_year (numeric; 2016) How far back do you want to go to, by default
#' it goes back to 2016.
#' @param wanted_data (character; tender extensions) What data from the website
#' are you looking for. Options are:
#'
#' "tender extensions": This will extract the contract extension report from the
#' webstite.
#'
#' "unspsc": This will extract uspsc data  from the website.
#'
#' "consultancy": This will extract the consultancy report from the website.
#'
#' "published contracts": Published contracts report.
#'
#'
#' @return (tibble)
#' @export
#'
#' @examples \dontrun{
#'
#' data <- get_aus_tender_data(start_year =2016, wanted_data = "tender extensions")
#'
#' }
get_aus_tender_data <- function(
    start_year= 2016,
    wanted_data = "tender extensions") {

  current_date <- lubridate::today() %>% format("%d-%b-%Y") %>% as.character()

  if(stringr::str_to_lower(wanted_data)  == "tender extensions") {

    base_url <-
      glue::glue("https://www.tenders.gov.au/Reports/CnAndAmendmentsDownload?AgencyStatus=-1&DateType=Publish%20Date&DateStart=01-Jan-{start_year}&DateEnd={current_date}")

    excel_skip <- 27

  }

  if(stringr::str_to_lower(wanted_data)  == "unspsc") {

    base_url <-
      glue::glue("https://www.tenders.gov.au/Reports/CnUnspscDownload?AgencyStatus=-1&DateStart=01-Jan-{start_year}&DateEnd={current_date}&DateType=Publish%20Date&GroupBy=By%20Category%20Code")

    excel_skip <- 9

  }

  if(stringr::str_to_lower(wanted_data)  == "consultancy") {

    base_url <-
      glue::glue("https://www.tenders.gov.au/Reports/CnConsultancyDownload?AgencyStatus=-1&DateStart=01-Jan-{start_year}&DateEnd={current_date}&DateType=Publish%20Date")

    excel_skip <- 15

  }

  if(stringr::str_to_lower(wanted_data)  == "published contracts") {

    base_url <-
      glue::glue("https://www.tenders.gov.au/Reports/CnPublishedDownload?AgencyStatus=0&DateType=Publish%20Date&DateStart=01-Jan-{start_year}&DateEnd={current_date}")

    excel_skip <- 19

  }

    url_for_dl <- as.character(base_url)

    httr::GET(url_for_dl, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

    dat <- readxl::read_excel(tf, skip = excel_skip)


  return(dat)

}


#' Go to the ABN website and extract information about a single ABN
#'
#' @param abn (character or numeric) The ABN you are looking to extract information
#' from.
#'
#' @return (tibble) Returns a tibble of extracted information.
#' @export
#'
#' @examples \dontrun{
#'
#' abns <- c("50128382187", "42000001007")
#'
#' data <- abns %>%
#'  purrr::map_dfr(extract_single_abn_data)
#'
#' }
extract_single_abn_data <- function(abn = "42000001007") {

  abn <- as.character(abn)

  url_base <- glue::glue("https://abr.business.gov.au/AbnHistory/View?id={abn}") %>% as.character()

  dat <- xml2::read_html(url_base) %>%
    rvest::html_table() %>%
    purrr::pluck(1)

  ACN <- dat %>%
    dplyr::filter(stringr::str_detect(`Entity name`, "A.C.N|ACN"))

  if(nrow(ACN) > 0) {

    ACN <- ACN %>%
      dplyr::pull(1) %>%
      stringr::str_extract_all("[0-9]+", simplify = T) %>%
      stringr::str_remove_all(pattern = " ") %>%
      paste0(collapse = "") %>%
      as.numeric()

  } else {
    ACN <- NA
  }


  business_name_1 <- dat[1,1] %>% as.character()
  business_name_2 <- dat[2,1] %>% as.character()

  business_start_date <- dat %>%
    dplyr::filter(stringr::str_detect(`Entity name`, "Active")) %>%
    dplyr::pull(2) %>%
    lubridate::as_date(format = "d b Y")

  business_end_date <- dat %>%
    dplyr::filter(stringr::str_detect(`Entity name`, "Active")) %>%
    dplyr::pull(3) %>%
    as.character()

  business_end_date <- ifelse(stringr::str_detect(tolower(business_end_date), "current"),
                              NA, lubridate::as_date(business_end_date, format = "d b Y"))

  current_post_code <- dat %>%
    dplyr::filter(
                  stringr::str_detect(`Entity name`, "VIC|NSW|ACT|Tas|TAS|SA|NT|Qld|QLD|WA")) %>%
    dplyr::filter(
                  stringr::str_detect(`Entity name`, "[0-9][0-9][0-9][0-9]"))

  current_post_code_date <- current_post_code %>%
    dplyr::pull(2) %>%
    purrr::pluck(1) %>%
    lubridate::as_date(format = "d b Y")

current_post_code_actual <- current_post_code %>%
  dplyr::pull(1) %>%
  purrr::pluck(1) %>%
  stringr::str_remove_all("[A-Z]|[a-z]") %>%
  stringr::str_remove_all(" ") %>%
  as.numeric()

current_state_actual <- current_post_code %>%
  dplyr::pull(1) %>%
  purrr::pluck(1) %>%
  stringr::str_extract_all("[A-Z]+|[a-z]+") %>%
  stringr::str_remove_all(" ")

returned_tibble <- dplyr::tibble(
  abn = as.numeric(abn),
  ACN = ACN,
  business_name_1 = business_name_1,
  business_name_2= business_name_2,
  business_start_date = business_start_date,
  business_end_date = business_end_date,
  current_post_code_actual = current_post_code_actual,
  current_state_actual = current_state_actual
)

return(returned_tibble)

}

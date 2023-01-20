get_aus_tender_extensions <- function(start_year= 2016,
                                      end_year = NULL) {


  current_date <- lubridate::today() %>% format("%d-%b-%Y") %>% as.character()

    base_url <-
    glue::glue("https://www.tenders.gov.au/Reports/CnAndAmendmentsDownload?AgencyStatus=-1&DateType=Publish%20Date&DateStart=01-Jan-2016&DateEnd=20-Jan-2023")

    url_for_dl <- as.character(base_url)

    httr::GET(url_for_dl, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

    dat <- readxl::read_excel(tf, skip = 27)

    return(dat)

}


get_aus_tender_unspc <- function(start_year= 2016,
                                      end_year = NULL) {


  current_date <- lubridate::today() %>% format("%d-%b-%Y") %>% as.character()

  base_url <-
    glue::glue("https://www.tenders.gov.au/Reports/CnAndAmendmentsDownload?AgencyStatus=-1&DateType=Publish%20Date&DateStart=01-Jan-2016&DateEnd=20-Jan-2023")

  url_for_dl <- as.character(base_url)

  httr::GET(url_for_dl, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

  dat <- readxl::read_excel(tf, skip = 27)

  return(dat)

}


get_aus_tender_data <- function(
    start_year= 2016,
    end_year = NULL,
    wanted_data = "tender extensions") {

  current_date <- lubridate::today() %>% format("%d-%b-%Y") %>% as.character()

  if(stringr::str_to_lower(wanted_data)  == "tender extensions") {

    base_url <-
      glue::glue("https://www.tenders.gov.au/Reports/CnAndAmendmentsDownload?AgencyStatus=-1&DateType=Publish%20Date&DateStart=01-Jan-2016&DateEnd={current_date}")

    excel_skip <- 27

  }

  if(stringr::str_to_lower(wanted_data)  == "unspsc") {

    base_url <-
      glue::glue("https://www.tenders.gov.au/Reports/CnUnspscDownload?AgencyStatus=-1&DateStart=01-Jan-2016&DateEnd={current_date}&DateType=Publish%20Date&GroupBy=By%20Category%20Code")

    excel_skip <- 9

  }

  if(stringr::str_to_lower(wanted_data)  == "consultancy") {

    base_url <-
      glue::glue("https://www.tenders.gov.au/Reports/CnConsultancyDownload?AgencyStatus=-1&DateStart=01-Jan-2016&DateEnd={current_date}&DateType=Publish%20Date")

    excel_skip <- 15

  }

  url_for_dl <- as.character(base_url)

  httr::GET(url_for_dl, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

  dat <- readxl::read_excel(tf, skip = excel_skip)

  return(dat)

}

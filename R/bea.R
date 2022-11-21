read_in_bea_key_local <- function(path = "C:/GitHub/bea_key.txt") {
  readr::read_file(path)
}

get_data_sets <- function(api_key = read_in_bea_key_local()) {

  url <- glue::glue("https://apps.bea.gov/api/data?&UserID={api_key}&method=GETDATASETLIST&") %>% as.character()

  res <- httr::GET(url = url)
  data_sets <- jsonlite::fromJSON( jsonlite::prettify(res) ) %>%
    purrr::pluck(1) %>%
    purrr::pluck(2) %>%
    purrr::pluck(1)

  url2 <- glue::glue("https://apps.bea.gov/api/data?&UserID={api_key}&method=getparameterlist&datasetname={data_sets$DatasetName[1]}") %>% as.character()

  res <- httr::GET(url = url2)
  parameters <- jsonlite::fromJSON( jsonlite::prettify(res) ) %>%
    purrr::pluck(1) %>%
    purrr::pluck(2) %>%
    purrr::pluck(1)

  url3 <- glue::glue("https://apps.bea.gov/api/data?&UserID={api_key}&method=GetParameterValues&datasetname=NIPA&ParameterName=TableID") %>% as.character()

  res <- httr::GET(url = url3)
  returned_value <- jsonlite::fromJSON( jsonlite::prettify(res) ) %>%
    purrr::pluck(1) %>%
    purrr::pluck(2) %>%
    purrr::pluck(1)
}

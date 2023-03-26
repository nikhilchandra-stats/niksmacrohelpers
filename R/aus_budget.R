#' Extract out AUS Budget Paper 2 Data from word doc
#'
#' @param skip_tables
#' @param financial_year
#'
#' @return
#'
#' @examples
extract_aus_BP2 <- function(skip_tables = 32,
                            financial_year = 2022) {

  dest <- paste0(tempdir(), "\\temp.docx")

  url1 =  "https://budget.gov.au/2022-23-october/content/bp2/download/bp2_02_receipt_payment.docx"

  httr::GET(url1, httr::write_disk(tf <- tempfile(fileext = ".docx")))

  read_doc <- docxtractr::read_docx(tf)

  tables <- read_doc %>% docxtractr::docx_extract_all_tbls()

  tables_selected <- tables[skip_tables:length(tables)]

  num_of_cols <- ncol(tables_selected[[1]]) - 1

  fy_names <- c()

  incrementor <- -1:(num_of_cols - 2)

  for (i in  1:length(incrementor)) {
    fy_names[i] <- glue::glue("fy_{financial_year +incrementor[i]}_{financial_year + (incrementor[i] + 1)}")
  }

  tibble_names <- c("var", fy_names)

  table_cleaner <- function(table_data,
                            tibble_names) {

    names(table_data) <- tibble_names

    table_data %>%
      dplyr::mutate(
        department =
          ifelse(!stringr::str_detect(var, "[a-z]"), var, NA)
      ) %>%
      tidyr::fill(department, .direction = "down") %>%
      dplyr::mutate(
        units = stringr::str_extract(!!as.name(tibble_names[2]), "\\$[a-z]")
      ) %>%
      tidyr::fill(units, .direction = "down") %>%
      dplyr::filter(var != "") %>%
      dplyr::filter(!!as.name(tibble_names[3]) != "" & !!as.name(tibble_names[4]) != "") %>%
      dplyr::rename(program = var) %>%
      dplyr::mutate(
        dplyr::across(.cols = c(-program, -units, -department), )
      )
  }

  safely_table_clean <- purrr::safely(table_cleaner, otherwise = NULL)

  data_in_list <- tables_selected %>%
    purrr::map(~ safely_table_clean(table_data = .x, tibble_names = tibble_names) %>%
                 purrr::pluck('result')) %>%
    purrr::keep(~!is.null(.x)) %>%
    purrr::keep(~ nrow(.x) >= 1 ) %>%
    purrr::map_dfr(dplyr::bind_rows) %>%
    dplyr::mutate(
      department =
        dplyr::case_when(
          department != "" ~ department
        )
    )

}

#' This function will extract out data from PBS Budget data from data.gov
#'
#' @param .path (character) Path to excel file containing the PBS data.
#' @param entity_name (character) The entity name of the data in the excel file
#' @param budget_calendar_year (numeric) The calendar year the budget was
#' published.
#'
#' @return (list) Returns a list of the tables extracted from the sheet.
#' @export
#'
#' @examples \dontrun{
#'
#' clean_extract_PBS_xlsx(
#' .path = "2021-22 PBS AFP.XLSX",
#'  entity_name = "Australian Federal Police",
#'  budget_calendar_year = 2021)
#'
#' }
clean_extract_PBS_xlsx <- function(.path = "australianfederalpolice201415.xlsx",
                                   entity_name = "Australian Federal Police",
                                   budget_calendar_year = 2014) {

  sheet_names <- readxl::excel_sheets(.path)

  budget_year_vec <- seq(budget_calendar_year, budget_calendar_year + 4, 1) %>%
    purrr::map(
      ~ glue::glue("{.x - 1}_{substr(.x %>% as.character(), 3,4)}")
    ) %>%
    unlist()

  for (j in 1:length(budget_year_vec)) {

    if(j == 1) {
      budget_year_vec[j] = glue::glue("estimated_actual_{budget_year_vec[j]}")
    }

    if(j == 2) {
      budget_year_vec[j] = glue::glue("budget_{budget_year_vec[j]}")
    }

    if(j != 2 & j != 1) {
      budget_year_vec[j] = glue::glue("FE_{budget_year_vec[j]}")
    }

  }

  table_names <- c()
  tables <- list()

  for (i in 1:length(sheet_names)) {

    table_names_temp <-
      names(readxl::read_excel(path = .path, sheet = sheet_names[i], range = "A1" ))[1]

    start_index <- 2

    if(is.na(table_names_temp)){
      table_names_temp <-
        names(readxl::read_excel(path = .path, sheet = sheet_names[i], range = "A2" ))[1]

      start_index <- 3
    }

    if( stringr::str_detect(sheet_names[i],"1.1") ) {
      table_temp <-
        readxl::read_excel(path = .path, sheet = sheet_names[i],
                           range = glue::glue("A{start_index}:C30") %>% as.character()
                           ) %>%
        dplyr::rename(var = 1,
                      !!as.name(budget_year_vec[1]) := 2,
                      !!as.name(budget_year_vec[2]) := 3) %>%
        dplyr::mutate(
          entity_name = entity_name,
          variable_type = table_names_temp
        )
    }

    if(stringr::str_detect(sheet_names[i],"1.2")) {
      table_temp <-
        readxl::read_excel(path = .path, sheet = sheet_names[i],
                           range = glue::glue("A{start_index + 1}:G30") %>% as.character()) %>%
        dplyr::rename(var = 1,
                      program = 2,
                      !!as.name(budget_year_vec[1]) := 3,
                      !!as.name(budget_year_vec[2]) := 4,
                      !!as.name(budget_year_vec[3]) := 5,
                      !!as.name(budget_year_vec[4]) := 6,
                      !!as.name(budget_year_vec[5]) := 7
                      ) %>%
        dplyr::mutate(
          entity_name = entity_name,
          variable_type = table_names_temp
        ) %>%
        dplyr::filter(
          !is.na(var)
        ) %>%
        tidyr::fill(program, .direction = "down")
    }

    if(stringr::str_detect(sheet_names[i],"2.2|2.3|3.1|3.2")) {
      table_temp <-
        readxl::read_excel(path = .path, sheet = sheet_names[i],
                           range = glue::glue("A{start_index}:F100") %>% as.character()) %>%
        dplyr::rename(var = 1,
                      !!as.name(budget_year_vec[1]) := 2,
                      !!as.name(budget_year_vec[2]) := 3,
                      !!as.name(budget_year_vec[3]) := 4,
                      !!as.name(budget_year_vec[4]) := 5,
                      !!as.name(budget_year_vec[5]) := 6
        ) %>%
        dplyr::mutate(
          entity_name = entity_name,
          variable_type = table_names_temp
        ) %>%
        dplyr::filter(
          !is.na(var)
        )
    }

    if(stringr::str_detect(sheet_names[i],"2.1|2.2|2.3|3.1|3.2|3.4|3.5|3.6|3.8|3.9")) {
      table_temp <-
        readxl::read_excel(path = .path, sheet = sheet_names[i],
                           range = glue::glue("A{start_index + 1}:F100") %>% as.character()) %>%
        dplyr::rename(var = 1,
                      !!as.name(budget_year_vec[1]) := 2,
                      !!as.name(budget_year_vec[2]) := 3,
                      !!as.name(budget_year_vec[3]) := 4,
                      !!as.name(budget_year_vec[4]) := 5,
                      !!as.name(budget_year_vec[5]) := 6
        ) %>%
        dplyr::mutate(
          entity_name = entity_name,
          variable_type = table_names_temp
        ) %>%
        dplyr::filter(
          !is.na(var)
        )
    }

    if(stringr::str_detect(sheet_names[i],"3.3")) {
      table_temp <-
        readxl::read_excel(path = .path, sheet = sheet_names[i],
                           range = glue::glue("A{start_index}:E100") %>% as.character()) %>%
        dplyr::rename(var = 1) %>%
        dplyr::mutate(
          entity_name = entity_name,
          variable_type = table_names_temp
        ) %>%
        dplyr::filter(
          !is.na(var)
        )
    }

    tables[[i]] <- table_temp

  }


  return(tables)

}

#' Download, read, clean PBS line items
#'
#' The is function will fetch data from data.gov on PBS expense items. It will
#' then clean and join all the data and return data in long form. The data will
#' be a timeseries going back to 2014-15 Budget.
#'
#' @return (tibble) A timeseries of Budget Data going back to 2014-15
#' @export
#'
#' @examples \dontrun{
#'
#' pbs_data <- get_pbs_line_items()
#'
#' }
get_pbs_expense_items <- function() {

  url <-
    dplyr::tibble(
      urls = c("https://data.gov.au/data/dataset/98039365-ae78-4290-8d7c-eb0a4fcbd52b/resource/21aa5327-eca8-4319-8596-549c1080856e/download/2021-22-pbs-program-expense-line-items.csv",
               "https://data.gov.au/data/dataset/86d7d307-92e2-48d9-b375-480685056673/resource/b8a3bd38-9662-4a00-9ffc-5c0c97d6b5bf/download/2020-21-pbs-program-expense-line-items_20201012.csv",
               "https://data.gov.au/data/dataset/2fe0ab1a-3161-477a-9175-5c829d80afab/resource/893cbd3a-4d00-463e-b645-439afaf83337/download/2019-20-pbs-program-expense-line-items.csv",
               "https://data.gov.au/data/dataset/d8f51107-d0de-4daf-a7d8-0fb7fbdce504/resource/d5bca8a5-f557-40da-8e5b-62047ce12802/download/2017-18-pbs-program-expenses-line-items-1.csv",
               "https://data.gov.au/data/dataset/0c516e3a-0fb0-45a4-ac3f-0ab33c265885/resource/bd6a09a3-7687-4cc2-ae61-d03c5d03c594/download/2016-17-pbs-line-item-dataset.csv",
               "https://data.gov.au/data/dataset/5b54386d-4b46-4736-87c5-28bd5ee38bcc/resource/365051dd-9335-4c2b-8c29-331718e079eb/download/201505181230budget1516.csv"
      ),
      calendar_year = c(2021, 2020, 2019, 2017, 2016, 2015)
    )

  url_xlsx <-
    dplyr::tibble(
      urls =
        c("https://data.gov.au/data/dataset/863c394c-a26c-4340-896b-a26b18af476d/resource/66ee77e9-34b0-4215-8bc3-bfa43b7c8f11/download/2018-19-pbs-program-expense-line-items-1.xlsx",
          "https://data.gov.au/data/dataset/c754efb9-f456-4417-acc7-bd96cff93cb4/resource/dacb615b-19ad-4f8f-b846-01e53c65589f/download/budget201415tablesanddata.xlsx"),
      calendar_year = c(2018, 2014)
    )

  rename_function <-
    function(read_in_data = test_data,
             calendar_year = 2015) {

      names_dat <- names(read_in_data) %>%
        purrr::map(
          ~
            dplyr::case_when(
              stringr::str_detect(.x, "x[0-9]+_[0-9]+") ~
                stringr::str_remove_all(.x, pattern = "[A-Z]|[a-z]|_[0-9]+"),
              TRUE ~ .x
            )
        ) %>%
        purrr::map(
          ~
            dplyr::case_when(
              as.numeric(.x) >= calendar_year ~ glue::glue("FE_{.x}"),
              as.numeric(.x) < calendar_year ~ glue::glue("estimated_actual_{.x}"),
              TRUE ~ .x
            )
        ) %>%
        unlist() %>%
        as.character()

      names(read_in_data) <-  names_dat

      complete_data <- read_in_data %>%
        dplyr::mutate(dplyr::across(tidyselect::contains("FE_"), ~as.numeric(.))) %>%
        dplyr::mutate(dplyr::across(tidyselect::contains("estimated_actual_"), ~as.numeric(.))) %>%
        tidyr::pivot_longer(tidyselect::contains(c("estimated_actual_", "FE_")),
                            names_to = "estimate_actual", values_to = "value")

      return(complete_data)

    }


  dat_csv <- url %>%
    split(.$calendar_year, drop = FALSE) %>%
    purrr::map_dfr(
      ~ readr::read_csv(.x$urls[1]) %>%
        janitor::clean_names() %>%
        rename_function(calendar_year = .x$calendar_year[1]) %>%
        dplyr::mutate(publish_year = .x$calendar_year[1])
    )

  xlsx_data <- url_xlsx %>%
    split(.$calendar_year, drop = FALSE) %>%
    purrr::map_dfr(
      ~ download_read_xlsx(.x$urls[1]) %>%
        janitor::clean_names() %>%
        rename_function(calendar_year = .x$calendar_year[1])%>%
        dplyr::mutate(publish_year = .x$calendar_year[1])
    )

  returned_data <-
    dat_csv %>%
    dplyr::bind_rows(xlsx_data)

  return(returned_data)

}

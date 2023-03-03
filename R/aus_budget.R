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

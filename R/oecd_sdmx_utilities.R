#' This function preserves extracts variable details for the chosen dataset.
#' For example if you give it QNA, it will return a list that provides
#' information on what each of the codes in the QNA dataset means. Eg; One of
#' the returned list elements will be subject that returns details for what each
#' of the coded values are
#' ie; CP00 = "National Consumer Price Index (CPI) by COICOP 1999 classification".
#'
#' @param dataset A string that is a dataset present in the OECD database. Use
#' OECD::get_datasets() to determine the possible options.
#'
#' @return (list) Returns a list that functions as a data dictionary for the
#' different variables in the data.
#' @export
#'
#' @examples
get_oecd_data_structure <- function (dataset) {

  url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/",
                dataset)
  data_structure <- rsdmx::readSDMX(url)

  descriptions <- data_structure@concepts %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      en =
        dplyr::case_when(
          is.na(.data$en) ~ .data$Name.en,
          TRUE ~ .data$en
        )
    ) %>%
    dplyr::rename(
       description = .data$en
    ) %>%
    dplyr::select(id, description)

  oecd_codes <- data_structure@codelists@codelists %>%
    as.list() %>%
    purrr::map( ~ .x@id) %>%
    unlist()

  df_lists <- list()

  for (j in 1:length(oecd_codes)) {

    df_lists[[j]] <- as.data.frame(data_structure@codelists, codelistId = oecd_codes[j]) %>%
      dplyr::select(.data$id, label = .data$label.en)

  }

  oecd_codes <- stringr::str_remove_all(oecd_codes, "CL_") %>%
    stringr::str_remove_all(dataset)%>%
    stringr::str_remove("_")

  names(df_lists) <- oecd_codes

  complete_data_list <- c(VAR_DESC = list(descriptions), df_lists)
#-------------------------------------------------------------------------------------------

  return(complete_data_list)

}

#' get_ABS_MB_pop
#'
#' This function will fetch the ABS population by meshblock from the ABS website
#'
#' Data is downloaded from:
#' "https://www.abs.gov.au/census/guide-census-data/mesh-block-counts/2021/"
#'
#' @param path
#' @param url_x
#'
#' @return
#' @export
#'
#' @examples
get_ABS_MB_pop <- function(url_x =
                             "https://www.abs.gov.au/census/guide-census-data/mesh-block-counts/2021/Mesh%20Block%20Counts%2C%202021.xlsx"
                           ) {

  dest <- paste0(tempdir(), "\\temp.XLSX")

  httr::GET(url_x, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))

  sheets_to_read <-
    readxl::excel_sheets(tf) %>%
    purrr::keep(~ stringr::str_detect(.x, "Table"))


  dat <- readxl::read_excel(tf, sheet = sheets_to_read[1], skip = 6)

  for (i in 2:length(sheets_to_read)) {

    dat <-
      dat %>%
      dplyr::bind_rows(
        readxl::read_excel(tf, sheet = sheets_to_read[i], skip = 6)
      )

  }
  return(dat)
}


#' get_WA_CED_redist
#'
#' @return (tibble) Tibble of Redistribution
#' @export
#'
#' @examples \dontrun{
#'
#' WA_redist <- get_CED_redist()
#'
#' }
get_CED_redist <- function(
    MB_allocation_file = get_abs_region_allocation(),
    region_allocation = "NSW electorate"
  ) {

  redist <- get_abs_region_allocation(geo_type = region_allocation)
  redist <- redist[,c(1,2,4)]
  names(redist) <- c("SA1_CODE_2021_join", "CED_NAME_2021", "SA2_NAME_2021")
  redist <- redist %>%
    dplyr::distinct(SA1_CODE_2021_join, CED_NAME_2021, SA2_NAME_2021)


  redist_MB_mapped<- MB_allocation_file %>%
    dplyr::distinct(MB_CODE_2021 ,SA1_CODE_2021 ,SA2_NAME_2021, STATE_NAME_2021) %>%
    dplyr::mutate(
      SA1_CODE_2021_join =
        paste0(stringr::str_sub(SA1_CODE_2021, start = 1, end = 1),
               stringr::str_sub(SA1_CODE_2021, start = 6, end = 11))
    ) %>%
    dplyr::left_join(redist
                     ) %>%
    dplyr::filter(!is.na(CED_NAME_2021)) %>%
    dplyr::select(-SA2_NAME_2021)

  return(redist_MB_mapped)

}

#' concord_CED_POA_by_pop
#'
#' This function will conocord CED to POA using meshblock population.
#' All allocation data is downloaded from:
#' "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/allocation-files/"
#'
#' @param MB_pop (tibble) Meshblock Population. By Default it will draw this from the ABS
#' website using an internal function in this package.
#' @param POA_data (tibble) POA Allocation Data. By Default it will draw this from the ABS
#' website using an internal function in this package.
#' @param CED_data (tibble) CED Allocation Data. By Default it will draw this from the ABS
#' website using an internal function in this package.
#'
#' @return (tibble) Returns a tibble with POA and CED concorded with percentage allocation
#' based on Meshblock population
#' @export
#'
#' @examples
concord_CED_POA_by_pop <- function(MB_pop =
                                     get_ABS_MB_pop() ,
                                   POA_data =
                                     get_abs_region_allocation(geo_type = "POA") ,
                                   CED_data =
                                     get_abs_region_allocation(geo_type = "CED")
                                   ) {


  MB_pop <- MB_pop %>%
    dplyr::distinct(MB_CODE_2021, Person, Dwelling, State, AREA_ALBERS_SQKM)

  POA_data <- POA_data %>%
    dplyr::distinct(MB_CODE_2021, POA_CODE_2021)

  CED_data <- CED_data %>%
    dplyr::distinct(MB_CODE_2021, CED_CODE_2021, CED_NAME_2021, STATE_NAME_2021)

  MB_POA_CED_joined <- MB_pop %>%
    dplyr::left_join(POA_data) %>%
    dplyr::left_join(CED_data) %>%
    dplyr::filter(!is.na(POA_CODE_2021)) %>%
    dplyr::filter(!is.na(CED_NAME_2021))

  MB_POA_CED_Summarised <-
    MB_POA_CED_joined %>%
    dplyr::group_by(CED_CODE_2021, CED_NAME_2021, POA_CODE_2021, STATE_NAME_2021) %>%
    dplyr::mutate(dplyr::across(c(Person, Dwelling,  AREA_ALBERS_SQKM), .fns = ~ as.numeric(.))) %>%
    dplyr::summarise(
      dplyr::across(
        c(Person, Dwelling,  AREA_ALBERS_SQKM),
        .fns = ~ sum(., na.rm = T)
      )
    ) %>%
    dplyr::group_by(CED_NAME_2021) %>%
    dplyr::mutate(
      poa_count = dplyr::n()
    ) %>%
    dplyr::group_by(POA_CODE_2021) %>%
    dplyr::mutate(
      double_counted_POA = dplyr::n()
    ) %>%
    dplyr::group_by(POA_CODE_2021) %>%
    dplyr::mutate(
      POA_Allocation =
        Person/sum(Person, na.rm = T)
    )

  return(MB_POA_CED_Summarised)

}

#' concord_CED_POA_by_pop_NSW_redist
#'
#' This function will conocord CED to POA using meshblock population but will include the new NSW redistribution
#' All allocation data is downloaded from:
#' "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/allocation-files/"
#'
#' @param MB_pop (tibble) Meshblock Population. By Default it will draw this from the ABS
#' website using an internal function in this package.
#' @param POA_data (tibble) POA Allocation Data. By Default it will draw this from the ABS
#' website using an internal function in this package.
#' @param CED_data (tibble) CED Allocation Data. By Default it will draw this from the ABS
#' website using an internal function in this package.
#'
#' @return (tibble) Returns a tibble with POA and CED concorded with percentage allocation
#' based on Meshblock population
#' @export
#'
#' @examples
concord_CED_POA_by_pop_redist <- function(MB_pop =
                                               get_ABS_MB_pop() ,
                                             POA_data =
                                               get_abs_region_allocation(geo_type = "POA") ,
                                             CED_data =
                                               get_abs_region_allocation(geo_type = "CED"),
                                             NSW_redist_MB_mapped = get_CED_redist(region_allocation = "NSW electorate"),
                                             WA_redist_MB_mapped = get_CED_redist(region_allocation = "WA electorate"),
                                             VIC_redist_MB_mapped = get_CED_redist(region_allocation = "VIC electorate")
) {


  MB_pop <- MB_pop %>%
    dplyr::distinct(MB_CODE_2021, Person, Dwelling, State, AREA_ALBERS_SQKM)

  POA_data <- POA_data %>%
    dplyr::distinct(MB_CODE_2021, POA_CODE_2021)

  CED_data <- CED_data %>%
    dplyr::distinct(MB_CODE_2021, CED_CODE_2021, CED_NAME_2021, STATE_NAME_2021)

  MB_POA_CED_joined <- MB_pop %>%
    dplyr::left_join(POA_data) %>%
    dplyr::left_join(CED_data) %>%
    dplyr::filter(!is.na(POA_CODE_2021)) %>%
    dplyr::filter(!is.na(CED_NAME_2021))

  NSW_redist_MB_mapped_join <-
    NSW_redist_MB_mapped %>%
    dplyr::left_join(MB_pop) %>%
    dplyr::left_join(POA_data) %>%
    dplyr::filter(!is.na(POA_CODE_2021))

  WA_redist_MB_mapped_join <-
    WA_redist_MB_mapped %>%
    dplyr::left_join(MB_pop) %>%
    dplyr::left_join(POA_data) %>%
    dplyr::filter(!is.na(POA_CODE_2021))

  VIC_redist_MB_mapped_join <-
    VIC_redist_MB_mapped %>%
    dplyr::left_join(MB_pop) %>%
    dplyr::left_join(POA_data) %>%
    dplyr::filter(!is.na(POA_CODE_2021))

  MB_POA_CED_Summarised <-
    MB_POA_CED_joined %>%
    dplyr::group_by(CED_CODE_2021, CED_NAME_2021, POA_CODE_2021, STATE_NAME_2021) %>%
    dplyr::mutate(dplyr::across(c(Person, Dwelling,  AREA_ALBERS_SQKM), .fns = ~ as.numeric(.))) %>%
    dplyr::summarise(
      dplyr::across(
        c(Person, Dwelling,  AREA_ALBERS_SQKM),
        .fns = ~ sum(., na.rm = T)
      )
    ) %>%
    dplyr::group_by(CED_NAME_2021) %>%
    dplyr::mutate(
      poa_count = dplyr::n()
    ) %>%
    dplyr::group_by(POA_CODE_2021) %>%
    dplyr::mutate(
      double_counted_POA = dplyr::n()
    ) %>%
    dplyr::group_by(POA_CODE_2021) %>%
    dplyr::mutate(
      POA_Allocation =
        Person/sum(Person, na.rm = T)
    )

  NSW_redist_MB_mapped_join_summ <-
    NSW_redist_MB_mapped_join %>%
    dplyr::group_by(CED_NAME_2021, POA_CODE_2021) %>%
    dplyr::mutate(dplyr::across(c(Person, Dwelling,  AREA_ALBERS_SQKM), .fns = ~ as.numeric(.))) %>%
    dplyr::summarise(
      dplyr::across(
        c(Person, Dwelling,  AREA_ALBERS_SQKM),
        .fns = ~ sum(., na.rm = T)
      )
    ) %>%
    dplyr::group_by(CED_NAME_2021) %>%
    dplyr::mutate(
      poa_count = dplyr::n()
    ) %>%
    dplyr::group_by(POA_CODE_2021) %>%
    dplyr::mutate(
      double_counted_POA = dplyr::n()
    ) %>%
    dplyr::group_by(POA_CODE_2021) %>%
    dplyr::mutate(
      POA_Allocation =
        Person/sum(Person, na.rm = T)
    ) %>%
    dplyr::mutate(
      STATE_NAME_2021 = "New South Wales"
    )

  WA_redist_MB_mapped_join_summ <-
    WA_redist_MB_mapped_join %>%
    dplyr::group_by(CED_NAME_2021, POA_CODE_2021) %>%
    dplyr::mutate(dplyr::across(c(Person, Dwelling,  AREA_ALBERS_SQKM), .fns = ~ as.numeric(.))) %>%
    dplyr::summarise(
      dplyr::across(
        c(Person, Dwelling,  AREA_ALBERS_SQKM),
        .fns = ~ sum(., na.rm = T)
      )
    ) %>%
    dplyr::group_by(CED_NAME_2021) %>%
    dplyr::mutate(
      poa_count = dplyr::n()
    ) %>%
    dplyr::group_by(POA_CODE_2021) %>%
    dplyr::mutate(
      double_counted_POA = dplyr::n()
    ) %>%
    dplyr::group_by(POA_CODE_2021) %>%
    dplyr::mutate(
      POA_Allocation =
        Person/sum(Person, na.rm = T)
    ) %>%
    dplyr::mutate(
      STATE_NAME_2021 = "Western Australia"
    )

  VIC_redist_MB_mapped_join_summ <-
    VIC_redist_MB_mapped_join %>%
    dplyr::group_by(CED_NAME_2021, POA_CODE_2021) %>%
    dplyr::mutate(dplyr::across(c(Person, Dwelling,  AREA_ALBERS_SQKM), .fns = ~ as.numeric(.))) %>%
    dplyr::summarise(
      dplyr::across(
        c(Person, Dwelling,  AREA_ALBERS_SQKM),
        .fns = ~ sum(., na.rm = T)
      )
    ) %>%
    dplyr::group_by(CED_NAME_2021) %>%
    dplyr::mutate(
      poa_count = dplyr::n()
    ) %>%
    dplyr::group_by(POA_CODE_2021) %>%
    dplyr::mutate(
      double_counted_POA = dplyr::n()
    ) %>%
    dplyr::group_by(POA_CODE_2021) %>%
    dplyr::mutate(
      POA_Allocation =
        Person/sum(Person, na.rm = T)
    ) %>%
    dplyr::mutate(
      STATE_NAME_2021 = "Victoria"
    )

  existing_codes <- MB_POA_CED_Summarised %>%
    dplyr::distinct(CED_NAME_2021, CED_CODE_2021)

  MB_POA_CED_Summarised_with_NSW <-MB_POA_CED_Summarised %>%
    dplyr::ungroup() %>%
    dplyr::filter(STATE_NAME_2021 != "New South Wales") %>%
    dplyr::filter(STATE_NAME_2021 != "Western Australia") %>%
    dplyr::filter(STATE_NAME_2021 != "Victoria") %>%
    dplyr::bind_rows(NSW_redist_MB_mapped_join_summ) %>%
    dplyr::bind_rows(WA_redist_MB_mapped_join_summ)%>%
    dplyr::bind_rows(VIC_redist_MB_mapped_join_summ) %>%
    dplyr::group_by(CED_NAME_2021) %>%
    tidyr::fill(CED_CODE_2021, .direction = "updown") %>%
    dplyr::select(-CED_CODE_2021) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(existing_codes)

  return(MB_POA_CED_Summarised_with_NSW)

}


#' get_ABS_TB_data_Education
#'
#' This function concords ABS Table Builder data on education attainment from
#' Table Builder to CED.The TB data is stored in my Github Repo.
#'
#' TB = ABS Table Builder
#'
#' @param path (character) url for where the TB data is stored
#' @param MB_pop (tibble) Meshblock Population. By Default it will draw this from the ABS
#' website using an internal function in this package.
#' @param MB_allocation_file (tibble) ASGS Allocation data. By Default it will draw this from the ABS
#' website using an internal function in this package.
#' @param summarise_cols (character vector) This is the columns from the main TB data set you
#' want to summarise.
#' @param POA_data (tibble) POA Allocation Data. By Default it will draw this from the ABS
#' website using an internal function in this package.
#' @param CED_data (tibble) CED Allocation Data. By Default it will draw this from the ABS
#' website using an internal function in this package.
#'
#' @return (tibble) Returns TB data summarised by CED
#' @export
#'
#' @examples
concord_ABS_TB_Education_CED <- function(path = "https://raw.githubusercontent.com/nikhilchandra-stats/macrodatasetsraw/refs/heads/master/data/SA1_Education.csv",
                                      MB_pop = get_ABS_MB_pop(),
                                      POA_data =
                                        get_abs_region_allocation(geo_type = "POA") ,
                                      CED_data =
                                        get_abs_region_allocation(geo_type = "CED"),
                                      MB_allocation_file = get_abs_region_allocation(),
                                      summarise_cols =
                                        c("Dwelling", "Person", "Bacchelors_and_above",
                                          "Bacchelors_and_above_summed",
                                          "Postgraduate Degree Level, nfd" ,
                                          "Bachelor Degree Level, nfd" , "Higher Doctorate",
                                          "Professional Specialist Qualification", "Doctoral Degree Level",
                                          "Master Degree Level, nfd" ,
                                          "Graduate Diploma and Graduate Certificate Level, nfd",
                                          "Graduate Diploma" , "Graduate Certificate" )
) {

  MB_pop_with_SA1 <-
    MB_pop %>%
    dplyr::left_join(
      MB_allocation_file %>% dplyr::distinct(MB_CODE_2021, SA1_CODE_2021)
    )

  MB_POA_CED_joined_SA1 <- MB_pop_with_SA1 %>%
    dplyr::left_join(POA_data) %>%
    dplyr::left_join(CED_data) %>%
    dplyr::filter(!is.na(POA_CODE_2021)) %>%
    dplyr::filter(!is.na(CED_NAME_2021))

  dat_temp <- readr::read_csv(file = path, skip = 9)
  dat_temp2 <- dat_temp[-1,]
  dat_temp3 <- dat_temp2[,-c(length(dat_temp2), length(dat_temp2) - 1 )]
  dat_temp4 <- dat_temp3 %>%
    dplyr::rename(SA1_CODE_2021 = 1)

  dat_temp4 <- dat_temp4 %>%
    dplyr::mutate(
      Bacchelors_and_above_summed =
        `Postgraduate Degree Level, nfd` +
        `Bachelor Degree Level, nfd` + `Higher Doctorate` +
        `Professional Specialist Qualification, Doctoral Degree Level` +
        `Master Degree Level, nfd` + `Graduate Diploma and Graduate Certificate Level, nfd` +
        `Graduate Diploma` + `Graduate Certificate` )

  MB_POA_CED_joined_SA1_summed <- MB_POA_CED_joined_SA1  %>%
    dplyr::group_by(SA1_CODE_2021, POA_CODE_2021, CED_CODE_2021, CED_NAME_2021, STATE_NAME_2021) %>%
    dplyr::summarise(Person = sum(Person, na.rm = T),
              Dwelling = sum(Dwelling, na.rm = T),
              AREA_ALBERS_SQKM = sum(Person, na.rm = T))

  dat_temp4 <- MB_POA_CED_joined_SA1_summed %>%
    dplyr::left_join(dat_temp4) %>%
    dplyr::group_by(CED_CODE_2021, CED_NAME_2021, SA1_CODE_2021, STATE_NAME_2021) %>%
    dplyr::summarise(
      dplyr::across(.cols = tidyselect::matches(summarise_cols, ignore.case = FALSE),
             .fns = ~ sum(., na.rm = T)
      )
    )

  dat_temp5 <- dat_temp4 %>%
    dplyr::group_by( SA1_CODE_2021) %>%
    dplyr::mutate(SA1_count = dplyr::n_distinct(CED_NAME_2021)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(CED_NAME_2021) %>%
    dplyr::summarise(
      dplyr::across(.cols = tidyselect::matches(summarise_cols, ignore.case = FALSE),
             .fns = ~ sum(., na.rm = T)
      )
    )

  names(dat_temp5)[-1] <- paste0("CED - ", names(dat_temp5)[-1])

  return(dat_temp5)

}

#' get_ABS_TB_data_LFS
#'
#' This function concords ABS Table Builder data on Labour Force Status from
#' Table Builder to CED.The TB data is stored in my Github Repo.
#'
#' TB = ABS Table Builder
#'
#' @param path (character) url for where the TB data is stored
#' @param MB_pop (tibble) Meshblock Population. By Default it will draw this from the ABS
#' website using an internal function in this package.
#' @param MB_allocation_file (tibble) ASGS Allocation data. By Default it will draw this from the ABS
#' website using an internal function in this package.
#' @param summarise_cols (character vector) This is the columns from the main TB data set you
#' want to summarise.
#' @param POA_data (tibble) POA Allocation Data. By Default it will draw this from the ABS
#' website using an internal function in this package.
#' @param CED_data (tibble) CED Allocation Data. By Default it will draw this from the ABS
#' website using an internal function in this package.
#'
#' @return (tibble) Returns TB data summarised by CED
#' @export
#'
#' @examples
concord_ABS_TB_LFS_CED <- function(path = "https://raw.githubusercontent.com/nikhilchandra-stats/macrodatasetsraw/refs/heads/master/data/SA1_LFS.csv",
                                MB_pop = get_ABS_MB_pop(),
                                POA_data =
                                  get_abs_region_allocation(geo_type = "POA") ,
                                CED_data =
                                  get_abs_region_allocation(geo_type = "CED"),
                                MB_allocation_file = get_abs_region_allocation(),
                                summarise_cols =
                                  c("Dwelling", "Person", "Employed, worked full-time",
                                    "Employed, worked part-time", "Employed, away from work",
                                    "Unemployed, looking for full-time work", "Unemployed, looking for part-time work",
                                    "Not in the labour force" , "Not stated" ,"Not applicable"  )
) {

  MB_pop_with_SA1 <-
    MB_pop %>%
    dplyr::left_join(
      MB_allocation_file %>% dplyr::distinct(MB_CODE_2021, SA1_CODE_2021)
    )

  MB_POA_CED_joined_SA1 <- MB_pop_with_SA1 %>%
    dplyr::left_join(POA_data) %>%
    dplyr::left_join(CED_data) %>%
    dplyr::filter(!is.na(POA_CODE_2021)) %>%
    dplyr::filter(!is.na(CED_NAME_2021))

  dat_temp <-  readr::read_csv(file = path, skip = 9)
  dat_temp2 <- dat_temp[-1,]
  dat_temp3 <- dat_temp2[,-c(length(dat_temp2), length(dat_temp2) - 1 )]
  dat_temp4 <- dat_temp3 %>%
    dplyr::rename(SA1_CODE_2021 = 1)

  MB_POA_CED_joined_SA1_summed <- MB_POA_CED_joined_SA1  %>%
    dplyr::group_by(SA1_CODE_2021, POA_CODE_2021, CED_CODE_2021, CED_NAME_2021, STATE_NAME_2021) %>%
    dplyr::summarise(Person = sum(Person, na.rm = T),
              Dwelling = sum(Dwelling, na.rm = T),
              AREA_ALBERS_SQKM = sum(Person, na.rm = T))

  dat_temp4 <- MB_POA_CED_joined_SA1_summed %>%
    dplyr::left_join(dat_temp4) %>%
    dplyr::group_by(CED_CODE_2021, CED_NAME_2021, SA1_CODE_2021, STATE_NAME_2021) %>%
    dplyr::summarise(
      dplyr::across(.cols = tidyselect::matches(summarise_cols, ignore.case = FALSE),
             .fns = ~ sum(., na.rm = T)
      )
    )

  dat_temp5 <- dat_temp4 %>%
    dplyr::group_by( SA1_CODE_2021) %>%
    dplyr::mutate(SA1_count = dplyr::n_distinct(CED_NAME_2021)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(CED_NAME_2021) %>%
    dplyr::summarise(
      dplyr::across(.cols = tidyselect::matches(summarise_cols, ignore.case = FALSE),
             .fns = ~ sum(., na.rm = T)
      )
    )

  names(dat_temp5)[-1] <- paste0("CED - ", names(dat_temp5)[-1])

  return(dat_temp5)

}

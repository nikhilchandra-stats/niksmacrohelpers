abs_regions <- get_abs_region_allocation(geo_type = "POA")
distinct_poas <- abs_regions %>%
  distinct(POA_CODE_2021, POA_NAME_2021)


dat_list <- list()

safely_scrape <- purrr::safely(get_abs_quick_stats, otherwise = NULL)

for (i in 2097:dim(distinct_poas)[1]) {

  wait_time <- runif(n = 1, min = 0.7, 3.3)

  Sys.sleep(wait_time)

  temp_dat <- safely_scrape(year = 2021,
                            geography = "POA",
                            geo_code = distinct_poas$POA_CODE_2021[i] %>% as.character()) %>%
    pluck(1)

  if(class(temp_dat) != "NULL") {

    file_name <- glue::glue("C:/Users/Nikhil Chandra/Documents/Repos/abs_raw_data_files/quick_stats_2021_poa_{distinct_poas$POA_CODE_2021[i]}.csv")

    write.csv(file = file_name, x =  temp_dat)

  }

}


files_in_drive <- fs::dir_info("C:/Users/Nikhil Chandra/Documents/Repos/abs_raw_data_files/") %>%
  pull(path)

rm(abs_regions)
gc()

comp_set <- list()

for (i in 1:length(files_in_drive)) {

comp_set[[i]] <- read_csv(files_in_drive[i])

}

test <- comp_set[[1]]

comp_set_2 <- comp_set %>%
  purrr::map_dfr(~ .x %>% select(-1) %>%
                   mutate(value = as.numeric(value),
                          geo_code = as.character(geo_code)))

write.csv(comp_set_2,
          file = "C:/Users/Nikhil Chandra/Documents/Repos/abs_raw_data_files/abs_data_by_region_poa_complete.csv")

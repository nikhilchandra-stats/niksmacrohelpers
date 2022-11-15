library(tidyverse)
library(lubridate)
library(rvest)
library(RSelenium)

scrape_dt <- lubridate::now(tzone = "Australia/Sydney")

#----------------------Detects if you are on pipeline or VM
local_testing <- TRUE


# If on VM run local selenium driver using firefox
if(local_testing){

  driver <- RSelenium::rsDriver(browser = "firefox", port = 4565L )

  remote_driver <- driver[["client"]]

  remote_driver$open()


}

#Xpaths
all_houses <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[5]/g[1]/path[1]"
three_bed_house <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[5]/g[3]/path[1]"
all_units <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[5]/g[5]/path[1]"
combined <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[5]/g[9]/path[1]"

get_sqm_chart_data <- function(
  page_source = remote_driver$getPageSource(),
  xpath_chart = "/html/body/div/div/div/div/div/div[1]/div/svg/g[5]/g[1]/path[1]"){

  chart_values <-
    page_source %>%
    pluck(1) %>%
    xml2::read_html() %>%
    rvest::html_element(xpath = xpath_chart) %>%
    as.character() %>%
    stringr::str_extract_all(pattern = "[0-9]+.[0-9]+ [0-9]+.[0-9]+|[0-9]+ [0-9]+.[0-9]+|[0-9]+.[0-9]+ [0-9]+") %>%
    pluck(1)

  dat <- tibble(
    x = chart_values
  ) %>%
    separate(col = x, into = c("x","y"), sep = " ") %>%
    mutate(across(.fns = as.numeric))

  return(dat)

}

#AXIS sketching
translate_sqm_axis_x <- function(
  page_source = remote_driver$getPageSource(),
  xpath_x = "/html/body/div/div/div/div/div/div[1]/div/svg/g[7]"
) {

  axis_x_points <- page_source %>%
    pluck(1) %>%
    xml2::read_html() %>%
    rvest::html_element(xpath = xpath_x) %>%
    rvest::html_children() %>%
    map(~ rvest::html_text2(.x))

  axis_x_points_values <- page_source %>%
    pluck(1) %>%
    xml2::read_html() %>%
    rvest::html_element(xpath = xpath_x) %>%
    rvest::html_children() %>% rvest::html_attrs() %>%
    map_dfr( ~ tibble(x = .x["x"], y = .x["y"])) %>%
    mutate(axis_labels = axis_x_points) %>%
    mutate(axis_labels = as.numeric(axis_labels)) %>%
    arrange(axis_labels) %>%
    mutate(x = as.numeric(x), y = as.numeric(y)) %>%
    mutate(x_increment = 365/c(NA, diff(x))) %>%
    fill(x_increment, .direction = "up")

  return(axis_x_points_values)

}

translate_sqm_axis_y <- function(
  page_source = remote_driver$getPageSource(),
  xpath_y = "/html/body/div/div/div/div/div/div[1]/div/svg/g[8]"
) {

  axis_y_points <- page_source %>%
    pluck(1) %>%
    xml2::read_html() %>%
    rvest::html_element(xpath = xpath_y) %>%
    rvest::html_children() %>%
    map(~ rvest::html_text2(.x)) %>%
    unlist()

  axis_y_points_values <- page_source %>%
    pluck(1) %>%
    xml2::read_html() %>%
    rvest::html_element(xpath = xpath_y) %>%
    rvest::html_children() %>% rvest::html_attrs() %>%
    map_dfr( ~ tibble(x = .x["x"], y = .x["y"])) %>%
    mutate(x = as.numeric(x), y = as.numeric(y)) %>%
    arrange((y)) %>%
    mutate(axis_labels = axis_y_points) %>%
    mutate(axis_labels = as.numeric(axis_labels)) %>%
    arrange(axis_labels) %>%
    mutate(y_axis_diff = c(NA, diff(y)))%>%
    mutate(label_axis_diff = c(NA, diff(axis_labels))) %>%
    mutate(y_increment = label_axis_diff/y_axis_diff) %>%
    select(-y_axis_diff, -label_axis_diff) %>%
    fill(y_increment, .direction = "up")
  #
  # axis_y_points_values_jj <- axis_y_points_values %>%
  #   arrange(desc(axis_labels)) %>%
  #   pull(axis_labels)
  #
  # axis_y_points_values <- axis_y_points_values %>%
  #   mutate(axis_labels = axis_y_points_values_jj)

  return(axis_y_points_values)

}

extract_chart_data_sqm <- function(
  page_source = remote_driver$getPageSource(),
  xpath_chart = all_houses,
  xpath_y = "/html/body/div/div/div/div/div/div[1]/div/svg/g[8]",
  start_date = "2005-01-01",
  time_step = NULL,
  filter_friday = FALSE){

  chart_dat <- get_sqm_chart_data(page_source = page_source, xpath_chart = xpath_chart) %>%
    mutate(y = as.numeric(y), x = as.numeric(x))

  y_axis_dat <- translate_sqm_axis_y(page_source, xpath_y = xpath_y)

  min_y <- y_axis_dat$y %>% min()
  max_y <- y_axis_dat$y %>% max()

  max_x <- chart_dat$x %>% max()
  min_x <- chart_dat$x %>% min()

  chart_dat <- chart_dat %>%
    mutate(y = (max_y - y) - min_y)

  y_axis_dat <- y_axis_dat %>%
    mutate(y = y - min_y)

  find_friday <- seq(lubridate::today() - lubridate::days(30), lubridate::today(), "day") %>%
    as_tibble() %>%
    mutate(friday = lubridate::wday(x = value, label = T)) %>%
    filter(friday == "Fri") %>%
    pull(value) %>%
    max() %>%
    as_date()

  if(!is.null(time_step)){

    firday_seq <- seq(lubridate::as_date(start_date), find_friday, time_step) %>%
      as_tibble()
  }

  if(is.null(time_step)){

    firday_seq <- seq(lubridate::as_date(start_date), find_friday, length.out = dim(chart_dat)[1]) %>%
      as_tibble()

  }

  if(filter_friday == TRUE){

    firday_seq <- firday_seq %>%
      mutate(friday = lubridate::wday(x = value, label = T)) %>%
      filter(friday == "Fri")

    firday_seq <- firday_seq  %>%
      arrange(desc(value)) %>%
      slice_head(n = dim(chart_dat)[1]) %>%
      arrange(value) %>%
      pull(value)

  }else{

    firday_seq <- firday_seq  %>%
      arrange(desc(value)) %>%
      slice_head(n = dim(chart_dat)[1]) %>%
      arrange(value) %>%
      pull(value)

  }

  for (i in 2:dim(y_axis_dat)[1]) {

    if(i == 2){
      chart_dat_translated <- chart_dat %>%
        mutate(
          translated_y =
            case_when(
              .data$y <= y_axis_dat$y[i] &  .data$y >= y_axis_dat$y[i - 1] ~
                y_axis_dat$axis_labels[i - 1] + (.data$y - y_axis_dat$y[i - 1])*y_axis_dat$y_increment[i - 1]
            )
        )
    }else{
      chart_dat_translated <- chart_dat_translated %>%
        mutate(
          translated_y =
            case_when(
              .data$y <= y_axis_dat$y[i] &  .data$y >= y_axis_dat$y[i - 1] ~
                y_axis_dat$axis_labels[i - 1] + (.data$y - y_axis_dat$y[i - 1])*y_axis_dat$y_increment[i - 1],
              TRUE ~ .data$translated_y
            )
        )
    }
  }

  chart_dat_translated <- chart_dat_translated %>%
    arrange(x) %>%
    mutate(
      translated_x = firday_seq
    )

  return(chart_dat_translated)

}


all_post_codes <- readxl::read_excel("data/post_codes/POA_2021_AUST.xlsx") %>%
  distinct(POA_CODE_2021) %>%
  pull(POA_CODE_2021)

for (i in 14:length(all_post_codes)) {

  postcode = all_post_codes[i]

  random_wait_time <- runif(n = 1, min = 6, max = 20) %>% round()

  Sys.sleep(random_wait_time)

  url <- glue::glue("https://sqmresearch.com.au/weekly-rents.php?postcode={postcode}&t=1") %>% as.character()

  remote_driver$navigate(url)

  safely_extract <- safely(extract_chart_data, otherwise = NULL)

  dat <-
    safely_extract(
      page_source = remote_driver$getPageSource(),
      xpath_chart = combined
    ) %>%
    pluck(1)

  if(any(class(dat) == "tbl_df") & !is.null(dat)){

    dat <- dat %>%
      mutate(
        poa_code = postcode
      )

    write.csv(dat,glue::glue("data/rental_data/rent_ts_{postcode}.csv"))

  }


  random_wait_time <- runif(n = 1, min = 3, max = 20) %>% round()

  Sys.sleep(random_wait_time)

}

try(remote_driver$quit())





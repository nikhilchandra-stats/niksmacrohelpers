extract_latest_news <- function() {




}

library(tidyverse)
library(lubridate)
library(rvest)
library(RSelenium)

helpeR::load_custom_functions()

scrape_dt <- lubridate::now(tzone = "Australia/Sydney")

#----------------------Detects if you are on pipeline or VM
local_testing <- TRUE


# If on VM run local selenium driver using firefox
if(local_testing){

  driver <- rsDriver(browser = c("firefox"),
                     port = 4570L, chromever = "108.0.5359.71")

  remote_driver <- driver[["client"]]

  remote_driver$open()

  driver$server$output()


}


google_search_news <- function() {

}

#----------------------Go to google and search
search_text <- "PWC Australia"

remote_driver$navigate(url = "https://www.google.com")

input_field <- remote_driver$findElement(using = "xpath", value = "/html/body/div[1]/div[3]/form/div[1]/div[1]/div[1]/div/div[2]/input")
input_field$sendKeysToElement(list(search_text, key = "enter"))

# click_field_search <- remote_driver$findElement(using = "xpath", value = "/html/body/div[1]/div[3]/form/div[1]/div[1]/div[4]/center/input[1]")
# click_field_search$clickElement()

click_field_news <- remote_driver$findElement(using = "xpath", value = "/html/body/div[7]/div/div[4]/div/div[1]/div/div[1]/div/div[2]/a")
click_field_news$clickElement()

page_sourced <- remote_driver$getPageSource()
#------------------------------------------------------------------------------------------------------------------------------

# Extraction

xpath_for_news_block <- "/html/body/div[7]/div/div[10]/div/div[2]/div[2]/div/div/div/div/div[xxxxxx]/div/div/a"
xpath_for_news_block <- stringr::str_replace(string = xpath_for_news_block, pattern = "xxxxxx", replacement = "4")


google_news_page_extraction <- function(xpath_for_news_block,
                                        search_value = search_text) {

  go_to_news_link_xpath <- glue::glue("{xpath_for_news_block}/div/div[2]/div[2]") %>% as.character()

  data_x <- page_sourced %>%
    unlist() %>%
    rvest::read_html() %>%
    rvest::html_element(xpath = xpath_for_news_block)

  link_element <- data_x %>%
    rvest::html_elements("div") %>%
    rvest::html_text()

  attribute_links <- data_x %>%
    rvest::html_attrs() %>%
    purrr::pluck("href") %>%
    stringr::str_replace("^[^_]*(www)", replacement = "")

  attribute_links <- paste0("www",attribute_links)

  extracted_text <- link_element %>%
    paste(collapse = " ")

  returned_data <-
    dplyr::tibble(
      text_data = extracted_text,
      hyperlink = attribute_links,

    )

}


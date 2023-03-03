library(tidyverse)
library(lubridate)
library(rvest)
library(RSelenium)

# You can install this from my github
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

# ---- Define the base function to loop over
#' This function will take in a Rselenium object and click through, provide
#' inputs and then works its way to the electricity prices and scrapes those
#' values. The function takes in a post code and subsequently gets the proces
#' for that.
#'
#' @param remote_driver (Rselenium object) Feed this function your remote driver
#' object once selenium starts.
#' @param url_x (character) The compare the market URL
#' @param string_postcode (character; "2600") Postcode
#' @param phone_number (character) Must be of the form "xxxx xxx xxx". You cant
#' get the prices without giving it details.
#' @param email_address (character) email address. You cant
#' get the prices without giving it details.
#' @param your_name (character) You cant get the prices without giving it
#' details. Just give it some bogus name.
#'
#' @return (tibble)
#' @export
#'
#' @examples
nav_get_price_comp_mark <- function(remote_driver,
                                    url_x = "https://www.comparethemarket.com.au/energy/journey/start?utility_compareto=E&utility_movingin=Y",
                                    string_postcode = "2600",
                                    phone_number,
                                    email_address,
                                    your_name
                                    ) {

  remote_driver$navigate(url = url_x)

  #Pick Electricity
  electricity_pick <- remote_driver$findElement(
    using = "xpath",
    value = "/html/body/div[1]/div/div/section/div/div/main/div/form/div[1]/div[2]/div[2]/div[1]/label/label/div/span/input"
  )
  electricity_pick$clickElement()

  # Pick Residential
  residential_pick <- remote_driver$findElement(
    using = "xpath",
    value = "/html/body/div[1]/div/div/section/div/div/main/div/form/div[1]/div[3]/div[2]/div[1]/label"
  )

  residential_pick$clickElement()

  # Pick Residential 2
  residential_pick <- remote_driver$findElement(
    using = "xpath",
    value = "/html/body/div[1]/div/div/section/div/div/main/div/form/div[1]/div[4]/div[2]/div[1]/label/label/div/span/input"
  )

  residential_pick$clickElement()

  #Put post code into the field box
  input_field <- remote_driver$findElement(using = "xpath", value = "/html/body/div[1]/div/div/section/div/div/main/div/form/div[1]/div[5]/div[2]/div/input")
  input_field$sendKeysToElement(list(string_postcode))


  # Sleep with Uniform Distribution
  Sys.sleep(runif(n = 1, 1,2))

  # Choose the first one that appears in the drop down list
  input_field_selector <- remote_driver$findElement(using = "xpath",
                                                    value = "/html/body/div[1]/div/div/section/div/div/main/div/form/div[1]/div[5]/div[2]/div/div/ul/li[1]")
  input_field_selector$clickElement()

  Sys.sleep(runif(n = 1, 1,2))

  # Click Check Box
  click_field_check_box <- remote_driver$findElement(using = "xpath", value = "/html/body/div[1]/div/div/section/div/div/main/div/form/div[2]/div/label/div/span")
  click_field_check_box$clickElement()

  # Go to next Page
  next_page_box <- remote_driver$findElement(using = "xpath", value = "/html/body/div[1]/div/div/section/div/div/main/div/div/div/button")
  next_page_box$clickElement()
  Sys.sleep(runif(n = 1, 1,2))

  # Solar Panels = No
  solar_power_box <- remote_driver$findElement(using = "xpath",
                                               value = "/html/body/div[1]/div/div/section/div/div/main/div/form/div[2]/div[1]/div[2]/div[2]/label")
  solar_power_box$clickElement()
  Sys.sleep(runif(n = 1, 1,2))

  # Extract out average usage
  page_sourced <- remote_driver$getPageSource()
  usage_dat <- rvest::read_html(page_sourced[[1]]) %>%
    rvest::html_element(xpath = "/html/body/div[1]/div/div/section/div/div/main/div/form/div[2]/div[3]/div/div/span") %>%
    rvest::html_text()

  Sys.sleep(runif(n = 1, 1,2))

  #Next Page
  next_page_click <- remote_driver$findElement(using = "xpath",
                                               value = "/html/body/div[1]/div/div/section/div/div/main/div/div/div[2]/button")
  next_page_click$clickElement()

  Sys.sleep(runif(n = 1, 1,2))

  # Give personal Details
  input_field <- remote_driver$findElement(using = "xpath", value = "/html/body/div[1]/div/div/section/div/div/main/div[1]/form/div/div[1]/input")
  input_field$sendKeysToElement(list(your_name))
  Sys.sleep(runif(n = 1, 1,2))

  input_field <- remote_driver$findElement(using = "xpath", value = "/html/body/div[1]/div/div/section/div/div/main/div[1]/form/div/div[2]/input")
  input_field$sendKeysToElement(list(email_address))
  Sys.sleep(runif(n = 1, 1,2))

  input_field <- remote_driver$findElement(using = "xpath", value = "/html/body/div[1]/div/div/section/div/div/main/div[1]/form/div/div[3]/input")
  input_field$sendKeysToElement(list(phone_number))
  Sys.sleep(runif(n = 1, 1,2))

  remote_driver$findElement(using = "xpath", value = "/html/body/div[1]/div/div/section/div/div/main/div[1]/div/div[2]/button")$clickElement()

  #-----------------------Reached Page
  Sys.sleep(runif(n = 1, 1,2))

  page_sourced <- remote_driver$getPageSource()

  html_read <- page_sourced[[1]] %>%
    rvest::read_html()

  price_div_1 <- html_read %>%
    rvest::html_element(xpath = "/html/body/div[1]/div/div/section/div/main/div[4]/div[1]")

  price_div_2 <- html_read %>%
    rvest::html_element(xpath = "/html/body/div[1]/div/div/section/div/main/div[4]/div[2]")

  price_div_3 <- html_read %>%
    rvest::html_element(xpath = "/html/body/div[1]/div/div/section/div/main/div[4]/div[3]")

  price_div_4 <- html_read %>%
    rvest::html_element(xpath = "/html/body/div[1]/div/div/section/div/main/div[4]/div[4]")


  prices1 <-  price_div_1 %>%
    rvest::html_element(xpath = "/html/body/div[1]/div/div/section/div/main/div[4]/div[1]/div/div[3]/div/div[1]/div[3]/div[1]/div/div/div/span") %>%
    rvest::html_text()

  prices2 <-  price_div_2 %>%
    rvest::html_element(xpath = "/html/body/div[1]/div/div/section/div/main/div[4]/div[2]/div/div[3]/div/div[1]/div[3]/div[1]/div/div/div/span") %>%
    rvest::html_text()

  prices3 <-  price_div_3 %>%
    rvest::html_element(xpath = "/html/body/div[1]/div/div/section/div/main/div[4]/div[3]/div/div[3]/div/div[1]/div[3]/div[1]/div/div/div/span") %>%
    rvest::html_text()

  prices4 <-  price_div_4 %>%
    rvest::html_element(xpath = "/html/body/div[1]/div/div/section/div/main/div[4]/div[4]/div/div[3]/div/div[1]/div[3]/div[1]/div/div/div/span") %>%
    rvest::html_text()

  tibble_prices <-
    tibble(
      full_text =
        c(price_div_1 %>% rvest::html_text(),
          price_div_2 %>% rvest::html_text(),
          price_div_3 %>% rvest::html_text(),
          price_div_4 %>% rvest::html_text()
        ),
      prices =
        c(prices1 %>% rvest::html_text(),
          prices2 %>% rvest::html_text(),
          prices3 %>% rvest::html_text(),
          prices4 %>% rvest::html_text()
        )
    )

  return(tibble_prices)

}




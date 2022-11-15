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

url <- "https://www.investing.com/pro/ASX:CBA/financials/income_statement/"

remote_driver$navigate(url)

download_xpath <- "/html/body/div[3]/div/div[4]/div[3]/div[1]/div/div[2]/button/span"

download_button1 <- remote_driver$findElement( value = download_xpath)
download_button1$mouseMoveToLocation(webElement = download_button1)
remote_driver$click(buttonId = 1)

download_xpath2 <- "/html/body/div[3]/div/div[4]/div[3]/div[1]/div/div[2]/div[2]/button[3]"

download_button2 <- remote_driver$findElement( value = download_xpath2)
download_button2$mouseMoveToLocation(webElement = download_button2)
remote_driver$click(buttonId = 1)

download_xpath3 <- "/html/body/div[10]/div/div/button[1]"

download_button3 <- remote_driver$findElement( value = download_xpath3)
download_button3$mouseMoveToLocation(webElement = download_button3)
remote_driver$click(buttonId = 1)


html_source <-remote_driver$getPageSource()

tables <- html_source %>%
  pluck(1) %>%
  xml2::read_html() %>%
  rvest::html_text()

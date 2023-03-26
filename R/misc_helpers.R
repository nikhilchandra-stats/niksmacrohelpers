#' Return a vector of days of the week
#'
#' @param abbrev (Boolean; FALSE) Abbreviates the days if set to TRUE
#'
#' @return (character) The function will return a vector of the days of the week
#' @export
#'
#' @examples \dontrun{
#' days_week <- days_of_week()
#' }
days_of_week <- function(abbrev = FALSE){
  ifelse(
    abbrev,
    c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  )
}

#' Return a vector of months of the year
#'
#' @param abbrev (Boolean; FALSE) Abbreviates the months if set to TRUE
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' days_week <- days_of_week()
#' }
months_of_year <- function(abbrev = FALSE){

  if(abbrev == T){
    return(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  }else{
    return(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  }


}

#' Detect a string and converts that string from the month abbreviation into
#' a number
#'
#' @param .vec (character) Input character vector
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#'
#' strings <- c("I am January", "You 03 February test TEst")
#'
#' extracted_month <- convert_month(strings)
#'
#' }
convert_month <- function(.vec){

  .vec %>%
    tolower() %>%
    as_tibble() %>%
    mutate(
      number =
        case_when(
          str_detect(value, "jan")|str_detect(value, "Jan") ~ "01",
          str_detect(value, "feb")|str_detect(value, "Feb") ~ "02",
          str_detect(value, "mar")|str_detect(value, "Mar") ~ "03",
          str_detect(value, "apr")|str_detect(value, "Apr") ~ "04",
          str_detect(value, "may")|str_detect(value, "May") ~ "05",
          str_detect(value, "jun")|str_detect(value, "Jun") ~ "06",
          str_detect(value, "jul")|str_detect(value, "Jul") ~ "07",
          str_detect(value, "aug")|str_detect(value, "Aug") ~ "08",
          str_detect(value, "sep")|str_detect(value, "Sep") ~ "09",
          str_detect(value, "oct")|str_detect(value, "Oct") ~ "10",
          str_detect(value, "nov")|str_detect(value, "Nov") ~ "11",
          str_detect(value, "dec")|str_detect(value, "Dec") ~ "12"
        )
    ) %>%
    pull(number)

}

#' Download and read in xlsx file
#'
#' @param url (character) The download URL for the xlsx file
#'
#' @return (tibble)
#' @export
#'
#' @examples \dontrun{
#' url <- "https://data.gov.au/data/dataset/863c394c-a26c-4340-896b-a26b18af476d/resource/66ee77e9-34b0-4215-8bc3-bfa43b7c8f11/download/2018-19-pbs-program-expense-line-items-1.xlsx"
#' data <- download_xlsx(url)
#' }
download_read_xlsx <- function(url) {

  dest <- paste0(tempdir(), "\\temp.XLSX")
  httr::GET(url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
  readxl::read_excel(tf)

}


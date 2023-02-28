get_pwc_management_helper <- function(url_var = "https://www.pwc.com.au/about-us/pwc-board-of-partners.html",
                                      xpath_var = "/html/body/div/div/div/div[2]/div/div[6]/div/div[2]/section/div/div/div/div/div/div[1]/div/div/div/div[1]") {

  page_read <- rvest::read_html(url_var) %>%
    rvest::html_elements(xpath =  xpath_var) %>%
    rvest::html_text() %>%
    stringr::str_split(pattern = "\r\n", simplify = T) %>%
    purrr::keep(~ stringr::str_detect(.x, "[a-z]+")) %>%
    purrr::map(
      ~ stringr::str_trim(.x)
    ) %>%
    unlist() %>%
    purrr::map(
      ~ stringr::str_split(.x, " ")
    ) %>%
    unlist()


  details <-
    dplyr::tibble(
          company = "PWC",
          first_name = page_read[1],
           last_name = page_read[2],
           role = page_read[3]
          )

  return(details)

}

get_pwc_management <- function(
    url_var = "https://www.pwc.com.au/about-us/pwc-board-of-partners.html") {


  xpaths <- "/html/body/div/div/div/div[2]/div/div[6]/div/div[2]/section/div/div/div/div/div/div[1]/div/div/div/div[xxxxxxxx]"

  xpaths_vec <- c()

  for (i in 1:15) {
    xpaths_vec[i] <- stringr::str_replace(xpaths, pattern = "xxxxxxxx", as.character(i))
  }

  safely_extract <- purrr::safely(get_pwc_management_helper, otherwise = NULL)

  extracted_data <- xpaths_vec %>%
    purrr::map(
      ~ safely_extract(url_var = url_var, xpath_var = .x) %>%
        purrr::pluck('result')
    ) %>%
    purrr::keep( ~ !is.null(.x)) %>%
    purrr::keep( ~ any(class(.x) == "data.frame"))  %>%
    purrr::keep(~ ncol(.x) > 2) %>%
    purrr::map_dfr(bind_rows)

}



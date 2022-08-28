china_chart <- function(){

  gdp_dat <- get_oecd_GDP_local()

  china_gdp <- gdp_dat %>%
    dplyr::filter(LOCATION == "CHN" ) %>%
    dplyr::mutate(Country = "China") %>%
    dplyr::select(-LOCATION, -country) %>%
    dplyr::filter(measure_name == "Growth rate based on seasonally adjusted volume data, percentage change on the previous quarter")

  trade_monthly <- get_trade_DFAT_monthly()

  trade_yearly <- get_trade_DFAT_yearly()

  trade_monthly <- trade_monthly %>%
    dplyr::filter(Country == "China")

  trade_yearly <- trade_yearly %>%
    dplyr::filter(Country == "China")


  macro_event_data <- get_macro_event_data() %>%
    dplyr::filter(symbol == "CNY") %>%
    dplyr::mutate(Country = "China")

  trade_yearly2 <- trade_yearly %>%
    dplyr::filter(stringr::str_detect(Type, "Iron ore & concentrates")) %>%
    dplyr::mutate(average_monthly = Value/12) %>%
    dplyr::select(Type, period = Year, Value, average_monthly) %>%
    dplyr::filter(period <= 2018) %>%
    dplyr::filter(period > 2006) %>%
    dplyr::mutate(period = as.character(period)) %>%
    dplyr::mutate(period_as_date = glue::glue("{period}-12-01") %>% as.character() )%>%
    mutate(period_as_date = as.Date(period_as_date, "%Y-%m-%d"))

  trade_monthly2 <- trade_monthly %>%
    dplyr::filter(stringr::str_detect(Type, "Iron ore & concentrates")) %>%
    dplyr::mutate(average_monthly = Values) %>%
    dplyr::select(Type, period = Date, Value  = Values, average_monthly) %>%
    dplyr::mutate(period_as_date = glue::glue("01-{period}") %>% as.character()) %>%
    dplyr::mutate(period_as_date = as.Date(period_as_date, "%d-%b-%Y"))

  trade_data <- trade_yearly2 %>%
    dplyr::bind_rows(trade_yearly2, trade_monthly2)

  # trade_data %>%
  #   arrange(period_as_date) %>%
  #   mutate(id = dplyr::row_number()) %>%
  #   ggplot(aes(x = period_as_date, y = average_monthly)) +
  #   geom_col() +
  #   theme_minimal() +
  #   theme(axis.text.x = element_text(angle = 45))

}

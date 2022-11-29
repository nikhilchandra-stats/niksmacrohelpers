
# niksmacrohelpers

<!-- badges: start -->
<!-- badges: end -->

The goal of niksmacrohelpers is to ...

## Installation

You can install the development version of niksmacrohelpers from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nikhilchandra-stats/niksmacrohelpers")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(niksmacrohelpers)

####OCED DATA
available_indicators <- get_oecd_avlble_indctors_DPLIVE()

GDP_based_indicators <- available_indicators %>%
    filter(str_detect(indicator_desc, "GDP|gdp")) %>%
    pull(INDICATOR)

GDP_based_indicators    
    
GDP <- get_oecd_data_DP_LIVE(
   indicators = GDP_based_indicators,
    countries = c("USA", "AUS", "JPN", "GBR"),
    start_time = 2020,
    end_time = 2021
    )
    
```


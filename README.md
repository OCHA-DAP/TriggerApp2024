
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TriggerApp2024

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of TriggerApp2024 is to …

## Installation

You can install the development version of TriggerApp2024 like so:

``` r
devtools::install(getwd())
library(TriggerApp2024)
ldf <- load_df_forecast(dataset = "mars_eth")
#TriggerApp2024::run_app()

source("R/mod_historical_process_simp_test.R")
source("test2.R")
run_app()

```
``` r
# Define the function to source all R scripts in a directory
source_all_scripts <- function(directory) {
  # Get a list of all R files in the directory
  r_files <- list.files(directory, pattern = "\\.R$", full.names = TRUE)
  
  # Source each file
  sapply(r_files, source, .GlobalEnv)
}

# Source all R scripts in the 'R' directory
source_all_scripts("R")
TriggerApp2024::run_app()
```
## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(TriggerApp2024)
## basic example code
|> dplyr::bind_rows(data.frame(adm0_en = "Aggregate",
                                    overall_activation = sum(joint_ar$overall_activation, na.rm = T),
                                    overall_rp = 1 / sum(joint_ar$overall_activation, na.rm = T)))
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

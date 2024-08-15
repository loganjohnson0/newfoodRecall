
<!-- README.md is generated from README.Rmd. Please edit that file -->

# foodRecall

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/loganjohnson0/newfoodRecall/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/loganjohnson0/newfoodRecall/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Installation

You can install the development version of foodRecall from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("loganjohnson0/foodRecall")
```

## Loading Package

``` r
library(foodRecall)
```

## Introduction

To use this package, you must register for an API key through the
[openFDA website](https://open.fda.gov/apis/authentication/). This is a
free API key that only requires your email address. You should receive
it immediately upon request. Upon any issues with the API key itself,
please contact the openFDA office. Be sure to not share your API key
with anyone!

<b>This product uses the openFDA API but is not endorsed or certified by
the Food and Drug Administration.</b>

## Save Your API Key

``` r
api_key <- "YOUR API KEY"
```

We have developed a few different functions for you to search for
different kinds of data. You can use either `recall_location` or
`recall_date` to search for data on food recalls. See notation below as
an example.

``` r
location <- foodRecall::recall_location(api_key = api_key, 
                                        city = "Ames", 
                                        state = "Iowa")

date <- foodRecall::recall_date(api_key = api_key,
                                report_date = "January 2023 to May 2023")
```

You can also map the resulting data to see the location of the recall
events, the impact of recall, and the number of recall events and
individual products each recalling firm has recalled.

``` r
foodRecall::map_recall(data = df)
```

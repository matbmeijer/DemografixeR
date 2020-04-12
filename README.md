
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DemografixeR

<!-- badges: start -->

<!-- badges: end -->

DemografixeR is an API wrapper of all 3 Demografix API’s - all in one:

  - <https://genderize.io/>
  - <https://agify.io/>
  - <https://nationalize.io/>

## Installation

You can install the released version of DemografixeR from
[CRAN](https://CRAN.R-project.org) with:

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("matbmeijer/DemografixeR")
```

## Example

This is a basic example which shows you how to estimate nationality,
gender and age by a given name:

``` r
library(DemografixeR)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(knitr)

df<-data.frame(names=c("Ana", NA, "Pedro",
                       "Francisco", "Maria", "Elena"),
                 country=c(NA, NA, "ES",
                           "DE", "ES", "NL"), stringsAsFactors = FALSE)

df %>% mutate(guessed_nationality=nationalize(name = names),
                guessed_gender=genderize(name = names, country_id = country),
                guessed_age=agify(name = names, country_id = country)) %>% 
  kable()
```

| names     | country | guessed\_nationality | guessed\_gender | guessed\_age |
| :-------- | :------ | :------------------- | :-------------- | -----------: |
| Ana       | NA      | PT                   | female          |           58 |
| NA        | NA      | NA                   | NA              |           NA |
| Pedro     | ES      | PT                   | male            |           69 |
| Francisco | DE      | CL                   | male            |           58 |
| Maria     | ES      | CY                   | NA              |           59 |
| Elena     | NL      | CC                   | female          |           69 |
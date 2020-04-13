
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DemografixeR

<!-- badges: start -->

<!-- badges: end -->

DemografixeR is an API wrapper of all 3 Demografix API’s - all in one:

  - <https://genderize.io/> - **Gender estimation** based on a name
  - <https://agify.io/> - **Age estimation** based on a name
  - <https://nationalize.io/> - **Nationality estimation** based on a
    name

## Documentation

You can find all the necessary documentation here:
<https://matbmeijer.github.io/DemografixeR/>

## Installation

You can install the released version of DemografixeR from
[CRAN](https://CRAN.R-project.org) with:

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("matbmeijer/DemografixeR")
```

## Examples

These are basic examples which shows you how to estimate
**nationality**, **gender** and **age** by a given name with & without
specifying a country. It also shows that the functions are able to deal
with missing values:

``` r
library(DemografixeR)

#Simple example without country_id
names<-c("Ben", "Allister", "Lucie", "Paula")
genderize(name = names)
#> [1] "male"   "male"   "female" "female"
nationalize(name = names)
#> [1] "AU" "ZA" "CZ" "PT"
agify(name = names)
#> [1] 48 44 24 50

#Simple example with
genderize(name = names, country_id = "US")
#> [1] "male"   "male"   "female" "female"
agify(name = names, country_id = "US")
#> [1] 67 46 65 70

#Workflow example with dplyr with missing values and multiple different countries
df<-data.frame(names=c("Ana", NA, "Pedro",
                       "Francisco", "Maria", "Elena"),
                 country=c(NA, NA, "ES",
                           "DE", "ES", "NL"), stringsAsFactors = FALSE)

df %>% dplyr::mutate(guessed_nationality=nationalize(name = names),
                guessed_gender=genderize(name = names, country_id = country),
                guessed_age=agify(name = names, country_id = country)) %>% 
  knitr::kable()
```

| names     | country | guessed\_nationality | guessed\_gender | guessed\_age |
| :-------- | :------ | :------------------- | :-------------- | -----------: |
| Ana       | NA      | PT                   | female          |           58 |
| NA        | NA      | NA                   | NA              |           NA |
| Pedro     | ES      | PT                   | male            |           69 |
| Francisco | DE      | CL                   | male            |           58 |
| Maria     | ES      | CY                   | NA              |           59 |
| Elena     | NL      | CC                   | female          |           69 |

``` r

#Detailed data.frame example:
genderize(name = names, simplify = FALSE, meta = TRUE) %>% knitr::kable()
```

|   | name     | type   | gender | probability | count | api\_rate\_limit | api\_rate\_remaining | api\_rate\_reset | api\_request\_timestamp |
| - | :------- | :----- | :----- | ----------: | ----: | ---------------: | -------------------: | ---------------: | :---------------------- |
| 2 | Ben      | gender | male   |        0.95 | 77991 |             1000 |                  866 |            44724 | 2020-04-13 11:34:36     |
| 1 | Allister | gender | male   |        0.98 |   129 |             1000 |                  866 |            44724 | 2020-04-13 11:34:36     |
| 3 | Lucie    | gender | female |        0.99 | 85580 |             1000 |                  866 |            44724 | 2020-04-13 11:34:36     |
| 4 | Paula    | gender | female |        0.98 | 74130 |             1000 |                  866 |            44724 | 2020-04-13 11:34:36     |

Please note that the ‘DemografixeR’ project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to
this project, you agree to abide by its terms.

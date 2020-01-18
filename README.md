
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ditiHelper

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

This package is a wrapper around patentsView package to help students
obtain the data in a tidy format.

## Installation

You can install the latest version of ditiHelper package from Github
with:

``` r
# install.packages("remotes")
remotes::install_github("R3myG/ditiHelper")
```

## Examples

### Get data

``` r
query <- patentsview::with_qfuns(
      or(
        text_all(patent_title = "artificial intelligence"),
        text_all(patent_abstract = "artificial intelligence")
      )
  )

all_dat_patents <- get_patents_data(query)
```

### Get inventors informations

``` r
 
get_inventors_df(all_dat_patents)
```

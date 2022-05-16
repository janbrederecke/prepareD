# prepareD

R package with useful functions for data preparation and documentation.

## Overview

### data_ family

-   `overview_data()` Returns an overview table for the input dataset.

-   `check_data()` Returns a table with info on missingness and variable type for the input dataset.

-   `show_na()` Returns overview on missing values for variables of the input.

### find_ family

-   `find_binary()` Returns all binary variables.

-   `find_continouus()` Returns all numeric variables.

### dummies_ family

-   `annotate_dummies()` Returns the annotation with additional dummy variables.

-   `make_dummies()` Returns the input data with additional dummy variables.

## Installation

You can get the development version from [GitHub](https://github.com/) with:

``` r
devtools::install_github("janbrederecke/prepareD")
```

## Contact
Please submit feedback and suggestions through Github's issues facility.

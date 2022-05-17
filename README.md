# prepareD

R package with useful functions for data preparation and documentation.

## Overview

### data_ family

-   `data_overview()` Returns an overview table for the input dataset.

-   `data_check()` Returns a table with info on missingness and variable type
for the input dataset.

-   `data_missing()` Returns overview on missing values for variables of the
input.

### find_ family

-   `find_binary()` Returns all binary variables.

-   `find_continouus()` Returns all numeric variables.

### dummies_ family

-   `dummies_annotate()` Returns the annotation with additional dummy variables.

-   `dummies_make()` Returns the input data with additional dummy variables.

### internal_ family

-   `internal_data()` Returns a dataframe and annotation file for testing
purposes.

## Installation

You can get the development version from [GitHub](https://github.com/) with:

``` r
devtools::install_github("janbrederecke/prepareD")
```

## Contact
Please submit feedback and suggestions through Github's issues facility.

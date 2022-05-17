# prepareD <img src="man/figures/logo_test.jpeg" width="200" height="200" align="right" />

R package with useful functions for data preparation and documentation.

## Overview

### data_ family

-   `data_overview()` Returns an overview table for the input dataset.

-   `data_check()` Returns a table with info on missingness and variable type
for the input dataset.

-   `data_missing()` Returns overview on missing values for variables of the
input.

-   `data_ready()` Returns information on why your data is (not) ready for
analysis.

### find_ family

-   `find_binary()` Returns binary variables from input dataset.

-   `find_continouus()` Returns continouus variables from input dataset.

### dummies_ family

-   `dummies_annotate()` Returns the annotation with additional dummy variables.

-   `dummies_make()` Returns the input data with additional dummy variables.

### comment_ family

-   `comment()` Let's you grow a matrix with comments thoughout your analyis.

### internal_ family

-   `internal_data()` Returns a dataframe and annotation file for testing
purposes.

## Installation

You can get the development version from [GitHub](https://github.com/) with:

``` r
devtools::install_github("janbrederecke/prepareD")
```

## Acknowledgement

The logo was designed by [Tim Brederecke](https://www.instagram.com/timbrederecke/). 

## Contact
Please submit feedback and suggestions through Github's [issues](https://github.com/janbrederecke/prepareD/issues) facility.

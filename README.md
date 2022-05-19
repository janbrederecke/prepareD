# prepareD <img src="man/figures/logo_test.jpeg" width="200" height="200" align="right" />

R package with useful functions for data preparation and documentation.

## Overview

### data_ family
**Functions to quickly return information on the input data** 

-   `data_overview()` Returns an overview table for the input dataset.

-   `data_check()` Returns a table with info on missingness and variable type
for the input dataset.

-   `data_missing()` Returns overview on missing values for variables of the
input.

-   `data_ready()` Returns information on why your data is (not) ready for
analysis.

### find_ family
**Functions to find specific groups of variables in the input data** 

-   `find_binary()` Returns binary variables from input dataset.

-   `find_continouus()` Returns continouus variables from input dataset.

### dummies_ family
**Functions to create dummy variables in the input data and annotation file** 

-   `dummies_annotate()` Returns the annotation with additional dummy variables.

-   `dummies_make()` Returns the input data with additional dummy variables.

### comment_ family
**Functions easily comment your analysis** 

-   `comment()` Let's you grow a matrix with comments throughout your analysis.

### time_ family
**Functions to easily measure time of your analysis** 

-   `time_start()` Creates a starting point for a time measurement.

-   `time_stop()` Calculates the time since `time_start()` was initialized.

### internal_ family
**Internal functions used to test and develop the functions in prepareD** 

-   `internal_data()` Creates a dataframe and an annotation file for testing
purposes in the global environment.

## Installation

You can get the development version from [GitHub](https://github.com/) with:

``` r
devtools::install_github("janbrederecke/prepareD")
```

## Acknowledgement

The logo was designed by [Tim Brederecke](https://www.instagram.com/timbrederecke/). 

## Contact
Please submit feedback and suggestions through Github's [issues](https://github.com/janbrederecke/prepareD/issues) facility.

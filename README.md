# prepareD

R package with useful functions for data preparation and documentation.

## Included functions

**overview_data**:
* Returns an overview table for the input dataset.

**check_data** :
* Returns a table with info on missingness and variable type for the input dataset

**show_na** :
* Returns overview on missing values for variables of the input

**find_binary** :
* Returns either a vector with all clearly binary variables or a list with additional information on variables that might also be binary
* Can automatically recode variables that are identified as containing values like 'yes' / 'no'

**find_continouus** :
* Returns either a vector with all numeric variables with three or more unique values or all numeric variables not in input vector .bin_var

**annotate_dummies** : 
* Returns the annotation with additional dummy variables for specified variables
* Requires a data.frame or matrix object of name 'annotation' in the global environment
* The annotation object must be specified using the column names "name", "pname", "unit", "short_pname", and "comment"

**make_dummies** : 
* Returns the input data with additional dummy variables for specified variables

## Installation

You can get the development version from [GitHub](https://github.com/) with:

``` r
devtools::install_github("janbrederecke/prepareD")
```



Author and maintainer: Jan Brederecke

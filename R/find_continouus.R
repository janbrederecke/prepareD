#' @title find_continouus
#'
#' @description Returns a vector with all numeric variables with three or more
#' unique values
#'
#' @param .data A data.frame or tibble
#' 
#' @return A character vector with the numeric variables
#' @examples -
#' @export

find_continouus <- function(.data
){
  
  # Get all numeric variables
  numeric_variables <- names(.data[, unlist(lapply(.data, is.numeric))])
  
  # Count unique values but NA in variables to be tested
  to_test <- unlist(lapply(.data[, numeric_variables], function(x)
    length(na.omit(unique(
      x
    )))))
  
  # Mark variables with 3 or more unique values as continouus variables
  cont_var <- names(to_test[to_test >= 3])
  
  # Return names of binary variables as output
  cont_var
}
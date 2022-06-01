#' @title find_continouus
#'
#' @description Returns a vector with all numeric variables with three or more
#' unique values.
#'
#' @param .data A data.frame or tibble.
#' @param .bin_var A character vector containing all binary variables
#' (if known). This results in the output of all numeric variables not in this
#' string.
#' 
#' @return A character vector with the numeric variables.
#' @examples -
#' @export

find_continouus <- function(.data
                            , .bin_var = NULL
){
  
  # Get all numeric variables
  numeric_variables <- names(.data[, unlist(lapply(.data, is.numeric))])
  
  # Create output in case a .bin_var vector was specified
  if (!is.null(.bin_var)) {
    
    # Check if .bin_var is not a character vector 
    if (class(.bin_var) != "character") {
      stop(".bin_var needs to be a character vector!")
    }
    
    # Keep all numeric variables not in .bin_var
    cont_var <- setdiff(numeric_variables, .bin_var)
    
    # Return output vector
    cont_var
    
  # Create output for unspecified .bin_var (return numerics mit 'unique >= 3')  
  } else {
  
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
}
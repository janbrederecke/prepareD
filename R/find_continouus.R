#' @export

find_continouus <- function(d) {
  
  # Get all numeric variables
  numeric_variables <- names(d[, unlist(lapply(d, is.numeric))])
  
  # Count unique values but NA in variables to be tested
  to_test <- unlist(lapply(d[, numeric_variables], function(x)
    length(na.omit(unique(
      x
    )))))
  
  # Mark variables with 3 or more unique values as continouus variables
  cont_var <- names(to_test[to_test >= 3])
  
  # Return names of binary variables as output
  cont_var
}
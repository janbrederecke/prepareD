#' @title make_dummies
#'
#' @description Returns the input data with additional dummy variables for
#' specified variables
#'
#' @param .data A data.frame or tibble
#' @param .input_variables A character vector containing variables that should be
#' recoded as dummy variables
#' 
#' @return The a data.frame of the input data with additional dummy variables
#' for all .input_variables
#' @examples -
#' @export

make_dummies <- function(.data
                         , .input_variables
){
  
  # Check if all cat_predictors are in data
  if (sum(.input_variables %in% names(.data)) < length(.input_variables)) {
    stop("Some .input_variables are not in data!")
  }
  
  # Get dummy levels per variable
  for (i in seq_along(.input_variables)) {
    
    # Find out which number of names(.data) corresponds to the variable
    dummy_name <- which(names(.data) %in% .input_variables[i])
    
    # Check variable type and define dummy_levels accordingly
    if (class(.data[[dummy_name]]) == "character") {
      dummy_levels <- unique(na.omit(.data[[dummy_name]]))
    }
    
    if (class(.data[[dummy_name]]) == "factor" |
        class(.data[[dummy_name]]) == "numeric" |
        class(.data[[dummy_name]]) == "integer") {
      dummy_levels <- as.numeric(as.character(sort(unique(
        na.omit(.data[[dummy_name]])
      ))))
    }
    
    # Create actual dummy variables in data
    for (j in seq_along(dummy_levels)) {
      name <- paste0(.input_variables[i], dummy_levels[j])
      .data[ncol(.data) + 1] <- NA
      names(.data)[ncol(.data)] <- paste0(name)
      .data[[name]] <- ifelse(.data[[dummy_name]] == dummy_levels[j], 1, 0)
    }
  }
  
  # Return data with dummy variables
  .data
}

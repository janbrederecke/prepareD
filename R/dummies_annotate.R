#' @title dummies_annotate
#'
#' @description Returns the annotation with additional dummy variables for
#' specified variables. Requires a data.frame or matrix object of name
#' 'annotation' in the global environment. The annotation object must be
#' specified using the column names "name", "pname", "unit", "short_pname", and
#' "comment". This function grabs the "pname" of the .input_variables to create
#' the dummy names.
#'
#' @param .data A data.frame or tibble
#' @param .input_variables A character vector containing variables that should be
#' recoded as dummy variables in the annotation
#' 
#' @return The annotation with additional dummy variables for all
#' .input_variables
#' @examples -
#' @export

dummies_annotate <- function(.data
                             , .input_variables
){
  
  # Check if all .input_variables are in data
  if (sum(.input_variables %in% names(.data)) < length(.input_variables)) {
    stop("Some variables are not in data!")
  }
  
  # Check which variable needs dummies and add to annotation
  for (i in seq_along(.input_variables)) {
    
    # Find out which number of names(.data) corresponds to the variable
    num <- which(names(.data) %in% .input_variables[i])
    
    # Check variable type and define dummy_levels accordingly
    if (class(.data[[num]]) == "factor" |
        class(.data[[num]]) == "character") {
      dummy_levels <- unique(na.omit(.data[[num]]))
    }
    
    if (class(.data[[num]]) == "numeric") {
      dummy_levels <- as.numeric(as.character(sort(unique(
        na.omit(.data[[num]])
      ))))
    }
    
    # Print the levels of every variable 
    print(paste0("Dummy levels of ", names(.data)[num], ": ",
                 paste(dummy_levels, collapse = ", ")))
    
    # Create the actual new names for the dummy variables
    for (j in seq_along(dummy_levels)) {
      name <- paste0(.input_variables[i], dummy_levels[j])
      
      # Check variable type and define dummy variable name accordingly
      if (class(.data[[num]]) == "factor" |
          class(.data[[num]]) == "character") {
        pname <-
          paste0(annotation[[2]][which(annotation[[1]] %in% names(.data)[[num]])],
                 ":",
                 dummy_levels[j])
      }
      
      if (class(.data[[num]]) == "numeric") {
        pname <-
          paste0(annotation[[2]][which(annotation[[1]] %in% names(.data)[[num]])],
                 ":",
                 dummy_levels[j])
      }
      
      # Create additional row and add it to annotation
      unit <- short_pname <- comment <- NA
      new_row <- cbind(name, pname, unit, short_pname, comment)
      annotation <- rbind(annotation, new_row)
      rownames(annotation) <- annotation[[1]]
    }
  }
  
  # Return ammended annotation
  annotation
}

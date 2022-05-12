#' @export

annotate_dummies <- function(d, cat_predictors) {
  
  # Check if all cat_predictors are in data
  if (sum(cat_predictors %in% names(d)) < length(cat_predictors)) {
    stop("Some variables are not in data!")
  }
  
  # Check which variable needs dummies and add to annotation
  for (i in seq_along(cat_predictors)) {
    
    # Find out which number of names(d) corresponds to the variable
    num <- which(names(d) %in% cat_predictors[i])
    
    # Check variable type and define dummy_levels accordingly
    if (class(d[[num]]) == "character") {
      dummy_levels <- unique(na.omit(d[[num]]))
    }
    
    if (class(d[[num]]) == "factor" |
        class(d[[num]]) == "numeric") {
      dummy_levels <- as.numeric(as.character(sort(unique(
        na.omit(d[[num]])
      ))))
    }
    
    # Print the levels of every variable 
    print(paste0("Dummy levels of ", names(d)[num], ": ",
                 paste(dummy_levels, collapse = ", ")))
    
    # Create the actual new names for the dummy variables
    for (j in seq_along(dummy_levels)) {
      name <- paste0(cat_predictors[i], dummy_levels[j])
      
      # Check variable type and define dummy variable name accordingly
      if (class(d[[num]]) == "character") {
        pname <-
          paste0(annotation[[2]][which(annotation[[1]] %in% names(d)[[num]])],
                 ":",
                 dummy_levels[j])
      }
      
      if (class(d[[num]]) == "factor" |
          class(d[[num]]) == "numeric") {
        pname <-
          paste0(annotation[[2]][which(annotation[[1]] %in% names(d)[[num]])],
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

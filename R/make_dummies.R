#' @export

make_dummies <- function(d, to_make_dummies) {
  
  # Check if all cat_predictors are in data
  if (sum(to_make_dummies %in% names(d)) < length(to_make_dummies)) {
    stop("Some variables are not in data!")
  }
  
  # Get dummy levels per variable
  for (i in seq_along(to_make_dummies)) {
    
    # Find out which number of names(d) corresponds to the variable
    dummy_name <- which(names(d) %in% to_make_dummies[i])
    
    # Check variable type and define dummy_levels accordingly
    if (class(d[[dummy_name]]) == "character") {
      dummy_levels <- unique(na.omit(d[[dummy_name]]))
    }
    
    if (class(d[[dummy_name]]) == "factor" |
        class(d[[dummy_name]]) == "numeric" |
        class(d[[dummy_name]]) == "integer") {
      dummy_levels <- as.numeric(as.character(sort(unique(
        na.omit(d[[dummy_name]])
      ))))
    }
    
    # Create actual dummy variables in data
    for (j in seq_along(dummy_levels)) {
      name <- paste0(to_make_dummies[i], dummy_levels[j])
      d[ncol(d) + 1] <- NA
      names(d)[ncol(d)] <- paste0(name)
      d[[name]] <- ifelse(d[[dummy_name]] == dummy_levels[j], 1, 0)
    }
  }
  
  # Return data with dummy variables
  d
}

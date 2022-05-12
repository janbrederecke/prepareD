#' @export

create_dummies <- function(cat_predictors, data) {
  
  for (i in seq_along(cat_predictors)) {
    dummy_name <- which(names(data) %in% cat_predictors[i])
    print(dummy_name)
    
    if (class(data[[dummy_name]]) == "character") {
      dummy_levels <- unique(na.omit(data[[dummy_name]]))
      print(dummy_levels)
    }
    
    if (class(data[[dummy_name]]) == "factor" |
        class(data[[dummy_name]]) == "numeric") {
      dummy_levels <- as.numeric(as.character(sort(unique(
        na.omit(data[[dummy_name]])
      ))))
      print(dummy_levels)
    }
    
    print(dummy_levels)
    
    for (j in seq_along(dummy_levels)) {
      name <- paste0(cat_predictors[i], dummy_levels[j])
      print(name)
      
      if (class(data[[dummy_name]]) == "character") {
        pname <-
          paste0(annotation[[2]][which(annotation[[1]] %in% names(data)[[dummy_name]])],
                 ":",
                 dummy_levels[j])
      }
      
      if (class(data[[dummy_name]]) == "factor" |
          class(data[[dummy_name]]) == "numeric") {
        pname <-
          paste0(annotation[[2]][which(annotation[[1]] %in% names(data)[[dummy_name]])],
                 ":",
                 dummy_levels[j])
      }
      
      unit <- NA
      short_pname <- NA
      comment <- NA
      x <- cbind(name, pname, unit, short_pname, comment)
      print(x)
      annotation <- rbind(annotation, x)
      rownames(annotation) <- annotation[[1]]
      
    }
  }
  annotation
}

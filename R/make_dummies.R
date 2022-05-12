#' @export

make_dummies <- function(to_make_dummies, data) {
  
  for (i in seq_along(to_make_dummies)) {
    dummy_name <- which(names(data) %in% to_make_dummies[i])
    
    if (class(data[[dummy_name]]) == "character") {
      dummy_levels <- unique(na.omit(data[[dummy_name]]))
    }
    
    if (class(data[[dummy_name]]) == "factor" |
        class(data[[dummy_name]]) == "numeric" |
        class(data[[dummy_name]]) == "integer") {
      dummy_levels <- as.numeric(as.character(sort(unique(
        na.omit(data[[dummy_name]])
      ))))
    }
    
    for (j in seq_along(dummy_levels)) {
      name <- paste0(to_make_dummies[i], dummy_levels[j])
      data[ncol(data) + 1] <- NA
      names(data)[ncol(data)] <- paste0(name)
      data[[name]] <-
        ifelse(data[[dummy_name]] == dummy_levels[j],
               1,
               0)
    }
  }
  data
}
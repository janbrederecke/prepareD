#' @export

check_data <- function(d, caption = "", print = TRUE) {
  
  # Make all necessary vectors beforehand to improve performance
  var <- var_type <- min <- max <- character(length = (ncol(d)))
  missings <- missings_prop <- empty_but_not_NA <- numeric(length = (ncol(d)))
  nrow_d <- nrow(d)
  
  # Calculate all the necessary values for the output table
  for (i in seq_along(names(d))) {
    
    ## Calculate missings and variable type for each variable
    var[i] <- names(d)[i]
    missings[i] <- sum(is.na(d[[i]]))
    missings_prop[i] <- round(sum(is.na(d[[i]])) / nrow_d * 100, 2)
    empty_but_not_NA[i] <- sum(d[[i]] %in% c("", " "), na.rm = TRUE)
    var_type[i] <- class(d[[i]])
    
    ## Get the minimum and maximum for every variable
    if (class(d[[i]]) %in% c("numeric", "integer")) {
      
      min[i] <- as.character(round(min(d[[i]], na.rm = TRUE), 3))
      max[i] <- as.character(round(max(d[[i]], na.rm = TRUE), 3))
      
    } else {
      
      min[i] <- as.character(sort(d[[i]])[1])
      max[i] <- as.character(sort(d[[i]], decreasing = TRUE)[1])
    }
  }
  
  # Put everything into a data.frame arranged by amount of missingness
  data <- data.frame(
    var = var,
    missings = missings,
    missings_prop = missings_prop,
    empty_but_not_NA = empty_but_not_NA,
    var_type = var_type,
    min = min,
    max = max
  ) %>%
    arrange(desc(missings))
  
  # Make a matrix from the data.frame and add desired names
  data <- as.matrix(data)
  colnames(data) <-
    c("Variable",
      "N missing",
      "% missing",
      "N empty but not NA",
      "Type",
      "Minimum",
      "Maximum")
  
  # Make knitr output if HTML output is desired
  if (print == TRUE) {
    options(knitr.kable.NA = '')
    print(kableExtra::kable_styling(
      kable_input = knitr::kable(
        data,
        format = "html",
        digits = 3,
        caption = caption,
        escape = FALSE
      ),
      full_width = TRUE,
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    ))
  
  # Return the output matrix if no HTML output is desired    
  } else {
    data
  }
}

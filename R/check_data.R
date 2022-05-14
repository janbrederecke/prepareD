#' @title check_data
#'
#' @description Provides a table with info on missingness and variable type for
#' the input dataset
#'
#' @param .data A data.frame or tibble
#' @param .caption A character vector used as the caption of the output
#' @param .print If .print = TRUE, an HTML output is generated, else a matrix is
#' returned
#'
#' @return Either a HTML output for use in Markdown files or a matrix
#' @examples -
#' @export
#' @importFrom kableExtra "kable_styling"
#' @importFrom knitr "kable"

check_data <- function(.data
              , .caption = ""
              , .print = TRUE
){
  
  # Make all necessary vectors beforehand to improve performance
  var <- var_type <- min <- max <- character(length = (ncol(.data)))
  missings <- missings_prop <- empty_but_not_NA <- numeric(length = (ncol(.data)))
  nrow_d <- nrow(.data)
  
  # Calculate all the necessary values for the output table
  for (i in seq_along(names(.data))) {
    
    ## Calculate missings and variable type for each variable
    var[i] <- names(.data)[i]
    missings[i] <- sum(is.na(.data[[i]]))
    missings_prop[i] <- round(sum(is.na(.data[[i]])) / nrow_d * 100, 2)
    empty_but_not_NA[i] <- sum(.data[[i]] %in% c("", " "), na.rm = TRUE)
    var_type[i] <- class(.data[[i]])
    
    ## Get the minimum and maximum for every variable
    if (class(.data[[i]]) %in% c("numeric", "integer")) {
      
      min[i] <- as.character(round(min(.data[[i]], na.rm = TRUE), 3))
      max[i] <- as.character(round(max(.data[[i]], na.rm = TRUE), 3))
      
    } else {
      
      min[i] <- as.character(sort(.data[[i]])[1])
      max[i] <- as.character(sort(.data[[i]], decreasing = TRUE)[1])
    }
  }
  
  # Make output matrix
  output <- matrix(c(var
                     , missings
                     , missings_prop
                     , empty_but_not_NA
                     , var_type
                     , min
                     , max)
                   , nrow = length(names(.data))
                   , ncol = 7
                   , dimnames = list(c(1:length(names(.data)))
                                     , c("Variable"
                                         , "N missing"
                                         , "% missing"
                                         , "N empty but not NA"
                                         , "Type"
                                         , "Minimum"
                                         , "Maximum")
            ))
  
  # Make knitr output if HTML output is desired
  if (.print == TRUE) {
    options(knitr.kable.NA = '')
    print(kableExtra::kable_styling(
      kable_input = knitr::kable(
        output
        , format = "html"
        , digits = 3
        , caption = .caption
        , escape = FALSE
      ),
      full_width = TRUE
      , bootstrap_options = c("striped", "hover", "condensed", "responsive")
    ))
  
  # Return the output matrix if no HTML output is desired    
  } else {
    output
  }
}

#' @title show_na
#'
#' @description Provides an overview on missing values for the input dataset
#'
#' @param d A data.frame or tibble
#' @param caption A character vector used as the caption of the output
#' @param print If print = TRUE, an HTML output is generated, else a matrix is
#' returned
#' 
#' @return Either a HTML output for use in Markdown files or a matrix
#' @examples -
#' @export
#' @importFrom dplyr "%>%" "arrange" "desc"
#' @importFrom kableExtra "kable_styling"
#' @importFrom knitr "kable"

show_na <- function(d, caption = "", print = TRUE) {
  
  # Make all necessary vectors beforehand
  var <- character()
  missings <- missings_prop <- empty_but_not_NA <- numeric()
  
  for (i in 1:length(names(d))) {
    
    if (sum(is.na(d[[i]])) > 0) {
      
      var[length(var) + 1] <- names(d)[i]
      missings[length(missings) + 1] <- sum(is.na(d[[i]]))
      missings_prop[length(missings_prop) + 1] <-
        round(sum(is.na(d[[i]])) / nrow(d) * 100, 2)
      empty_but_not_NA[length(empty_but_not_NA) + 1] <-
        sum(d[[i]] %in% c("", " "), na.rm = TRUE)
    }
  }
  
  # Make data.frame with all the info
  output <- data.frame(
    var = var,
    missings = missings,
    missings_prop = missings_prop,
    empty_but_not_NA = empty_but_not_NA
  ) %>%
    dplyr::arrange(dplyr::desc(missings))
  
  # Make a matrix with correct colnames
  output <- as.matrix(output)
  colnames(output) <-
    c("Variable",
      "N missing",
      "% missing",
      "N empty but not NA")
  
  # Make an HTML output if wanted
  if (print == TRUE) {
    
    ## Make pretty knitr output
    options(knitr.kable.NA = '')
    print(kableExtra::kable_styling(
      kable_input = knitr::kable(
        output,
        format = "html",
        digits = 3,
        caption = caption,
        escape = FALSE
      ),
      full_width = TRUE,
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    ))
    
  # Standard output if wanted  
  } else {
    output
  }
}
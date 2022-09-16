#' @title data_overview
#'
#' @description Provides an overview table for the input dataset.
#'
#' @param .data A data.frame or tibble.
#' @param .caption A character vector used as the caption of the output.
#' @param .print If .print = TRUE, an HTML output is generated, else a matrix is
#' returned.
#' @param .percent_missing A number determining the percentage of
#' missingness used to count variables with high amount of missing values.
#'
#' @return Either a HTML output for use in Markdown files or a matrix.
#' @examples -
#' @export

data_overview <- function(.data
                 , .caption = ""
                 , .print = TRUE
                 , .percent_missing = 30
){

  # Count columns and rows
  cols <- ncol(.data)
  rows <- nrow(.data)

  # Calculate percent of cols and rows to prevent recalculating in next step
  percent_of_rows <- round((.percent_missing / 100) * rows, 0)
  percent_of_cols <- round((.percent_missing / 100) * cols, 0)

  # Count empty columns and columns with more than 30% missings
  empty_cols <- sum(apply(.data, 2, function(x) sum(is.na(x)) == rows) + 0)
  cols_percent_missing <-
    sum(apply(.data, 2, function(x) sum(is.na(x)) >= percent_of_rows) + 0)

  # Count empty rows and rows with more than 30% missings
  empty_rows <- sum(apply(.data, 1, function(x) sum(is.na(x)) == cols) + 0)
  rows_percent_missing <-
    sum(apply(.data, 1, function(x) sum(is.na(x)) >= percent_of_cols) + 0)

  # Necessary for use with pipe in front of the function
  if (as.character(deparse(substitute(.data))) == ".") {
    input_name <- c("Input dataset")
  } else {
    input_name <- c(as.character(deparse(substitute(.data))))
  }

  # Make output matrix
  output <- matrix(c(cols
                     , empty_cols
                     , cols_percent_missing
                     , rows
                     , empty_rows
                     , rows_percent_missing)
                   , nrow = 1
                   , ncol = 6
                   , dimnames = list(c(paste0(input_name))
                                     , c(
      "Variables"
      , "Empty variables"
      , paste0("Variables with more than "
                , .percent_missing
                , "% missing values")
      , "Rows"
      , "Empty rows"
      , paste0("Rows with more than "
                , .percent_missing
                , "% missing values")
            )))

  # Make an HTML output if wanted
  if (.print == TRUE) {

    # Make pretty knitr output
    options(knitr.kable.NA = '')
    print(kableExtra::kable_styling(
      kable_input = knitr::kable(
        output
        , format = "html"
        , digits = 3
        , caption = .caption
        , escape = FALSE
        , align = c("l", "c", "c", "c", "c", "c")
      ),
      , full_width = TRUE
      , bootstrap_options = c("striped", "hover", "condensed", "responsive")
    ))

  # Standard output if wanted
  } else {
    output
  }
}

#' @export

overview_data <- function(d, caption = "", print = TRUE, percent_missing = 30) {
  
  # Count columns and rows
  cols <- ncol(d)
  rows <- nrow(d)
  
  # Calculate percent of cols and rows to prevent recalculating in next step
  percent_of_rows <- round((percent_missing / 100) * rows, 0)
  percent_of_cols <- round((percent_missing / 100) * cols, 0)
  
  # Count empty columns and columns with more than 30% missings
  empty_cols <- sum(apply(d, 2, function(x) sum(is.na(x)) == rows) + 0)
  cols_percent_missing <- 
    sum(apply(d, 2, function(x) sum(is.na(x)) >= percent_of_rows) + 0)
  
  # Count empty rows and rows with more than 30% missings 
  empty_rows <- sum(apply(d, 1, function(x) sum(is.na(x)) == cols) + 0)
  rows_percent_missing <- 
    sum(apply(d, 1, function(x) sum(is.na(x)) >= percent_of_cols) + 0)
  
  # Make data.frame with all the info
  data <- data.frame(
    cols = cols,
    empty_cols = empty_cols,
    cols_percent_missing = cols_percent_missing,
    rows = rows,
    empty_rows = empty_rows,
    rows_percent_missing = rows_percent_missing
  )
  
  # Make a matrix with correct colnames
  data <- as.matrix(data)
  colnames(data) <-
    c(
      "N variables",
      "N empty variables",
      paste0("N variables with more than ", percent_missing, "% missing values"),
      "N rows",
      "N empty rows",
      paste0("N rows with more than ", percent_missing, "% missing values")
    )
  
  # Necessary to use pipe in fron of the function and print correct data name
  if (as.character(deparse(substitute(d))) == ".") {
    rownames(data) <- c("Input dataset")
  } else {
    rownames(data) <- c(as.character(deparse(substitute(d))))
  }
  
  if (print == TRUE) {
    
    # Make a HTML output
    options(knitr.kable.NA = '')
    print(kableExtra::kable_styling(
      kable_input = knitr::kable(
        data,
        format = "html",
        digits = 3,
        caption = caption,
        escape = FALSE,
        align = c("l", "c", "c", "c", "c", "c")
      ),
      full_width = TRUE,
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    ))
    
    # Standard output
  } else {
    data
  }  
}
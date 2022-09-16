#' @title data_clean
#'
#' @description Returns the input dataframe without empty rows and columns.
#'
#' @param .data A data.frame or tibble.
#' @param .rows If TRUE, empty rows are omitted.
#' @param .columns If TRUE, empty columns are omitted.
#' @param .row_percent Integer, indicates the percentage of missing values that
#' define an empty row.
#' @param .column_percent Integer, indicates the percentage of missing values
#' that define an empty column.
#' @param .print If .print = TRUE, a short summary is returned.
#' @param .return_tbl_df If TRUE a tibble is returned, else a data.frame is
#' returned.
#'
#' @return A data.frame or tibble without empty rows and columns.
#' @examples -
#' @export

data_clean <- function(.data
                     , .rows = TRUE
                     , .columns = TRUE
                     , .row_percent = 100
                     , .column_percent = 100
                     , .print = TRUE
                     , .return_tbl_df = FALSE
){

  # Check if input data is correct (a data.frame)
  ## Check if input is given at all
  if (is.null(.data)) {
    stop("You have to give a dataframe as input!")

  ## Check if input has the right format
  } else if (!is.data.frame(.data)) {
    stop("Your input has to be a dataframe!")
  }

  # Count columns and rows
  cols <- ncol(.data)
  rows <- nrow(.data)

  # Calculate percent of cols and rows to prevent recalculating in next step
  percent_of_rows <- round((.row_percent / 100) * rows, 0)
  percent_of_cols <- round((.column_percent / 100) * cols, 0)

  # Find out which rows and columns have too many missings
  empty_rows <- apply(.data, 1, function(x) sum(is.na(x)) >= percent_of_rows)
  empty_cols <- apply(.data, 2, function(x) sum(is.na(x)) >= percent_of_cols)

  # Remove the rows and columns with too many missings
  ## Keep the original names for output
  names_data <- names(.data)

  .data <- .data[!empty_rows, ]
  .data <- .data[!empty_cols]

  if (.print == TRUE) {
    if (sum(empty_cols) > 0) {
        print(paste0("Removed columns: ", paste0(names_data[empty_cols], collapse = ", ")))
    } else {
        print(paste0("No columns removed."))
    }

    if (sum(empty_rows) > 0) {
        print(paste0("Number of removed rows: ", sum(empty_rows)))
    } else {
        print(paste0("No rows removed."))
    }
  }
  # Return output
  if (.return_tbl_df == TRUE) {
    return(tibble::as_tibble(.data))
  } else {
    return(as.data.frame(.data))
  }
}

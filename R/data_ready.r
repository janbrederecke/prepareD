#' @title data_ready
#'
#' @description Returns a suggestion on how ready your data is for analysis.
#'
#' @param .data A data.frame or tibble.
#'
#' @return A message indicating how ready your data is for analysis.
#' @examples -
#' @export

data_ready <- function(.data
){    
    # Check if data is a tibble and if not, turn it into one (automatically checks for valid names)
    if (class(.data)[1] == "data.frame") {
    .data <- tibble::as_tibble(.data)
    } else if (!class(.data)[1] %in% "tbl_df") {
    stop("Input must be a data.frame or tibble.")
    }
}
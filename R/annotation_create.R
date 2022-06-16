#' @title annotation_create
#'
#' @description Creates an annotation file for the input dataset.
#'
#' @param .data A dataframe.
#' @param .name_as_pname If TRUE, the column names are also added as the pname.
#'
#' @return A matrix with all names of the input dataset in row 'name' and
#' additional rows 'pname', 'unit', 'short_pname', and 'comment'.
#' @examples -
#' @export

annotation_create <- function(.data
                              , .name_as_pname = FALSE
){

  # Check if input data is correct (a data.frame)
  ## Check if input is given at all
  if (is.null(.data)) {
    stop("You have to give a dataframe as input!")

  ## Check if input has the right format
  } else if (!is.data.frame(.data)) {
    stop("Your input has to be a dataframe!")
  }

  # Check if input for .name_as_pname is correct
  if (!is.logical(.name_as_pname)) {
    stop("The input for .name_as_pname has to be a logical!")
  }

  # Create annotation matrix
  annotation <- matrix(nrow = ncol(.data),
                       ncol = 5,
                       dimnames = list(c(),
                        c("name", "pname", "unit", "short_pname", "comment")))
  annotation[, "name"] <- names(.data)

  if (.name_as_pname) {
    annotation[, "pname"] <- names(.data)
  }

  # Return annotation matrix
  annotation
}

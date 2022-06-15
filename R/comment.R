#' @title comment
#'
#' @description Let's you comment your analysis with one simple function.
#'
#' @param .comment A character vector.
#'
#' @return A matrix containing your comments is created in the global
#' environment.
#' @examples -
#' @export

comment <- function(.comment = ""
){
  # Check if input is correct (a character vector)
  if (is.character(.comment) != TRUE) {

    # Stop if input is of wrong type
    stop("Your comment has to be a character vector!")
  }

  # Check if object 'comments' already exists
  if (!exists("comments")) {

    # Make an empty empty comments matrix if not existing
    comments <-  matrix(nrow = 0, ncol = 1, dimnames = list(c(), c("Comments")))
  }

  # Row bind the new comment to the comments object
  comments <- rbind(comments, .comment)

  # Exchange rownames for the number of each comment
  rownames(comments) <- 1:nrow(comments)

  # Return the comments matrix to the global environment
  comments <<- comments
}

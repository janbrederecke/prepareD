#' @title time_start
#'
#' @description Initializes the starting point of a time measurement.
#'
#' @return Creates a time_started object in the global environment.
#' @examples -
#' @export

time_start <- function(
){
  # Create the time start object in the global environment
  time_started <<- Sys.time()
}

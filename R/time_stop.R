#' @title time_stop
#'
#' @description Calculates the time difference since 'time_start' has been
#' initialized
#'
#' @return Creates a 'time_it_took' object in the global environment.
#' @examples -
#' @export

time_stop <- function(
){
  # Create the time start object in the global environment
  time_it_took <<- Sys.time() - time_started
}
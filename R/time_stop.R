#' @title time_stop
#'
#' @description Calculates the time difference since 'time_start' has been
#' initialized.
#'
#' @return Creates a 'time_it_took' object in the global environment.
#' @examples -
#' @export

time_stop <- function(
){
  if (!exists("time_started")) {
    stop("You have to initialize a 'time_started' using 'time_start' before
         'time_stop' can be used.")
  }
  
  # Calculate difference  between time_start and the Sys.time
  time_difference <- Sys.time() - time_started
  
  # Format the time
  time <- round(as.numeric(unclass(time_difference)), 2)
  
  # Get the unit of the time difference (usually minutes)
  unit <- attr(time_difference, "units")
  
  # Create the time_it_took object in the global environment
  time_it_took <- NULL
  time_it_took <<- paste0(time, " ", unit)
}

# We have to introduce a global variable for time_started as we cannot declare
# it inside the function
if (getRversion() >= "2.15.1")  utils::globalVariables(c("time_started"))
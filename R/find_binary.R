#' @title find_binary
#'
#' @description Returns either a vector with all clearly binary variables or a
#' list with additional information on variables that might also be binary.
#'
#' @param .data A data.frame or tibble.
#' @param .output "bin" returns a vector of binary variables, "list" returns a
#' list with binary variables, variables coded yes/no and more information.
#' @param .include If .include = TRUE, variables recoded as 0/1 within the function
#' are returned in the list-output - requires .output = "list".
#' @param .yes Optional character vector that can be used to feed values that
#' should be used as 'yes'.
#' @param .no Optional character vector that can be used to feed values that
#' should be used as 'no'.
#' @param .na Optional character vector that can be used to feed values that
#' should be used as 'NA'.
#' @param .print If .print = TRUE, values detected as yes/no are
#' printed.
#' 
#' @return Either a character vector with clearly binary variables or a list
#' with additional information.
#' @examples -
#' @export

find_binary <- function(.data
                        , .output = "bin"
                        , .include = FALSE
                        , .yes = NULL
                        , .no = NULL
                        , .na = NULL
                        , .print = TRUE
){
  
  # Stop if output is not defined correctly
  if (!(.output %in% c("bin", "list"))) {
    stop("For '.output' only 'bin' and 'list' allowed.")
  }
  
  # Check for external yes, no, and NA vectors
  ## Check for yes_vector
  if (is.null(.yes)) {
    .yes <- c("y", "yes", "j", "ja", "jo", "1", "1-ja", "cad")
  }
  
  ## Print which values were counted as yes
  if (.print == TRUE) {
    print(paste0("These values were coded as 'yes': ", 
                 paste(.yes, collapse = ", ")))
  }
  
  ## Check for no_vector
  if (is.null(.no)) {
    .no <- c("no", "n", "nein", "ne", "0", "0-nein", "no-cad")
  }
  
  ## Print which values were counted as no
  if (.print == TRUE) {
    print(paste0("These values were coded as 'no': ",
                 paste(.no, collapse = ", ")))
  }
  
  ## Check for NA_vector
  if (is.null(.na)) {
    .na <- c("", " ", "unknown", "vielleicht", NA)
  }
  
  ## Print which values were counted as NA
  if (.print == TRUE) {
    print(paste0("These values were coded as 'NA': ",
                 paste(.na, collapse = ", ")))
  }
  
  # Get all numeric variables
  numeric_variables <- names(.data[, unlist(lapply(.data, is.numeric))])
  
  # Calculate minimum and maximum for all numeric variables
  mins <- apply(.data[numeric_variables], 2, min, na.rm = TRUE)
  maxs <- apply(.data[numeric_variables], 2, max, na.rm = TRUE)
  
  # Get all numeric variables that only have 0s and 1s
  bin_var <- sort(numeric_variables[(mins == 0 & maxs == 1) |
                                      (mins == 0 & maxs == 0) |
                                      (mins == 1 & maxs == 1)])
  
  # Get all numeric variables not in bin_var to check for suspects
  to_test <- numeric_variables[!numeric_variables %in% bin_var]
  
  # Add character variables
  character_variables <- names(.data[, unlist(lapply(.data, is.character))])
  to_test <- c(to_test, character_variables)
  
  # Make auxilliary dataset containing only variables of interest
  d_aux <- .data[, to_test]
  
  # Make all the character variables lowercase
  d_aux[, character_variables] <- lapply(d_aux[, character_variables], tolower)
  
  # Make all variables with obvious y / n coding equal 
  d_aux[, character_variables] <-
    lapply(d_aux[, character_variables], function(x) {
      dplyr::if_else(
        condition = x %in% .yes,
        true = "yes",
        false = dplyr::if_else(
          condition = x %in% .no,
          true = "no",
          false = dplyr::if_else(
            condition = x %in% .na,
            true = NA_character_,
            false = x
          )
        )
      )
    })
  
  # Define the two output variants
  # List output:
  if (.output == "list") {
    
    # Find "clear" yes/no binary variables
    ## Get number of unique values in character variables
    yn_unique_val <- unlist(lapply(d_aux[, character_variables], function(x)
      length(stats::na.omit(unique(
        x
      )))))
    
    ## Get character variables with 1 and 2 unique values
    suspected_ynvar_1 <- names(yn_unique_val[yn_unique_val == 1])
    suspected_ynvar_2 <- names(yn_unique_val[yn_unique_val == 2])
    
    ## Get the exact unique values of character variables with 1 unique
    yn1_test <- lapply(dplyr::select(d_aux, dplyr::all_of(suspected_ynvar_1)), function(x)
    {
      labelled::remove_attributes(stats::na.omit(unique(x)), c("na.action"))
    })
    
    ## Classify as y/n variable or not
    yn_var <- names(yn1_test[yn1_test %in% c("yes", "no")])
    
    ## Get the exact unique values of character variables with 2 uniques
    yn2_test <- lapply(dplyr::select(d_aux, dplyr::all_of(suspected_ynvar_2)), function(x)
    {
      labelled::remove_attributes(stats::na.omit(sort(unique(x))), c("na.action"))
    })
    
    ## Add to y/n variable or not
    yn2_var <- unlist(lapply(yn2_test, function(x)
    {
      sum(stringr::str_detect(x, paste(c("yes", "no"),
                                       collapse = "|")))
    }))
    yn_var <- c(yn_var, names(yn2_var[yn2_var == 2]))
    
    # Count unique values but NA in variables to be tested
    to_test <- setdiff(to_test, yn_var)
    to_test <- unlist(lapply(d_aux[, to_test], function(x)
      length(stats::na.omit(unique(
        x
      )))))
    
    # Mark variables with 2 or less unique values as supected binary variables
    suspected_var <- names(to_test[to_test <= 2])
    
    # Add unique values for all variables of interesst
    unique_values <- lapply(.data[, sort(c(bin_var, yn_var, suspected_var))],
                            function(x) {
                              labelled::remove_attributes(sort(stats::na.omit(unique(x))),
                                                          c("na.action"))
                            })
    
    # Create output list
    output <- list(
      "bin_var" = sort(bin_var),
      "yn_var" = sort(yn_var),
      "suspected_var" = sort(suspected_var),
      "unique_values" = unique_values,
      "yes_values" = .yes,
      "no_values" = .no
    )
    
    # Return output
    if (.include != TRUE) {
      
      output
      
      # Return output with binarized y/n data    
    } else {
      
      # Binarize y/n data
      d_aux <- as.data.frame(dplyr::select(d_aux,
                                           dplyr::all_of(sort(yn_var))
                                           )
                             )
      d_aux <-
        as.data.frame(lapply(d_aux, function(x) {
          dplyr::if_else(
            condition = x == "yes",
            true = 1,
            false = dplyr::if_else(
              condition = x == "no",
              true = 0,
              false = NA_real_
            )
          )
        }))
      
      # Add binarized data to output list
      output[["binarized_yn_data"]] <- d_aux
      
      # Return output
      output
    }  
    
    # Standard output:  
  } else if (.output == "bin") {
    
    # Count unique values but NA in variables to be tested
    to_test <- unlist(lapply(d_aux, function(x)
      length(stats::na.omit(unique(
        x
      )))))
    
    # Mark variables with 2 or less unique values as supected binary variables
    suspected_var <- sort(names(to_test[to_test <= 2]))
    
    # Report suspected variables 
    if (length(suspected_var) > 0) {
      print(paste0(
        "These variables might be binary but are not coded using 0 & 1: ",
                   suspected_var))
    }
    
    # Return binary variables
    bin_var
  }
}

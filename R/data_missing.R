#' @title data_missing
#'
#' @description Returns overview on missing values for variables of the input
#' dataset containing at least one missing value.
#'
#' @param .data A data.frame or tibble.
#' @param .caption A character vector used as the caption of the output.
#' @param .print If .print = TRUE, an HTML output is generated, else a matrix is
#' returned.
#' @param .sort_by_na If TRUE, results are sorted by the number of NAs.
#'
#' @return Either a HTML output for use in Markdown files or a matrix.
#' @examples -
#' @export

data_missing <- function(.data
                    , .caption = ""
                    , .print = TRUE
                    , .sort_by_na = FALSE
){

  # Make all necessary vectors beforehand
  var <- character()
  missings <- missings_prop <- empty_but_not_NA <- numeric()

  for (i in 1:length(names(.data))) {
    if (sum(is.na(.data[[i]])) > 0) {
      var[length(var) + 1] <- names(.data)[i]
      missings[length(missings) + 1] <- sum(is.na(.data[[i]]))
      missings_prop[length(missings_prop) + 1] <-
        round(sum(is.na(.data[[i]])) / nrow(.data) * 100, 2)
      empty_but_not_NA[length(empty_but_not_NA) + 1] <-
        sum(.data[[i]] %in% c("", " "), na.rm = TRUE)
    } else {
      var[length(var) + 1] <- names(.data)[i]
      missings[length(missings) + 1] <- 0
      missings_prop[length(missings_prop) + 1] <- 0
      empty_but_not_NA[length(empty_but_not_NA) + 1] <-
        sum(.data[[i]] %in% c("", " "), na.rm = TRUE)

    }
  }

  # Make output matrix
  output <- matrix(c(var
                     , missings
                     , missings_prop
                     , empty_but_not_NA
                     )
                   , nrow = length(var)
                   , ncol = 4
                   , dimnames = list(c(1:length(var))
                                     , c("Variable"
                                         , "Missing"
                                         , "% missing"
                                         , "Empty but not NA"
                                         )
                                    )
                  )

  # Decide if results are sorted by number of NAs
  if (.sort_by_na == TRUE) {
    output <- output[order(as.numeric(output[, "Missing"]),
                           decreasing = TRUE), ]
    rownames(output) <- NULL
  }

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
      ),
      full_width = TRUE
      , bootstrap_options = c("striped", "hover", "condensed", "responsive")
    ))

  # Standard output if wanted
  } else {
    output
  }
}

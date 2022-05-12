#' @export

show_na <- function(d, caption = "", print = TRUE) {
  var <- vector()
  missings <- vector()
  missings_prop <- vector()
  empty_but_not_NA <- vector()
  
  for (i in 1:length(names(d))) {
    if (sum(is.na(d[[i]])) > 0) {
      var[length(var) + 1] <- names(d)[i]
      missings[length(missings) + 1] <- sum(is.na(d[[i]]))
      missings_prop[length(missings_prop) + 1] <-
        round(sum(is.na(d[[i]])) / nrow(d) * 100, 2)
      empty_but_not_NA[length(empty_but_not_NA) + 1] <-
        sum(d[[i]] %in% c("", " "), na.rm = TRUE)
    }
  }
  
  data <- data.frame(
    var = var,
    missings = missings,
    missings_prop = missings_prop,
    empty_but_not_NA = empty_but_not_NA
  ) %>%
    arrange(desc(missings))
  
  data <- as.matrix(data)
  colnames(data) <-
    c("Variable",
      "N missing",
      "% missing",
      "N empty but not NA")
  
  if (print == TRUE) {
    options(knitr.kable.NA = '')
    print(kableExtra::kable_styling(
      kable_input = knitr::kable(
        data,
        format = "html",
        digits = 3,
        caption = caption,
        escape = FALSE
      ),
      full_width = TRUE,
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    ))
  } else {
    data
  }
}
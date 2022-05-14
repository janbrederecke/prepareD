d <- data.frame(
  num1 = c(1, 2, 0, NA, 5, 1, 7, 1, 0, 10, 1, 2),
  num2 = c(2, 2, 2, NA, 2, 2, 2, 2, 2, 3, 2, 2),
  num3 = c(2.22222, 5, NA, 6.789, 10000.009, 34, 6, 7, 3, 5, 6, 66),
  yn1 = c("yes", "no-cad", "nein", "n", "j", "ja", "0", NA, "1-ja", "0-nein",
          "no", "no"),
  yn2 = c("yes", "yes", "yes", "yes", "yes", "yes",  "yes", NA, "yes", "yes",
          "yes", "yes"),
  yn3 = c("no", "no", "no", "no", "no", "no",  "no", NA, "no", "no", "no",
          "no"),
  bn1 = c(1, 0, 0, 0, 0, 0, 1, NA, NA, 1, 1, 0),
  bn2 = c(0, 0, 0, 0, 0, 0, 0, NA, NA, 0, 0, 0),
  bn3 = c(1, 1, 1, 1, 1, 1, 1, NA, NA, 1, 1, 1),
  dt1 = as.Date(c(rep("2021-03-17", 6), rep("2022-03-17", 3),
                  rep("2023-03-17", 3))),
  dt2 = as.Date(c(rep("2021-03-17", 5), rep("2021-03-18", 6),NA)),
  na1 = c(rep(NA, 12))
)

library(dplyr)
# Decide on the size of the data to be able to check speed of functions
for (i in 1:10) d <- rbind(d, d)
rm(i)

annotation <-
  data.frame(
    "name" = c("bn1", "bn2"),
    "pname" = c("Binary Variable", "Binary Variable2"),
    "unit" = c(NA, NA),
    "short_pname" = c(NA, NA),
    comment = c(NA, NA)
  )
rownames(annotation) <- annotation[[1]]

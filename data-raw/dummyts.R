## code to prepare `dummyts` dataset goes here
library(dplyr)
l <- 1e6
theta <- seq(0, 2 * pi * 8, length.out = l)
set.seed(1736)
dummyts <- tibble(t = as.POSIXct("1970-01-01", tz = "UTC") + 0:(l - 1),
                  y = sin(theta^1.2) + rnorm(l, sd = 0.25))
usethis::use_data(dummyts, overwrite = TRUE)

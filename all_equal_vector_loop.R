library(tidyverse)

v <- sample.int(20, 10, replace = TRUE)

while (all(v == 10) == FALSE) {
  v <- map_at(v, ~. + 1, .at = which(v < 10))
  v <- map_at(v, ~. - 1, .at = which(v > 10))
  print(flatten_dbl(v))
}
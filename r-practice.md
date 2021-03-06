R practice BAN401
================
Marcus Lauritsen
26 October 2018

## Problem 2 — L5&6

In this problem, we are going to utilize while and for loops in order to
manipulate some data.

1)  Start by creating a vector of length 10 with random numbers between
    0 and 20
2)  Use a while loop and with an inner for loop with conditional
    statements to increment all numbers in the vector lower than 10 by
    1, and decrement all numbers above 10 by 1 until the vector only
    contains numbers exactly equal to 10. Print the vector for each
    iteration of the while loop.
3)  Use a while loop and vector conditioning to achieve the same result
    as in 2)

<!-- end list -->

``` r
library(tidyverse)

v <- sample.int(20, 10, replace = TRUE)

while (all(v == 10) == FALSE) {
  v <- map_at(v, ~. + 1, .at = which(v < 10))
  v <- map_at(v, ~. - 1, .at = which(v > 10))
  print(flatten_dbl(v))
}
```

    ##  [1] 19  3  5 19  7  8 19 14  2  2
    ##  [1] 18  4  6 18  8  9 18 13  3  3
    ##  [1] 17  5  7 17  9 10 17 12  4  4
    ##  [1] 16  6  8 16 10 10 16 11  5  5
    ##  [1] 15  7  9 15 10 10 15 10  6  6
    ##  [1] 14  8 10 14 10 10 14 10  7  7
    ##  [1] 13  9 10 13 10 10 13 10  8  8
    ##  [1] 12 10 10 12 10 10 12 10  9  9
    ##  [1] 11 10 10 11 10 10 11 10 10 10
    ##  [1] 10 10 10 10 10 10 10 10 10 10

``` r
v <- sample.int(20, 10, replace = TRUE)

while (all(v == 10) == FALSE) {
  v[which(v > 10)] <- vapply(v[which(v > 10)], function(x) x - 1, vector("double", 1))
  v[which(v < 10)] <- vapply(v[which(v < 10)], function(x) x + 1, vector("double", 1))
  print(v)
}
```

    ##  [1]  4 13  5  8  5  9 10 18  2 10
    ##  [1]  5 12  6  9  6 10 10 17  3 10
    ##  [1]  6 11  7 10  7 10 10 16  4 10
    ##  [1]  7 10  8 10  8 10 10 15  5 10
    ##  [1]  8 10  9 10  9 10 10 14  6 10
    ##  [1]  9 10 10 10 10 10 10 13  7 10
    ##  [1] 10 10 10 10 10 10 10 12  8 10
    ##  [1] 10 10 10 10 10 10 10 11  9 10
    ##  [1] 10 10 10 10 10 10 10 10 10 10

``` r
v <- sample(20, 10, replace = TRUE)

while (!all(v == 10)) {
  v[which(v > 10)] <- v[which(v > 10)] - 1
  v[which(v < 10)] <- v[which(v < 10)] + 1
  print(v)
}
```

    ##  [1]  5  5 14 15  4 12 12  6 10 12
    ##  [1]  6  6 13 14  5 11 11  7 10 11
    ##  [1]  7  7 12 13  6 10 10  8 10 10
    ##  [1]  8  8 11 12  7 10 10  9 10 10
    ##  [1]  9  9 10 11  8 10 10 10 10 10
    ##  [1] 10 10 10 10  9 10 10 10 10 10
    ##  [1] 10 10 10 10 10 10 10 10 10 10

## Problem 3 — L1&2

Assume the following exchange rate: 1 USD = 9 NOK, and no taxes. Next
year, the economy is expecting a growth in inflation equal to 3%,
affecting both input and output prices. The company’s market share is
expected to grow from 5% to 25%, but the total market is constant. XYZ’s
costs are expected to decline by 5% of today’s cost because of a more
efficient production line. Based on the above information, you are asked
to answer the following questions:

What is the highest, lowest and the average price? What is this year’s
profits? Provide your answer with two strings, one telling what the
profits are in NOK, and one in USD. Please round your answer to the
nearest whole number if necessary.

``` r
m <- matrix(c(55, 25, 250,
              63, 15, 133,
              85, 25, 821),
            nrow = 3,
            byrow = TRUE,
            dimnames = list(
              c("x", "y", "z"),
              c("price", "cost", "units.sold")
              ))
```

## Problem 3 — L3&4

Assume no taxes and that prices and costs are given in NOK. Next year,
the economy is expecting a growth in inflation equal to 3%, affecting
both input and output prices. XYZ’s market share is expected to grow
from 5% to 25%, but the total market is constant. XYZ’s costs are
expected to decline by 5% of today’s cost because of a more efficient
production line. Based on this information, you are asked to answer the
following questions:

1.  Find next year’s prices, costs, and number of units sold and display
    them in a matrix.
2.  Assuming that the price and cost trends continue, but XYZ’s market
    share stays the same for year +2, what is that year’s prices, costs
    and number of units? Use a matrix to show your answer.
3.  Store the information on prices, costs, and number of units sold
    from the three time periods in a list. Print the list.
4.  Display the development in the cost of product Y as a vector, using
    list indexing.

<!-- end list -->

``` r
m <- matrix(c(55, 25, 250,
              63, 15, 133,
              85, 25, 821),
            nrow = 3,
            byrow = TRUE,
            dimnames = list(
              c("x", "y", "z"),
              c("price", "cost", "units.sold")
              ))

forecast.products <- function(df, inflation, growth.factor, cost.prod, n = 1) {
  df[, "price"] <- df[, "price"] * (1 + inflation)^n
  df[, "cost"] <- df[, "cost"] * (1 + inflation - cost.prod)^n
  df[, "units.sold"] <- df[, "cost"] * growth.factor
  return(df)
}

# task 1
n <- forecast.products(df = m,
                       inflation = 0.03,
                       growth.factor = 5,
                       cost.prod = -0.05)

# task 2
n2 <- forecast.products(df = n,
                       inflation = 0.03,
                       growth.factor = 1,
                       cost.prod = -0.05)

# task 3
(matrices <- list(m, n, n2))
```

    ## [[1]]
    ##   price cost units.sold
    ## x    55   25        250
    ## y    63   15        133
    ## z    85   25        821
    ## 
    ## [[2]]
    ##   price cost units.sold
    ## x 56.65 27.0        135
    ## y 64.89 16.2         81
    ## z 87.55 27.0        135
    ## 
    ## [[3]]
    ##     price   cost units.sold
    ## x 58.3495 29.160     29.160
    ## y 66.8367 17.496     17.496
    ## z 90.1765 29.160     29.160

``` r
# task 4
costs.y <- vector("double", length(matrices))
for (i in seq_along(matrices)) costs.y[i] <- matrices[[i]]["y", "cost"]
print(costs.y)
```

    ## [1] 15.000 16.200 17.496

## Problem \#3 — L7&8

What is the most common hair color for men and/or women in the R dataset
HairEyeColor?

Make a function that takes gender as a string argument. The function
should display the frequency of the different hair colors for the given
gender, in a pie chart. When passing “Female” to the function, it should
display a pie chart similar the one below. The title of the chart should
adapt according to the argument and the most common color - if the most
common hair color for gender supposedly was red, the title should
automatically say so.

``` r
library(tidyverse)
library(datasets)

HairEyeColor <- as.tibble(HairEyeColor)

pie.chart.hair <- function(sex) {
  filtered.df <- HairEyeColor %>% 
    filter(Sex == sex) %>%
    group_by(Hair) %>% 
    summarise(frequency = sum(n)) %>% 
    ungroup() %>% 
    arrange(desc(frequency))
  
  # create vector of colors to fill pie chart with
  colors <- vector("character", nrow(filtered.df))
  for (i in 1:nrow(filtered.df)) {
    colors[i] <- filtered.df$Hair[i]
  }
  colors <- replace(colors, colors == "Blond", "Yellow")
  
  pie(filtered.df$frequency,
      labels = filtered.df$Hair,
      col = colors,
      main = paste("The most common hair color for the sex", tolower(sex), "is", tolower(filtered.df[which.max(filtered.df$frequency), "Hair"])))
}

pie.chart.hair("Female")
```

![](r-practice_files/figure-gfm/problem%203L7&8-1.png)<!-- -->

``` r
pie.chart.hair("Male")
```

![](r-practice_files/figure-gfm/problem%203L7&8-2.png)<!-- -->

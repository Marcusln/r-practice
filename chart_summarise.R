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
pie.chart.hair("Male")

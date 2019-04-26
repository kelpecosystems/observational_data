library(tidyverse)
library(readxl)

fish <- read_excel("./FtWeatherill_fish_2017.xlsx")

fish_wide <- fish %>%
  spread(SIZE, COUNT)

write_csv(fish_wide, "../FtWeatherill_fish_2017.csv", fill = 0)

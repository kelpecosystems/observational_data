library(tidyverse)
library(readxl)

fish <- read_excel("./fish_2018_KEEN-ONE-size-classes.xlsx")

fish_wide <- fish %>%
  group_by(YEAR, MONTH, DAY, DATE, `VIS (m)`, TRANSECT, SP_CODE, `FISH.SIZE (cm)`,
           `AREA (m)`, OBS_CODE, NOTES, `ENTERED BY`, `CHECKED BY`) %>%
  summarize(COUNT = sum(COUNT, na.rm=T)) %>%
  spread(`FISH.SIZE (cm)`, COUNT)

write_csv(fish_wide, "../FtWeatherill_fish_2018.csv")

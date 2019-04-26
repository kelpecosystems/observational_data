library(tidyverse)
library(readxl)
library(sf)

sites <- read_excel("./Site_info_2017.xls") %>%
  st_as_sf(coords = c("LATITUDE_UTM", "LONGITUDE_UTM") ) %>%
  st_set_crs("+init=epsg:32619") %>%
  st_transform("+init=epsg:4226") %>%
  as_Spatial() %>%
  as.data.frame() %>%
  rename(`Start Longitude` = coords.x1, `Start Latitude` = coords.x2,
         `Start Depth (m)` = START_DEPTH,	`End Depth (m)`=END_DEPTH,
         `Visibility (m)` = VISIBILITY_M, `Temperature (C)` = WATER_TEMP) %>%
  mutate(`End Latitude`="",	`End Longitude`="",
         `Temperature (C)` = (`Temperature (C)`-32)*5/9)

write_csv(sites, "../FtWeatherill_Site_info_2017.csv")

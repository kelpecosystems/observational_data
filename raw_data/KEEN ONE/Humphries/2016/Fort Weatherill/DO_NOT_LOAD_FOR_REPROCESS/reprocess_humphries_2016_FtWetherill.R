######################################################################
#' Humphries Lab used nonstandard format for 2016 Ft. Wetherill
#' This should fix
#' 
######################################################################

library(tidyverse)
library(lubridate)
library(readxl)

#### Fish ####
fish <- read_excel("./FtWeatherill_fish_2016.xlsx") %>%
  spread(SIZE, COUNT) %>%
  rename(`0-10 cm` = `0_10`, 
         `10-50 cm` = `10_50`,
         `50-100 cm` = `50_100`) %>%
  mutate(YOY=NA,
         `>100 cm` = NA)

write_excel_csv(fish, "../FtWeatherill_fish_2016_reprocessed.csv", na="")
#xlsx::write.xlsx(fish, "../FtWeatherill_fish_2016_reprocessed.xlsx", showNA=FALSE)

#### Quad ####
quad <- read_excel("./FtWeatherill_QUAD_2016_TALBENHORIN.xlsx") %>%
  mutate(QUAD = paste(QUAD_NO, SIDE, sep=" ")) %>%
  select(-QUAD_NO, -SIDE) %>%
  mutate(DAY = day(DATE)) %>%
  spread(QUAD, COUNT)

write_excel_csv(quad, "../FtWeatherill_QUAD_2016_TALBENHORIN_reprocessed.csv", na="")
	
#### Swath ####
swath <- read_excel("./FtWeatherill_Swath_2016.xlsx") %>%
  mutate(SIDE = gsub("IN 20", "0-20 IN", SIDE),
         SIDE = gsub("IN 40", "20-40 IN", SIDE),
         SIDE = gsub("ON 20", "0-20 IN", SIDE),
         SIDE = gsub("OFF 20", "20-0 OFF", SIDE),
         SIDE = gsub("OFF 40", "40-20 OFF", SIDE)
  ) %>%
  spread(SIDE, COUNT)

write_excel_csv(swath, "../FtWeatherill_Swath_2016_reprocessed.csv", na="")

#### UPC ####
#looks like they only did one record per point
upc_ct <- read_excel("./FtWeatherill_UPC_2016_CT.xlsx", na=".") %>%
  rename(SP_CODE_0 = SP_CODE) %>%
  mutate(SP_CODE_1 = NA,
         SP_CODE_2 = NA,
         SP_CODE_3 = NA,
         SP_CODE_4 = NA,
         SP_CODE_5 = NA,
         DAY = day(DATE)
  )

write_excel_csv(upc_ct, "../FtWeatherill_UPC_2016_CT_reprocessed.csv", na="")

upc <- read_excel("./FtWeatherill_UPC_2016.xlsx", na=".") %>%
  mutate(SP_CODE_0 = NA,
         SP_CODE_4 = NA,
         SP_CODE_5 = NA
  ) %>%
  fill(`DEPTH @ 0 M`,	`DEPTH @ 40 M`,	VISIBILITY)
  
write_excel_csv(upc, "../FtWeatherill_UPC_2016.xls_reprocessed.csv", na="")



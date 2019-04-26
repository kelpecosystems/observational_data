library(tidyverse)
library(readxl)

upc <- read_excel("./Benthic_UPC_2015.xlsx")

upc_wide <- upc %>%
  filter(`SP CODE`!="SUBSTRATE") %>%
  group_by(YEAR, MONTH, DAY, DATE, `DISTANCE (m)`, SIDE, TRANSECT,  NOTES, `ENTERED BY`, `CHECKED BY`) %>%
  mutate(ENTRY = 1:n()) %>%
  ungroup() %>%
  mutate(ENTRY = str_c("SP_CODE_", ENTRY-1)) %>%
  spread(ENTRY, `SP CODE`, fill = "") %>%
  mutate(SP_CODE_3 = "",
         SP_CODE_4 = "",
         SP_CODE_5 = "",
  ) %>%
  rename(SUBSTRTE = SUBSTRATE)

write_csv(upc_wide, "../Benthic_UPC_2015.csv")

###############################################################
# QC FOR INTEGRATED DATA 
# 
# updataed Feb 16, 2017 by Tori
###############################################################
library(readxl)
library(dplyr)
library(ggplot2)

# Updated species list 
sp_list <- read_excel("../species_lists/KEEN ONE New England Species List for code verification.xlsx")
sp_list <- sp_list[,1:13]
sp_list$UPC <- as.numeric(sp_list$UPC)
sp_list$Swath <- as.numeric(sp_list$Swath)

quads_int <- read.csv("../integrated_data/quads_integrated.csv")
quads_int <- quads_int[,3:25]

swath_int <- read.csv("../integrated_data/swath_integrated.csv")
swath_int <- swath_int[,3:25]

fish_int <- read.csv("../integrated_data/fish_integrated.csv")
fish_int <- fish_int[,3:25]

cover_int <- read.csv("../integrated_data/cover_integrated.csv")
cover_int <- cover_int[,3:22]


# prep data frames to be joined 
cover_select <- cover_int %>%
  select(YEAR, SITE, TRANSECT, SP_CODE, PERCENT_COVER, GROUP, DIVISION.FAMILY, COMMON.DIVISION.NAME, COMMON.NAME, KINGDOM, PHYLUM, CLASS, ORDER, FAMILY, GENUS, SPECIES)

quads_select <- quads_int %>%
  select(YEAR, SITE, TRANSECT, SP_CODE, COUNT, GROUP, DIVISION.FAMILY, COMMON.DIVISION.NAME, COMMON.NAME, KINGDOM, PHYLUM, CLASS, ORDER, FAMILY, GENUS, SPECIES) %>%
  rename(QUADS_COUNT = COUNT)

fish_select <- fish_int %>%
  select(YEAR, SITE, TRANSECT, SP_CODE, COUNT, GROUP, DIVISION.FAMILY, COMMON.DIVISION.NAME, COMMON.NAME, KINGDOM, PHYLUM, CLASS, ORDER, FAMILY, GENUS, SPECIES) %>%
  rename(FISH_COUNT = COUNT)

swath_select <- swath_int %>%
  select(YEAR, SITE, TRANSECT, SP_CODE, COUNT, GROUP, DIVISION.FAMILY, COMMON.DIVISION.NAME, COMMON.NAME, KINGDOM, PHYLUM, CLASS, ORDER, FAMILY, GENUS, SPECIES) %>%
  rename(SWATH_COUNT = COUNT)


# join all protocols
join1 <- full_join(cover_select, quads_select, by=c("YEAR", "SITE", "TRANSECT", "SP_CODE", "GROUP", "DIVISION.FAMILY", "COMMON.DIVISION.NAME", "COMMON.NAME", "KINGDOM", "PHYLUM", "CLASS", "ORDER", "FAMILY", "GENUS", "SPECIES"))

join2 <- full_join(join1, swath_select, by=c("YEAR", "SITE", "TRANSECT", "SP_CODE", "GROUP", "DIVISION.FAMILY", "COMMON.DIVISION.NAME", "COMMON.NAME", "KINGDOM", "PHYLUM", "CLASS", "ORDER", "FAMILY", "GENUS", "SPECIES"))

merged_data <- full_join(join2, fish_select, by=c("YEAR", "SITE", "TRANSECT", "SP_CODE", "GROUP", "DIVISION.FAMILY", "COMMON.DIVISION.NAME", "COMMON.NAME", "KINGDOM", "PHYLUM", "CLASS", "ORDER", "FAMILY", "GENUS", "SPECIES"))

#clean up
merged_data <- merged_data %>%
  filter(!is.na(SITE))

gathered_data <- merged_data %>%
  group_by(YEAR, SITE, TRANSECT) %>%
  gather(key=PROTOCOL, value=COUNT, PERCENT_COVER, FISH_COUNT, SWATH_COUNT, QUADS_COUNT)



##### QUALITY CONTROL CHECKS ####

# CHECK 1: four transects for each site in each year
check_1 <- merged_data %>%
  group_by(YEAR, SITE) %>%
  summarise(total_transects = length(unique(TRANSECT))) %>%
  filter(total_transects != 4)

# CHECK 2: have quad, swath, fish, site, temp, and point count data for each transect in each site in each year
###### FINISH WHEN HAVE SITE INFO, AND TEMP ######### *****************
check_2 <- gathered_data %>%
  group_by(YEAR, SITE, TRANSECT) %>%
  summarise(protocols = length(unique(PROTOCOL)))

# CHECK 3: no negative numbers (data is sensible)
check_3 <- gathered_data %>%
  filter(COUNT < 0)


# CHECK 4: no insane outlier abundances (data is sensible)
check_4 <- gathered_data %>%
  group_by(YEAR, SITE, TRANSECT, PROTOCOL) %>%
  filter(!is.na(COUNT)) %>%
  #define outliers (ANY FORMULA MAY BE SUBSTITUTED TO CALCULATE UPPER AND LOWER LIMITS AS LONG AS "upper" and "lower" COLUMN NAMES ARE NOT CHANGED)
  summarise(IQR = IQR(COUNT), mean = mean(COUNT),  upper = ((1.5*IQR)+mean), lower = (mean-(1.5*IQR)))

  # combine upper and lower limit values with original data frame to compare
check_4 <- full_join(gathered_data, check_4, by=c("YEAR", "SITE", "TRANSECT", "PROTOCOL"))

check_4 <- check_4 %>%
  mutate(outlier = ifelse(COUNT > upper, "high_outlier",
                          ifelse(COUNT < lower, "low_outlier", "reasonable"))) %>%
  filter(outlier != "reasonable") %>%
  mutate(amount_above = COUNT - upper, amount_below = lower - COUNT) %>%
  mutate(amount_above = replace(amount_above, which(amount_above < 0), NA)) %>%
  mutate(amount_below = replace(amount_below, which(amount_below < 0), NA))


# CHECK 5: no characters where there should be numbers
###### JARRETT DO THIS
 



# QUAD QC #
# taxonomic info for each species
quads_full <- full_join(quads_int, sp_list, by="SP_CODE") #match species with list of data
str(quads_full)

quads_taxa <- quads_full %>%
  filter(is.na(Kingdom))

# taxa codes match taxa codes we have for that region
quads_region <- quads_full %>%
  filter(Quads == 0) %>% #what is not expeted
  filter(COUNT >= 1) %>% #and was recorded
  filter(!is.na(SP_CODE)) #and not blank



# SWATH QUALITY CONTROL #
swath_full <- full_join(swath_int, sp_list, by="SP_CODE") #match species with list of data

swath_region <- swath_full %>%
  filter(Swath == 0) %>%
  filter(COUNT >= 1) %>%
  filter(!is.na(SP_CODE))



# FISH QC #

fish_full <- full_join(fish_int, sp_list, by="SP_CODE") #match species with list of data

# species unexpected to be observed based on location
fish_region <- fish_full %>%
  filter(Fish == 0) %>% #what is not expeted
  filter(COUNT >= 1) %>% #and was recorded
  filter(!is.na(SP_CODE)) #and not blank




# COVER QUALITY CONTROL #
cover_full <- full_join(cover_int, sp_list, by="SP_CODE") #match species with list of data

# CHECK: taxa codes match taxa codes for that region
cover_region <- cover_full %>%
  filter(UPC == 0) %>% #what is not expeted
  filter(PERCENT_COVER >= 1) %>% #and was recorded
  filter(!is.na(SP_CODE)) #and not blank


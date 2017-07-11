
#####################################################################
# Read and Merge KEEN data ######
# updated Feb 16, 2017 by Tori
#####################################################################





######## 2014 ########


library(purrr)

# Load the functions for processing different kinds of KEEN data
source("./keen_processing_functions.R")

tl_dir <- "../raw_data/"
files <- paste0(tl_dir, list.files(tl_dir, recursive=TRUE))

# Who are we not working on
files <- files[!grepl("Grabowski", files)]
files <- files[!grepl("Dijkstra", files)]
files <- files[!grepl("Solomon", files)]
files <- files[!grepl("2015", files)]
files <- files[!grepl("2016", files)]


####
# make merged data sets
####

# Data cleaning while merging
quads <- map_df(files[grepl("[q,Q]uad", files)], ~process_quad(read_keen_data(.x, debug=T))) %>%
  mutate(SP_CODE = gsub("CAIRS", "CAIS", SP_CODE)) %>% #early error
  mutate(SP_CODE = gsub("BLB", "BLD", SP_CODE)) %>% #common typo
  mutate(SP_CODE = gsub("SPS", "SDS", SP_CODE)) %>% #common typo
  filter(SP_CODE != "Shrimp") %>% #common typo
  mutate(SP_CODE = gsub("COFR", "COF", SP_CODE)) %>% #early mistake
  mutate(SP_CODE = gsub("ASNO", "ASDI", SP_CODE)) %>% #code re-write
  mutate(SP_CODE = gsub("SAHY", "HEAM", SP_CODE)) %>% #code re-write 
  mutate(SITE = toupper(SITE)) %>% #make all site names uppercase
  mutate(SITE = trimws(SITE)) %>%
  mutate(SITE = gsub(" ", "_", SITE)) %>% #and no spaces, all underscores
  filter(!is.na(COUNT))

swath <- map_df(files[grepl("[s,S]wath", files)], ~process_swath(read_keen_data(.x, debug=T))) %>%
  mutate(SITE = toupper(SITE)) %>% #make all site names uppercase
  mutate(SITE = gsub(" ", "_", SITE)) #and no spaces, all underscores

fish <- map_df(files[grepl("[f,F]ish", files)], ~process_fish(read_keen_data(.x, debug=T))) %>%
  mutate(SITE = toupper(SITE)) %>% #make all site names uppercase
  mutate(SITE = gsub(" ", "_", SITE)) #and no spaces, all underscores

cover <- map_df(files[grepl("upc", files)], ~process_pointcount(read_keen_data(.x, debug=T))) %>%
  mutate(SP_CODE = gsub("HYRU", "HIRU", SP_CODE)) %>%
  mutate(SP_CODE = gsub("COCO", "COTR", SP_CODE)) %>%
  mutate(SP_CODE = gsub("PISE", "POLS", SP_CODE)) %>%
  mutate(SP_CODE = gsub("PILS", "POLS", SP_CODE)) %>%
  mutate(TRANSECT = trimws(TRANSECT)) %>%
  mutate(SITE = toupper(SITE)) %>% #make all site names uppercase
  mutate(SITE = gsub(" ", "_", SITE)) #and no spaces, all underscores


####
# merge each data type with the species list
####

spList <- read.csv("../derived_data/merged_sp_list_keenone.csv", stringsAsFactors=FALSE)
quadsWithSp <- left_join(quads, spList, by=c("SP_CODE" = "SPECIES.CODE"))
swathWithSp <- left_join(swath, spList, by=c("SP_CODE" = "SPECIES.CODE"))
fishWithSp <- left_join(fish, spList, by=c("SP_CODE" = "SPECIES.CODE"))

# remember to deal with substrate in cover
coverWithSp <- left_join(cover, spList, by=c("SP_CODE" = "SPECIES.CODE")) %>%
  mutate(GROUP.y = ifelse(GROUP.x=="SUBSTRATE", "Substrate", GROUP.y)) %>%
  select(-GROUP.x) %>% rename(GROUP = GROUP.y)


####
# Reorder some columns
####

quadsWithSp <- quadsWithSp %>%
  select(YEAR, MONTH, DAY, SITE, TRANSECT, 
         SP_CODE, QUAD, SIDE, COUNT, AREA, GROUP, DIVISION.FAMILY, 
         COMMON.DIVISION.NAME, SIZE, COMMON.NAME, KINGDOM, PHYLUM, 
         CLASS, ORDER, FAMILY, GENUS, SPECIES, NOTES)

swathWithSp <- swathWithSp %>%
  select(YEAR, MONTH, DAY, SITE, TRANSECT, 
         SP_CODE, QUAD, SIDE, COUNT, AREA, GROUP, DIVISION.FAMILY, 
         COMMON.DIVISION.NAME, SIZE, COMMON.NAME, KINGDOM, PHYLUM, 
         CLASS, ORDER, FAMILY, GENUS, SPECIES, NOTES)

fishWithSp <- fishWithSp %>%
  select(YEAR, MONTH, DAY, SITE, TRANSECT, 
         SP_CODE, QUAD, SIDE, SIZE.x, COUNT, AREA, GROUP, DIVISION.FAMILY, 
         COMMON.DIVISION.NAME, SIZE.y, COMMON.NAME, KINGDOM, PHYLUM, 
         CLASS, ORDER, FAMILY, GENUS, SPECIES) %>%
  rename(FISH.SIZE = SIZE.x, SIZE = SIZE.y)

coverWithSp <- coverWithSp %>%
  select(YEAR, MONTH, DAY, SITE, TRANSECT, SIDE, SP_CODE, 
         PERCENT_COVER, GROUP, DIVISION.FAMILY, COMMON.DIVISION.NAME, SIZE, 
         COMMON.NAME, KINGDOM, PHYLUM, CLASS, ORDER, FAMILY, 
         GENUS, SPECIES)

write.csv(quadsWithSp, "../derived_data/keen_quads_2014.csv")
write.csv(swathWithSp, "../derived_data/keen_swath_2014.csv")
write.csv(fishWithSp, "../derived_data/keen_fish_2014.csv")
write.csv(coverWithSp, "../derived_data/keen_cover_2014.csv")





######## 2015 ########


files <- paste0(tl_dir, list.files(tl_dir, recursive=TRUE))


# Who are we not working on
files <- files[!grepl("Grabowski", files)]
files <- files[!grepl("Dijkstra", files)]
files <- files[!grepl("Solomon", files)]
files <- files[!grepl("2014", files)]
files <- files[!grepl("2016", files)]

####
# make merged data sets
####


# Data cleaning while merging
quads <- map_df(files[grepl("[q,Q]uad", files)], ~process_quad(read_keen_data(.x, debug=T))) %>%
  mutate(SP_CODE = gsub("CAIRS", "CAIS", SP_CODE)) %>% #early error
  mutate(SP_CODE = gsub("BLB", "BLD", SP_CODE)) %>% #common typo
  mutate(SP_CODE = gsub("SPS", "SDS", SP_CODE)) %>% #common typo
  filter(SP_CODE != "Shrimp") %>% #common typo
  mutate(SP_CODE = gsub("COFR", "COF", SP_CODE)) %>% #early mistake
  mutate(SP_CODE = gsub("ASNO", "ASDI", SP_CODE)) %>% #code re-write
  mutate(SP_CODE = gsub("SAHY", "HEAM", SP_CODE)) %>% #code re-write
  mutate(SP_CODE = gsub("CABOS", "CABO", SP_CODE)) %>% #typo
  filter(!is.na(COUNT))

swath <- map_df(files[grepl("[s,S]wath", files)], ~process_swath(read_keen_data(.x, debug=T)))

fish <- map_df(files[grepl("[f,F]ish", files)], ~process_fish(read_keen_data(.x, debug=T)))

cover <- map_df(files[grepl("upc", files)], ~process_pointcount(read_keen_data(.x, debug=T))) %>%
  mutate(SP_CODE = gsub("CLCR", "CHCR", SP_CODE)) %>%
  mutate(SP_CODE = gsub("SJ", "SLJ", SP_CODE)) %>%
  mutate(SP_CODE = gsub("SCAL", "SCAU", SP_CODE)) %>%
  mutate(SP_CODE = gsub("ANOM", "ANSP", SP_CODE)) #%>%
 # mutate(SP_CODE = gsub("BARE", "", SP_CODE))


####
#merge each data type with the species list
####

spList <- read.csv("../derived_data/merged_sp_list_keenone.csv", stringsAsFactors=FALSE)
quadsWithSp <- left_join(quads, spList, by=c("SP_CODE" = "SPECIES.CODE"))
swathWithSp <- left_join(swath, spList, by=c("SP_CODE" = "SPECIES.CODE"))
fishWithSp <- left_join(fish, spList, by=c("SP_CODE" = "SPECIES.CODE"))

# remember to deal with substrate in cover
coverWithSp <- left_join(cover, spList, by=c("SP_CODE" = "SPECIES.CODE")) %>%
  mutate(GROUP.y = ifelse(GROUP.x=="SUBSTRATE", "Substrate", GROUP.y)) %>%
  select(-GROUP.x) %>% rename(GROUP = GROUP.y)

####
# Reorder some columns
####

quadsWithSp <- quadsWithSp %>%
  select(YEAR, MONTH, DAY, SITE, TRANSECT, 
         SP_CODE, QUAD, SIDE, COUNT, AREA, GROUP, DIVISION.FAMILY, 
         COMMON.DIVISION.NAME, SIZE, COMMON.NAME, KINGDOM, PHYLUM, 
         CLASS, ORDER, FAMILY, GENUS, SPECIES, NOTES)

swathWithSp <- swathWithSp %>%
  select(YEAR, MONTH, DAY, SITE, TRANSECT, 
         SP_CODE, QUAD, SIDE, COUNT, AREA, GROUP, DIVISION.FAMILY, 
         COMMON.DIVISION.NAME, SIZE, COMMON.NAME, KINGDOM, PHYLUM, 
         CLASS, ORDER, FAMILY, GENUS, SPECIES, NOTES)

fishWithSp <- fishWithSp %>%
  select(YEAR, MONTH, DAY, SITE, TRANSECT, 
         SP_CODE, QUAD, SIDE, SIZE.x, COUNT, AREA, GROUP, DIVISION.FAMILY, 
         COMMON.DIVISION.NAME, SIZE.y, COMMON.NAME, KINGDOM, PHYLUM, 
         CLASS, ORDER, FAMILY, GENUS, SPECIES) %>%
  rename(FISH.SIZE = SIZE.x, SIZE = SIZE.y)

coverWithSp <- coverWithSp %>%
  select(YEAR, MONTH, DAY, SITE, TRANSECT, SIDE, SP_CODE, 
         PERCENT_COVER, GROUP, DIVISION.FAMILY, COMMON.DIVISION.NAME, SIZE, 
         COMMON.NAME, KINGDOM, PHYLUM, CLASS, ORDER, FAMILY, 
         GENUS, SPECIES)

write.csv(quadsWithSp, "../derived_data/keen_quads_2015.csv")
write.csv(swathWithSp, "../derived_data/keen_swath_2015.csv")
write.csv(fishWithSp, "../derived_data/keen_fish_2015.csv")
write.csv(coverWithSp, "../derived_data/keen_cover_2015.csv")





######## 2016 ########


files <- paste0(tl_dir, list.files(tl_dir, recursive=TRUE))

# Who are we not working on
files <- files[!grepl("Grabowski", files)]
files <- files[!grepl("Dijkstra", files)]
files <- files[!grepl("Solomon", files)]
files <- files[!grepl("2014", files)]
files <- files[!grepl("2015", files)]

####
# make merged data sets
####

# Data cleaning while merging
quads <- map_df(files[grepl("[q,Q]uad", files)], ~process_quad(read_keen_data(.x, debug=T))) %>%
  mutate(SP_CODE = gsub("CAIRS", "CAIS", SP_CODE)) %>% #early error
  mutate(SP_CODE = gsub("BLB", "BLD", SP_CODE)) %>% #common typo
  mutate(SP_CODE = gsub("SPS", "SDS", SP_CODE)) %>% #common typo
  filter(SP_CODE != "Shrimp") %>% #common typo
  mutate(SP_CODE = gsub("COFR", "COF", SP_CODE)) %>% #early mistake
  mutate(SP_CODE = gsub("ASNO", "ASDI", SP_CODE)) %>% #code re-write
  mutate(SP_CODE = gsub("SAHY", "HEAM", SP_CODE)) %>% #code re-write
  mutate(SP_CODE = gsub("CUNNER", "TAAD", SP_CODE)) %>% #error
  mutate(SP_CODE = gsub("SDJ", "SDS", SP_CODE)) %>% #error
  filter(!is.na(COUNT))

length(unique(quads$YEAR))


swath <- map_df(files[grepl("[s,S]wath", files)], ~process_swath(read_keen_data(.x, debug=T)))

fish <- map_df(files[grepl("[f,F]ish", files)], ~process_fish(read_keen_data(.x, debug=T)))

cover <- map_df(files[grepl("upc", files)], ~process_pointcount(read_keen_data(.x, debug=T))) %>%
  mutate(SP_CODE = gsub("BUSUP", "BUSP", SP_CODE)) %>% #typo
  mutate(SP_CODE = gsub("CHCHR", "CHCR", SP_CODE)) %>% #typo
  mutate(SP_CODE = gsub("CYHPU", "CYPU", SP_CODE)) %>% #typo
  mutate(SP_CODE = gsub("SJ", "SLJ", SP_CODE)) %>% #typo
  mutate(SP_CODE = gsub("CLCR", "CHCR", SP_CODE)) %>% #typo
  mutate(SP_CODE = gsub("DO", "CO", SP_CODE)) %>% #typo
  mutate(SP_CODE = gsub("PRE", "SPRE", SP_CODE)) %>% #typo
  mutate(SP_CODE = gsub("SSPRE", "SPRE", SP_CODE))

#######
# merge each data type with the species list
#######
spList <- read.csv("../derived_data/merged_sp_list_keenone.csv", stringsAsFactors=FALSE)
quadsWithSp <- left_join(quads, spList, by=c("SP_CODE" = "SPECIES.CODE"))
swathWithSp <- left_join(swath, spList, by=c("SP_CODE" = "SPECIES.CODE"))
fishWithSp <- left_join(fish, spList, by=c("SP_CODE" = "SPECIES.CODE"))

# remember to deal with substrate in cover
coverWithSp <- left_join(cover, spList, by=c("SP_CODE" = "SPECIES.CODE")) %>%
  mutate(GROUP.y = ifelse(GROUP.x=="SUBSTRATE", "Substrate", GROUP.y)) %>%
  select(-GROUP.x) %>% rename(GROUP = GROUP.y)

####
# Reorder some columns
####

quadsWithSp <- quadsWithSp %>%
  select(YEAR, MONTH, DAY, SITE, TRANSECT, 
         SP_CODE, QUAD, SIDE, COUNT, AREA, GROUP, DIVISION.FAMILY, 
         COMMON.DIVISION.NAME, SIZE, COMMON.NAME, KINGDOM, PHYLUM, 
         CLASS, ORDER, FAMILY, GENUS, SPECIES, NOTES)

swathWithSp <- swathWithSp %>%
  select(YEAR, MONTH, DAY, SITE, TRANSECT, 
         SP_CODE, QUAD, SIDE, COUNT, AREA, GROUP, DIVISION.FAMILY, 
         COMMON.DIVISION.NAME, SIZE, COMMON.NAME, KINGDOM, PHYLUM, 
         CLASS, ORDER, FAMILY, GENUS, SPECIES, NOTES)

fishWithSp <- fishWithSp %>%
  select(YEAR, MONTH, DAY, SITE, TRANSECT, 
         SP_CODE, QUAD, SIDE, SIZE.x, COUNT, AREA, GROUP, DIVISION.FAMILY, 
         COMMON.DIVISION.NAME, SIZE.y, COMMON.NAME, KINGDOM, PHYLUM, 
         CLASS, ORDER, FAMILY, GENUS, SPECIES) %>%
  rename(FISH.SIZE = SIZE.x, SIZE = SIZE.y)

coverWithSp <- coverWithSp %>%
  select(YEAR, MONTH, DAY, SITE, TRANSECT, SIDE, SP_CODE, 
         PERCENT_COVER, GROUP, DIVISION.FAMILY, COMMON.DIVISION.NAME, SIZE, 
         COMMON.NAME, KINGDOM, PHYLUM, CLASS, ORDER, FAMILY, 
         GENUS, SPECIES)

write.csv(quadsWithSp, "../derived_data/keen_quads_2016.csv")
write.csv(swathWithSp, "../derived_data/keen_swath_2016.csv")
write.csv(fishWithSp, "../derived_data/keen_fish_2016.csv")
write.csv(coverWithSp, "../derived_data/keen_cover_2016.csv")










######## MERGE ALL KEEN data ( YEARS 2014, 2015, 2016) ########

# fish (derived)
fish2014 <- read.csv("../derived_data/keen_fish_2014.csv")
fish2015 <- read.csv("../derived_data/keen_fish_2015.csv")
fish2016 <- read.csv("../derived_data/keen_fish_2016.csv")

# combine into new data frame
fish_INTEGRATED <- rbind(fish2014, fish2015, fish2016)

write.csv(fish_INTEGRATED, "../integrated_data/fish_integrated.csv")



# quads (derived)
quads2014 <- read.csv("../derived_data/keen_quads_2014.csv")
quads2015 <- read.csv("../derived_data/keen_quads_2015.csv")
quads2016 <- read.csv("../derived_data/keen_quads_2016.csv")

# combine into new data frame
quads_INTEGRATED <- rbind(quads2014, quads2015, quads2016)

write.csv(quads_INTEGRATED, "../integrated_data/quads_integrated.csv")



# cover (derived)
cover2014 <- read.csv("../derived_data/keen_cover_2014.csv")
cover2015 <- read.csv("../derived_data/keen_cover_2015.csv")
cover2016 <- read.csv("../derived_data/keen_cover_2016.csv")

# combine into new data frame
cover_INTEGRATED <- rbind(cover2014, cover2015, cover2016)

write.csv(cover_INTEGRATED, "../integrated_data/cover_integrated.csv")



# swath (derived)
swath2014 <- read.csv("../derived_data/keen_swath_2014.csv")
swath2015 <- read.csv("../derived_data/keen_swath_2015.csv")
swath2016 <- read.csv("../derived_data/keen_swath_2016.csv")

# combine into new data frame
swath_INTEGRATED <- rbind(swath2014, swath2015, swath2016)
 
write.csv(swath_INTEGRATED, "../integrated_data/swath_integrated.csv")


###### JARRETT CHECK SWATH TEMPLATE FOR HIDDEN COLUMN

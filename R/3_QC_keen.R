################################
#'
#'     Data quality control    
#'     For KEEN data
#'     
#' Checks each data source for completeness
#' and if it fails, writes the error out
#' 
#' @changelog
#' 2019-07-14: added kelp
#' 2018-04-14: added fix for bad fish size classes (-100 to >100)
################################

#### Load relevant libraries and helper code ####
library(tidyverse)
source("QC_KEEN_name_gsub.R")


#### Clear the old error logs ####
err_files <- list.files("../data_error_logs/", recursive=TRUE)

if(length(err_files)>0)
  file.remove(paste0("../data_error_logs/", err_files))

#### Read in the data, implementing fixes ####
  

quadsWithSp <- read.csv("../derived_data/keen_quads.csv", stringsAsFactors = FALSE) %>%
  name_gsub

swathWithSp <- read.csv("../derived_data/keen_swath.csv", stringsAsFactors = FALSE) %>%
  name_gsub

fishWithSp <- read.csv("../derived_data/keen_fish.csv", stringsAsFactors = FALSE) %>%
  name_gsub

coverWithSp <- read.csv("../derived_data/keen_cover.csv", stringsAsFactors = FALSE) %>%
  name_gsub


kelpWithSp <- read.csv("../derived_data/keen_kelp.csv", stringsAsFactors = FALSE) %>%
  name_gsub

sites <- read.csv( "../derived_data/keen_sites.csv", stringsAsFactors = FALSE) %>%
  name_gsub %>%
  mutate(START_LONGITUDE = ifelse(NETWORK=="KEEN ONE" & START_LONGITUDE>0, -1*START_LONGITUDE, START_LONGITUDE),
         END_LONGITUDE = ifelse(NETWORK=="KEEN ONE" & END_LONGITUDE>0, -1*END_LONGITUDE, END_LONGITUDE))

#### Check that there are no NAs in key fields ####



#### Check that each site in each year has requred data ####

## Which site has what
get_unique <- . %>%
  group_by(NETWORK, PI, YEAR, SITE, TRANSECT) %>%
  slice(1L)



quads_unique <- quadsWithSp %>% get_unique %>% summarize(PROTOCOL="QUAD")
swath_unique <- swathWithSp %>% get_unique %>% summarize(PROTOCOL="SWATH")
fish_unique <- fishWithSp %>% get_unique %>% summarize(PROTOCOL="FISH")
cover_unique <- coverWithSp %>% get_unique %>% summarize(PROTOCOL="UPC")
kelp_unique <- kelpWithSp %>% get_unique %>% summarize(PROTOCOL="KELP")

#put them together and see what is missing
unique_sites <- rbind(quads_unique, swath_unique, 
                      fish_unique, cover_unique,
                      kelp_unique) %>% ungroup()

#to see
#unique(unique_sites$SITE)
#unique(unique_sites$TRANSECT)

problem_sites <- unique_sites %>%
  group_by(PI, YEAR, SITE, TRANSECT) %>%
  summarize(PROTOCOLS = list(unique(PROTOCOL))) %>%
  mutate(n = map_dbl(PROTOCOLS, length)) %>%
  filter(n<5) 


protocols <- c("QUAD", "SWATH", "FISH", "UPC", "KELP")

missing_data <- problem_sites %>%
  mutate(missing = map_chr(PROTOCOLS, 
                       ~paste0(protocols[!(protocols %in% .x)], collapse = ","))) %>%
  select(-PROTOCOLS)%>%
  arrange(PI)

#checks
kelpWithSp %>% filter(YEAR==2015) %>% filter(PI=="Grabowski") %>%
  filter(TRANSECT == "Pumphouse Beach 3")

#write report out if needed
if(length(missing_data$SITE) > 0){
  write.csv(missing_data, "../data_error_logs/missing_protocols.csv", row.names=FALSE)
  print("FAIL: Missing data")
}else{
  print("PASS: Missing data check passed")
}

#### For each source, check that all samples are present ####

#6 quads per transect
quadSampsBad <- quadsWithSp %>%
  group_by(PI, YEAR, SITE, TRANSECT) %>%
  summarize(Sampled = n_distinct(paste(QUAD, SIDE))) %>%
  filter(Sampled !=6)  %>%
  mutate(`Should Be` = 6) %>%
  ungroup()


#4 swath per transect
swathSampBad <- swathWithSp  %>%
  group_by(PI, YEAR, SITE, TRANSECT) %>%
  summarize(Sampled = n_distinct(paste(QUAD, SIDE))) %>%
  filter(Sampled !=4)  %>%
  mutate(`Should Be` = 4) %>%
  ungroup()


#80 UPC per transect
coverSampBad <- coverWithSp  %>%
  filter(GROUP=="Substrate") %>%
  group_by(PI, YEAR, SITE, TRANSECT) %>%
  summarize(Sampled = sum(PERCENT_COVER)) %>%
  filter(Sampled != 100) %>%
  mutate(`Should Be` = 100) %>%
  ungroup()

missing_samps <- rbind(quadSampsBad, swathSampBad, coverSampBad) %>%
  arrange(PI)


#write report out if needed
if(length(missing_samps$SITE) > 0){
  write.csv(missing_samps, "../data_error_logs/missing_samps.csv", row.names=FALSE)
  print("FAIL: Missing samples")
}else{
  print("PASS: Missing sample check passed")
}



#### For each source, check that species are complete/valid ####
quadNoSp <- filter(quadsWithSp, is.na(COMMON.DIVISION.NAME)) %>%
  arrange(SP_CODE) %>%
  arrange(PI)


swathNoSp <- filter(swathWithSp, is.na(COMMON.DIVISION.NAME)) %>%
  arrange(SP_CODE)%>%
  arrange(PI)


fishNoSp <- filter(fishWithSp, is.na(COMMON.DIVISION.NAME)) %>%
  filter(COMMON.DIVISION.NAME != "NO_FISH") %>%
  arrange(SP_CODE)%>%
  arrange(PI)


coverNoSp <- filter(coverWithSp, is.na(GROUP)) %>%
  arrange(SP_CODE)%>%
  arrange(PI)


#write report out if needed
if(length(quadNoSp$SITE) > 0){
  write.csv(quadNoSp, "../data_error_logs/quadNoSp.csv", row.names=FALSE)
  print("FAIL: Bad species in quads")
}else{
  print("PASS: Species in Quads check passed")
}


#write report out if needed
if(length(swathNoSp$SITE) > 0){
  write.csv(swathNoSp, "../data_error_logs/swathNoSp.csv", row.names=FALSE)
  print("FAIL: Bad species in swath")
}else{
  print("PASS: Species in Swaths check passed")
}


#write report out if needed
if(length(fishNoSp$SITE) > 0){
  write.csv(fishNoSp, "../data_error_logs/fishNoSp.csv", row.names=FALSE)
  print("FAIL: Bad species in fish")
}else{
  print("PASS: Species in fish check passed")
}


#write report out if needed
if(length(coverNoSp$SITE) > 0){
  write.csv(coverNoSp, "../data_error_logs/coverNoSp.csv", row.names=FALSE)
  print("FAIL: Bad species in cover")
}else{
  print("PASS: Species in cover check passed")
}


#### Does each site have 4 transects? ####

#### Were depths really metric? ####
sites_depth_bad <- sites %>%
  filter(START_DEPTH_M>20) %>%
  rbind(sites %>%
          filter(END_DEPTH_M>20))


#write report out if needed
if(length(sites_depth_bad$END_DEPTH_M) > 0){
  write.csv(sites_depth_bad, "../data_error_logs/sites_depth_bad.csv", row.names=FALSE)
  print("FAIL: Some depths not metric")
  
  #fix bad sites in data
  sites <- sites %>%
    mutate(START_DEPTH_M = ifelse(START_DEPTH_M<20, START_DEPTH_M,
                                  START_DEPTH_M*0.3048),
           END_DEPTH_M = ifelse(END_DEPTH_M<20,END_DEPTH_M,
                                  END_DEPTH_M*0.3048)
    )
  
}else{
  print("PASS: Metric depth check passed")
}

#### Were vizes really metric? ####
#### Were temps really metric? ####
bad_temp <- sites %>%
  filter(TEMPERATURE_C > 40)

if(length(bad_temp$TEMPERATURE_C) > 0){
  write.csv(bad_temp, "../data_error_logs/bad_temp.csv", row.names=FALSE)
  print("FAIL: Some temps not metric")
  
  #fix bad sites in data
  sites <- sites %>%
    mutate(TEMPERATURE_C = ifelse(TEMPERATURE_C<40, TEMPERATURE_C,
                                (TEMPERATURE_C-32)*5/9))
  
}else{
  print("PASS: Metric temps check passed")
}

#### Are the transects in each site close together? ####

sites_summary <- sites %>%
  group_by(PI, SITE) %>%
  summarise(START_LONGITUDE = mean(START_LONGITUDE, na.rm=T),
            START_LATITUDE = mean(START_LATITUDE, na.rm=T),
            n_years = length(unique(YEAR)),
            sd_lat_start = sd(START_LATITUDE, na.rm=T),
            sd_lon_start = sd(START_LONGITUDE, na.rm=T),
            sd_lat_end = sd(END_LATITUDE, na.rm=T),
            sd_lon_end = sd(END_LONGITUDE, na.rm=T),
            
  )

bad_latlong_maybe <- sites_summary %>% 
  filter(sd_lat_start > 0.1 | 
           sd_lon_start > 0.1 |
           sd_lat_end > 0.1 |
           sd_lon_end > 0.1)

if(length(bad_latlong_maybe$sd_lon_start) > 0){
  write.csv(bad_latlong_maybe, "../data_error_logs/bad_latlong_maybe.csv", row.names=FALSE)
  print("FAIL: Some latlongs are incorrect")
}else{
  print("PASS: Latlong consistency check passed")
}


#fix fish size class error
fishWithSp$FISH.SIZE <- gsub("^-100", ">100", fishWithSp$FISH.SIZE)

#### Write out cleaned data ####

write.csv(quadsWithSp, "../cleaned_data/keen_quads.csv", row.names=FALSE)
write.csv(swathWithSp, "../cleaned_data/keen_swath.csv", row.names=FALSE)
write.csv(fishWithSp, "../cleaned_data/keen_fish.csv", row.names=FALSE)
write.csv(coverWithSp, "../cleaned_data/keen_cover.csv", row.names=FALSE)
write.csv(kelpWithSp, "../cleaned_data/keen_kelp.csv", row.names=FALSE)

write.csv(sites, "../cleaned_data/keen_sites.csv", row.names=FALSE)
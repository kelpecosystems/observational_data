######################################################################
#' Functions to process each file type from KEEN data
#' 
#' 
######################################################################

library(tidyverse)
library(readxl)

read_keen_data <- function(filename, sheet=1, debug=FALSE){
  if(debug) print(filename)
  
  adf <- read_excel(filename, sheet=sheet) 
  names(adf) <- toupper(names(adf))
 # if(debug) print(warnings())
  
  
  class(adf$TRANSECT) <- "character" #not everyone uses numbers
  
  names(adf) <- trimws(names(adf))
  names(adf) <- make.names(names(adf), unique=TRUE)
  names(adf) <- gsub("\\.", "_", names(adf))
  adf %>%
    filter(!is.na(YEAR)) %>% #kills blank rows
    filter(!is.na(SITE)) %>% #kills blank rows
    mutate(SITE = fix_sites(SITE),
           TRANSECT = fix_transects(TRANSECT),
           NETWORK = getOrg(filename),
           PI = getPI(filename))
}

process_quad <- function(adf){
  #fix those who have long quad data, like Dijkstra 2015 Nubble
  if("QUAD_NO" %in% names(adf)){
    adf <- mutate(adf, QUAD = str_extract(QUAD_NO, "[0-9]+"))
  }
  if("SIDE" %in% names(adf)){
    adf <- adf %>%
      mutate(SIDE = toupper(SIDE),
             QUAD = str_c("Q", QUAD, "_", SIDE)) %>%
      select(-SIDE) %>%
      spread(QUAD, COUNT)
  }
  
  
  #subset to selected columns
  adf %>%
    mutate(SP_CODE = gsub("CAIRS", "CAIS", SP_CODE)) %>% #early error
    mutate(SP_CODE = gsub("BLB", "BLD", SP_CODE)) %>% #common typo
    mutate(SP_CODE = gsub("SPS", "SDS", SP_CODE)) %>% #common typo
    filter(SP_CODE != "Shrimp") %>% #common typo
    mutate(SP_CODE = gsub("COFR", "COF", SP_CODE)) %>% #early mistake
    mutate(SP_CODE = gsub("ASNO", "ASDI", SP_CODE)) %>% #code re-write
    mutate(SP_CODE = gsub("SAHY", "HEAM", SP_CODE)) %>% #code re-write
    mutate(SP_CODE = gsub("CABOS", "CABS", SP_CODE)) %>% #typo from 2015
    mutate(SP_CODE = gsub("CUNNER", "TAAD", SP_CODE)) %>% #error from 2016
    mutate(SP_CODE = gsub("SDJ", "SDS", SP_CODE)) %>% #error from 2016
    select(NETWORK, PI, YEAR, MONTH, DAY, SITE, TRANSECT, SP_CODE, 
           Q0_OFF, Q8_ON, Q16_OFF, Q24_ON, Q32_OFF, Q40_ON, NOTES) %>%
    #make long
    gather(QUAD, COUNT, Q0_OFF, Q8_ON, Q16_OFF, Q24_ON, Q32_OFF, Q40_ON) %>%
    separate(QUAD, c("QUAD", "SIDE"), sep="_") %>%
    mutate(QUAD = gsub("Q", "", QUAD)) %>%
    mutate(SIDE = gsub("ON", "I", SIDE)) %>%
    mutate(SIDE = gsub("OFF", "O", SIDE))  %>%
    mutate(AREA = 1) %>%
    group_by(NETWORK, PI, YEAR, MONTH, DAY, SITE, TRANSECT, SP_CODE,
             QUAD, SIDE, AREA) %>% #dealing with duplicate row from diver error
    summarise(COUNT = sum(COUNT, na.rm=T),
              NOTES = paste(NOTES, collapse=",")) %>%
    ungroup()
}

# filename <- "../raw_data/KEEN ONE/Byrnes/2014/SW Shoals Entry 2014/SW Shoals 2014 Quad Entry.xlsx"
# adf <- read_keen_data(filename)
# head(process_quads(adf))

process_fish <- function(adf){
  
  #in case of old template
  if("SIZE__CM_" %in% names(adf)){
    adf <- adf %>%
      mutate( SIZE__CM_ = toupper(SIZE__CM_),
              SIZE__CM_ = str_replace_all(SIZE__CM_, "-", "_"),
              SIZE__CM_ = str_replace_all(SIZE__CM_, "$", "_CM"),
              SIZE__CM_ = str_replace_all(SIZE__CM_, "\\>", "_"),
              SIZE__CM_ = str_replace_all(SIZE__CM_, "^", "X"),
      ) %>%
      spread(SIZE__CM_, COUNT)
    cols <- c("YOY", "X0_10_CM", "X10_50_CM", "X50_100_CM", "X_100_CM")
    cols <- cols[!( cols %in% names(adf))]
    
    #fix missing columns
    if(length(cols)>0){
      adf[,cols]<-0
    }
  }
  
  adf %>%
    select(NETWORK, PI, YEAR, MONTH, DAY, SITE, TRANSECT, SP_CODE,
           YOY, X0_10_CM, X10_50_CM, X50_100_CM, X_100_CM) %>%
    mutate(QUAD = NA, SIDE = NA) %>%
    gather(SIZE, COUNT, YOY, X0_10_CM, X10_50_CM, X50_100_CM, X_100_CM) %>%
    mutate(SIZE = gsub("_CM","", SIZE)) %>%
    mutate(SIZE = gsub("X","", SIZE)) %>%
    mutate(SP_CODE = gsub("NO FISH","NO_FISH", SP_CODE)) %>%
    mutate(SIZE = gsub("$_",">", SIZE)) %>%
    mutate(SIZE = gsub("_","-", SIZE)) %>%
    #DEAL WITH NO FISH CODE
    mutate(COUNT = ifelse(SP_CODE=="NO FISH", 0, COUNT)) %>%
    mutate(COUNT = ifelse(SP_CODE=="NO_FISH", 0, COUNT)) %>%
    filter(!is.na(COUNT)) %>%
    mutate(AREA = 80)
  
}

# filename <- '../Data/Verified Data/2014/Baker South Entry 2014/Baker South Fish 2014.xlsx'
# adf <- read_keen_data(filename)
# head(process_fish(adf))


process_swath <- function(adf){

  if("SIDE" %in% names(adf)){
    adf <- adf %>%
      mutate(SIDE = toupper(SIDE),
             SIDE = str_remove(SIDE, "SHORE"),
             SIDE = str_replace_all(SIDE, "[ ,-]", "_"),
             SIDE = str_replace_all(SIDE, "_M_", "_"),
             SIDE = str_replace_all(SIDE, "^", "X"),
      ) %>%
      spread(SIDE, COUNT)
  }
  
  
  adf %>%
    select(NETWORK, PI, YEAR, MONTH, DAY, SITE, TRANSECT, SP_CODE, 
           X0_20_IN, X20_40_IN, X40_20_OFF, X20_0_OFF, NOTES) %>%
    #make long
    gather(QUAD, COUNT, X0_20_IN, X20_40_IN, X40_20_OFF, X20_0_OFF) %>%    
    mutate(QUAD = gsub("0_([A-Z])", "0__\\1", QUAD)) %>%
    separate(QUAD, c("QUAD", "SIDE"), sep="__") %>%
    mutate(QUAD = gsub("X", "", QUAD)) %>%
    mutate(QUAD = gsub("_", "-", QUAD)) %>%
    #mutate(QUAD = gsub("^", "0^", QUAD)) %>%
    mutate(SIDE = gsub("IN", "I", SIDE)) %>%
    mutate(SIDE = gsub("OFF", "O", SIDE)) %>%
    mutate(SP_CODE = gsub("MYT", "MYSP", SP_CODE)) %>%
    mutate(SP_CODE = gsub("HAOM", "HOAM", SP_CODE)) %>%
    mutate(SP_CODE = gsub("USP", "ULSP", SP_CODE)) %>%
    mutate(SP_CODE = gsub("HOMO", "HOAM", SP_CODE)) %>%
    mutate(SP_CODE = gsub("ARSU", "ASRU", SP_CODE)) %>%
    mutate(SP_CODE = gsub("TAADS", "TAAD", SP_CODE)) %>%
    mutate(SP_CODE = gsub("CYLU ", "CYLU", SP_CODE)) %>%
    mutate(SP_CODE = gsub("PSSP ", "PHSP", SP_CODE)) %>%
    mutate(SP_CODE = gsub("BUUC ", "BUCA", SP_CODE)) %>%
    mutate(AREA = 20) %>%
    group_by(NETWORK, PI, YEAR, MONTH, DAY, SITE, TRANSECT, SP_CODE, 
                    QUAD, SIDE, AREA)%>%
    summarise(COUNT = sum(COUNT, na.rm=T), #as 2 divers often do swath
              NOTES = paste(NOTES, collapse=",")) %>%
    ungroup()
}

# filename <- '../Data/Verified Data/2014/Baker South Entry 2014/Baker South Swaths  2014.xlsx'
# adf <- read_keen_data(filename)
# head(process_swath(adf))


process_pointcount <- function(adf){
  adf %>%
    select(NETWORK, PI, YEAR, MONTH, DAY, SITE, TRANSECT, SIDE, 
           SP_CODE_0, SP_CODE_1, SP_CODE_3, SP_CODE_4, SP_CODE_5,
           SUBSTRTE,NOTES) %>%
    gather(RECORD, SP_CODE, SP_CODE_0, SP_CODE_1, SP_CODE_3, SP_CODE_4, SP_CODE_5,
           SUBSTRTE) %>%  
    mutate(GROUP = ifelse(RECORD == "SUBSTRTE", "SUBSTRATE", "LIVING")) %>%
    select(-RECORD) %>%
    mutate(SP_CODE = gsub("BUSUP", "BUSP", SP_CODE)) %>% #typo
    mutate(SP_CODE = gsub("CHCHR", "CHCR", SP_CODE)) %>% #typo
    mutate(SP_CODE = gsub("CYHPU", "CYPU", SP_CODE)) %>% #typo
    mutate(SP_CODE = gsub("SJ", "SLJ", SP_CODE)) %>% #typo
    mutate(SP_CODE = gsub("CLCR", "CHCR", SP_CODE)) %>% #typo
    mutate(SP_CODE = gsub("DO", "CO", SP_CODE)) %>% #typo
    mutate(SP_CODE = gsub("PRE", "SPRE", SP_CODE)) %>% #typo
    mutate(SP_CODE = gsub("SSPRE", "SPRE", SP_CODE)) %>% #typo
    filter(SP_CODE != "-")  %>% #an NA string used improperly
    group_by(NETWORK, PI, YEAR, MONTH, DAY, SITE, TRANSECT, SP_CODE, GROUP) %>%
    summarise(PERCENT_COVER = 100*length(SP_CODE)/80) %>%
    ungroup() %>%
    filter(!is.na(SP_CODE))
    
    
}
# 
# filename <- "../raw_data/KEEN ONE/Dijkstra/2014/Nubble Light 2014/Nubble, Maine UPC 2014.xlsx"
# adf <- read_keen_data(filename)
# head(process_pointcount(adf))


process_subcanopy_sizedist <- function(adf){

}

process_temperature <- function(adf){
  
}

process_siteinfo <- function(adf){
  
  if("VISIBILITY__M_" %in% names(adf)){
    adf <- rename(adf, VISIBILITY_M = VISIBILITY__M_)
  }
  
  if("TEMPERATURE__C_" %in% names(adf)){
    adf <- rename(adf, TEMPERATURE_C = TEMPERATURE__C_)
  }
  
  if("END_DEPTH__M_" %in% names(adf)){
    adf <- rename(adf, END_DEPTH_M = END_DEPTH__M_)
  }
  
  if("START_DEPTH__M_" %in% names(adf)){
    adf <- rename(adf, START_DEPTH_M = START_DEPTH__M_)
  }
  
  if("END_DEPTH" %in% names(adf)){
    adf <- rename(adf, END_DEPTH_M = END_DEPTH)
  }
  
  if("START_DEPTH" %in% names(adf)){
    adf <- rename(adf, START_DEPTH_M = START_DEPTH)
  }
  
  #deal with some missing columns in 
  adf %>%
    select(NETWORK, PI, YEAR, MONTH, DAY, SITE, TRANSECT,
           START_LATITUDE, START_LONGITUDE,
           END_LATITUDE, END_LONGITUDE,
           START_DEPTH_M, END_DEPTH_M,
           TEMPERATURE_C,
           VISIBILITY_M)
  
}

#Some sites ae misspelled. This is the common function
#to fix typos
fix_sites <- function(siteCol){
  siteCol <- trimws(siteCol)
  siteCol <- gsub("South[W,w]est", "SW", siteCol)
  adf_site <- data.frame(SITE = siteCol) %>%
  mutate(SITE = gsub("BAKER_SOUTH", "Baker South", SITE)) %>%
    mutate(SITE = gsub("BAKER_NORTH", "Baker North", SITE)) %>%
    mutate(SITE = gsub("LITTLE_BREWSTER", "Little Brewster", SITE)) %>%
    mutate(SITE = gsub("Little Brewster Island", "Little Brewster", SITE)) %>%
    mutate(SITE = gsub("NE_APPLEDORE", "NE Appledore", SITE)) %>%
    mutate(SITE = gsub("NW_APPLEDORE", "NW Appledore", SITE)) %>%
    mutate(SITE = gsub("SW_APPLEDORE", "SW Appledore", SITE)) %>%
    mutate(SITE = gsub("CALF_ISLAND", "Calf Island", SITE)) %>%
    mutate(SITE = gsub("Canoe$", "Canoe Beach", SITE)) %>%
    mutate(SITE = gsub("\\, Nahant MA", "", SITE)) %>%
    mutate(SITE = gsub("Pemaquid, ME", "Pemaquid", SITE)) %>%
    mutate(SITE = gsub("Ft Wetherill", "Fort Weatherill", SITE)) %>%
    mutate(SITE = gsub("Ft. Weatherill", "Fort Weatherill", SITE)) %>%
    mutate(SITE = gsub("Ft.Weatherill", "Fort Weatherill", SITE)) %>%
    mutate(SITE = gsub("FTWE", "Fort Weatherill", SITE))
    
  
  adf_site$SITE
}


fix_transects <- function(transCol){
  transCol <- gsub("[t,T]ransect", "", transCol) #site info
  transCol <- trimws(transCol)
  transCol <- gsub("N Head", "North Head", transCol)
  transCol <- gsub("\\+[ ]*", "and ", transCol)
  transCol <- gsub("Pe[p]+e[r]+e[l]+", "Pepperrell", transCol)
  transCol <- gsub("Smith Cove", "Smith's Cove", transCol)
  transCol <- gsub("Norwegian$", "Norwegian Cove", transCol)
  transCol <- gsub("Larus$", "Larus Ledge", transCol)
  transCol <- gsub("^8-Ball$", "Magic 8 Ball", transCol)
  transCol <- gsub("^8\\. Ball$", "Magic 8 Ball", transCol)
  transCol <- gsub("^N ", "North ", transCol)
  transCol <- gsub("^S ", "South ", transCol)
  transCol <- gsub("^2 N ", "North ", transCol)
  transCol
}

getOrg <- function(afile){
  gsub("\\.\\.\\/raw_data\\/(.+?)\\/(.*)", "\\1", afile)
}

getPI <- function(afile){
  gsub("\\.\\.\\/raw_data\\/(.+?)\\/(.+?)\\/(.*)", "\\2", afile)
}

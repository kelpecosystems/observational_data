  ######################################################################
  #' Read and Merge KEEN data
  #' 
  #' 
  ######################################################################
  
  setwd(paste0(here::here(), "/", "R"))
  
  #Load the functions for processing different kinds of KEEN data
  source("./keen_processing_functions.R")
  
  tl_dir <- "../raw_data/"
  files <- paste0(tl_dir, list.files(tl_dir, recursive=TRUE))
  
  #Who are we not working on
  files <- files[!grepl("KEEN PNW", files)]
  files <- files[!grepl("KEEN SOCAL", files)]
  files <- files[!grepl("SUBEKEEN", files)]
  files <- files[!grepl("Cape Elizabeth", files)] #only visited once, 1 transect

  #Ancillary files
  files <- files[!grepl("\\.pdf", files)]
  files <- files[!grepl("\\.zip", files)]
  files <- files[!grepl("\\.kmz", files)]
  files <- files[!grepl("\\.csv", files)]
  files <- files[!grepl("\\~", files)] #system files
  files <- files[!grepl("Scan", files)] #images
  files <- files[!grepl("jpeg", files)] #images
  files <- files[!grepl("Icon", files)] #weird Mac Icon files
  files <- files[!grepl("DO_NOT_LOAD", files)] #if I had to reprocess files
  
  ########
  #make merged data sets
  ########
  
  
  #Data cleaning while merging
  quads <- map_df(files[grepl("[q,Q][u,U][a,A][d,D]", files)],
                  ~process_quad(read_keen_data(.x, debug=T))) %>%
    mutate(COUNT = ifelse(is.na(COUNT), 0, COUNT)) #Deals with quads that are empty, and provides understandable 0s
  
  swath <- map_df(files[grepl("[s,S][W,w][A,a][T,t][H,h]", files)],
                  ~process_swath(read_keen_data(.x, debug=T)))
  
  fish <- map_df(files[grepl("[f,F][I,i][S,s][H,h]", files)],
                 ~process_fish(read_keen_data(.x, debug=T)))
  
  cover <- map_df(files[grepl("[U,u][P,p][C,c]", files)],
                  ~process_pointcount(read_keen_data(.x, debug=T)))
  
  sites <- map_df(files[grep("[S,s][I,i][T,t][E,e]", files)],
                 ~process_siteinfo(read_keen_data(.x, debug=T)))
  
  kelp <- map(files[grep("[K,k][E,e][L,l][P,p]", files)],
                 ~read_keen_data(.x, debug = T)) %>%
    map_df(process_kelp) 
  
  #######
  #merge each data type with the species list
  #######
  spList <- read.csv("../derived_data/merged_sp_list_keenone.csv", stringsAsFactors=FALSE)
  quadsWithSp <- left_join(quads, spList, by=c("SP_CODE" = "SPECIES.CODE"))
  swathWithSp <- left_join(swath, spList, by=c("SP_CODE" = "SPECIES.CODE"))
  fishWithSp <- left_join(fish, spList, by=c("SP_CODE" = "SPECIES.CODE"))
  kelpWithSp <- left_join(kelp, spList, by=c("SP_CODE" = "SPECIES.CODE"))
  
  #remember to deal with substrate in cover
  coverWithSp <- left_join(cover, spList, by=c("SP_CODE" = "SPECIES.CODE")) %>%
    mutate(GROUP.y = ifelse(GROUP.x=="SUBSTRATE", "Substrate", GROUP.y)) %>%
    select(-GROUP.x) %>% rename(GROUP = GROUP.y)
  
  ###########
  # Reorder some columns
  ###########
  
  quadsWithSp <- quadsWithSp %>%
    select(NETWORK, PI, YEAR, MONTH, DAY, SITE, TRANSECT, 
           SP_CODE, QUAD, SIDE, COUNT, AREA, GROUP, DIVISION.FAMILY, 
           COMMON.DIVISION.NAME, SIZE, COMMON.NAME, KINGDOM, PHYLUM, 
           CLASS, ORDER, FAMILY, GENUS, SPECIES, NOTES)
  
  
  swathWithSp <- swathWithSp %>%
    select(NETWORK, PI, YEAR, MONTH, DAY, SITE, TRANSECT, 
           SP_CODE, QUAD, SIDE, COUNT, AREA, GROUP, DIVISION.FAMILY, 
           COMMON.DIVISION.NAME, SIZE, COMMON.NAME, KINGDOM, PHYLUM, 
           CLASS, ORDER, FAMILY, GENUS, SPECIES, NOTES)
  
  
  fishWithSp <- fishWithSp %>%
    select(NETWORK, PI, YEAR, MONTH, DAY, SITE, TRANSECT, 
           SP_CODE, QUAD, SIDE, SIZE.x, COUNT, AREA, GROUP, DIVISION.FAMILY, 
           COMMON.DIVISION.NAME, SIZE.y, COMMON.NAME, KINGDOM, PHYLUM, 
           CLASS, ORDER, FAMILY, GENUS, SPECIES) %>%
    rename(FISH.SIZE = SIZE.x, SIZE = SIZE.y)
  
  coverWithSp <- coverWithSp %>%
    select(NETWORK, PI, YEAR, MONTH, DAY, SITE, TRANSECT, SP_CODE, 
           PERCENT_COVER, GROUP, DIVISION.FAMILY, COMMON.DIVISION.NAME, SIZE, 
           COMMON.NAME, KINGDOM, PHYLUM, CLASS, ORDER, FAMILY, 
           GENUS, SPECIES)
  
  kelpWithSp <- kelpWithSp %>%
    select(NETWORK, PI, YEAR, MONTH, DAY, SITE, TRANSECT, SP_CODE, 
           BLADE_LENGTH_CM, WIDTH_CM, STIPE_LENGTH_CM,
           WET_WEIGHT, DRY_WEIGHT,
           GROUP, DIVISION.FAMILY, COMMON.DIVISION.NAME, SIZE, 
           COMMON.NAME, KINGDOM, PHYLUM, CLASS, ORDER, FAMILY, 
           GENUS, SPECIES)
  
  write.csv(quadsWithSp, "../derived_data/keen_quads.csv", row.names=FALSE)
  write.csv(swathWithSp, "../derived_data/keen_swath.csv", row.names=FALSE)
  write.csv(fishWithSp, "../derived_data/keen_fish.csv", row.names=FALSE)
  write.csv(coverWithSp, "../derived_data/keen_cover.csv", row.names=FALSE)
  write.csv(kelpWithSp, "../derived_data/keen_kelp.csv", row.names=FALSE)
  
  write.csv(sites, "../derived_data/keen_sites.csv", row.names=FALSE)

#################
# Renaming KEEN Sites and Transects
#
# maky error creep in over time, so the QC_KEEN.R script will use this
# to fix both site and transect names
##################################################

name_gsub <- . %>%
  mutate(TRANSECT = gsub("\\&", "and", TRANSECT)) %>%
  mutate(TRANSECT = gsub("DEVILS_DANCE_FLOOR", "Devil's Dance Floor", TRANSECT)) %>%
  mutate(TRANSECT = gsub("BROAD_COVE", "Broad Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("BROADWAY_42ND", "Broadway and 42nd", TRANSECT)) %>%
  mutate(TRANSECT = gsub("NORWEGIAN_COVE", "Norwegian Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("LARUS_LEDGE", "Larus Ledge", TRANSECT)) %>%
  mutate(TRANSECT = gsub("MAGIC_8_BALL", "Magic 8 Ball", TRANSECT)) %>%
  mutate(TRANSECT = gsub("NORTH_HEAD", "North Head", TRANSECT)) %>%
  mutate(TRANSECT = gsub("SANDPIPER", "Sandpiper Beach", TRANSECT)) %>%
  mutate(TRANSECT = gsub("NORTH_PEPPERRELL", "North Pepperrell", TRANSECT)) %>%
  mutate(TRANSECT = gsub("NORTH_SMITHS_COVE", "North Smith's Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("SOUTH_PEPPERRELL", "South Pepperrell", TRANSECT)) %>%
  mutate(TRANSECT = gsub("SOUTH_SMITHS_COVE", "South Smith's Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Broad cove", "Broad Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Broadway$", "Broadway and 42nd", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Sandpiper$", "Sandpiper Beach", TRANSECT)) %>%
  mutate(SITE = gsub("BAKER_SOUTH", "Baker South", SITE)) %>%
  mutate(SITE = gsub("BAKER_NORTH", "Baker North", SITE)) %>%
  mutate(SITE = gsub("LITTLE_BREWSTER", "Little Brewster", SITE)) %>%
  mutate(SITE = gsub("NE_APPLEDORE", "NE Appledore", SITE)) %>%
  mutate(SITE = gsub("NW_APPLEDORE", "NW Appledore", SITE)) %>%
  mutate(SITE = gsub("SW_APPLEDORE", "SW Appledore", SITE)) %>%
  mutate(SITE = gsub("CALF_ISLAND", "Calf Island", SITE)) %>%
  mutate(SITE = gsub("Canoe$", "Canoe Beach", SITE)) %>%
  mutate(SITE = gsub("Pemaquid, ME", "Pemaquid", SITE)) 
name_gsub <- . %>%
  mutate(TRANSECT = gsub("\\&", "and", TRANSECT)) %>%
  mutate(TRANSECT = gsub("DEVILS_DANCE_FLOOR", "Devil's Dance Floor", TRANSECT)) %>%
  mutate(TRANSECT = gsub("BROAD_COVE", "Broad Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("BROADWAY_42ND", "Broadway and 42nd", TRANSECT)) %>%
  mutate(TRANSECT = gsub("NORWEGIAN_COVE", "Norwegian Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("LARUS_LEDGE", "Larus Ledge", TRANSECT)) %>%
  mutate(TRANSECT = gsub("MAGIC_8_BALL", "Magic 8 Ball", TRANSECT)) %>%
  mutate(TRANSECT = gsub("NORTH_HEAD", "North Head", TRANSECT)) %>%
  mutate(TRANSECT = gsub("SANDPIPER", "Sandpiper Beach", TRANSECT)) %>%
  mutate(TRANSECT = gsub("NORTH_PEPPERRELL", "North Pepperrell", TRANSECT)) %>%
  mutate(TRANSECT = gsub("NORTH_SMITHS_COVE", "North Smith's Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("SOUTH_PEPPERRELL", "South Pepperrell", TRANSECT)) %>%
  mutate(TRANSECT = gsub("SOUTH_SMITHS_COVE", "South Smith's Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Broad cove", "Broad Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Broadway$", "Broadway and 42nd", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Sandpiper$", "Sandpiper Beach", TRANSECT)) 
  
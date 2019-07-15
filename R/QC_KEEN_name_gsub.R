#################
# Renaming KEEN Sites and Transects
#
# maky error creep in over time, so the QC_KEEN.R script will use this
# to fix both site and transect names
##################################################

name_gsub <- . %>%
  mutate(TRANSECT = trimws(TRANSECT)) %>%
  mutate(TRANSECT = gsub("\\&", "and", TRANSECT)) %>%
  mutate(TRANSECT = gsub("DEVILS_DANCE_FLOOR", "Devil's Dance Floor", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Devils", "Devil's", TRANSECT)) %>%
  mutate(TRANSECT = gsub("BROAD_COVE", "Broad Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("BROADWAY_42ND", "Broadway and 42nd", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Broadway [0-9][0-9]", "Broadway and 42nd", TRANSECT)) %>%
  mutate(TRANSECT = gsub("NORWEGIAN_COVE", "Norwegian Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("LARUS_LEDGE", "Larus Ledge", TRANSECT)) %>%
  mutate(TRANSECT = gsub("MAGIC_8_BALL", "Magic 8 Ball", TRANSECT)) %>%
  mutate(TRANSECT = gsub("NORTH_HEAD", "North Head", TRANSECT)) %>%
  mutate(TRANSECT = gsub("SANDPIPER", "Sandpiper Beach", TRANSECT)) %>%
  mutate(TRANSECT = gsub("NORTH_PEPPERRELL", "North Pepperrell", TRANSECT)) %>%
  mutate(TRANSECT = gsub("NORTH_SMITHS_COVE", "North Smith's Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("SOUTH_PEPPERRELL", "South Pepperrell", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Dancefloor", "Dance Floor", TRANSECT)) %>%
  mutate(TRANSECT = gsub("South Pepperrell", "South Pepperrell", TRANSECT)) %>%
  mutate(TRANSECT = gsub("S. Smiths Cove", "South Smith's Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("N. Smiths Cove", "North Smith's Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("N. Pepperrell", "North Pepperrell", TRANSECT)) %>%
  mutate(TRANSECT = gsub("S. Pepperrell", "South Pepperrell", TRANSECT)) %>%
  mutate(TRANSECT = gsub("SOUTH_SMITHS_COVE", "South Smith's Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Broad cove", "Broad Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Broadway$", "Broadway and 42nd", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Sandpiper$", "Sandpiper Beach", TRANSECT)) %>%
  mutate(SITE = gsub("BAKER_SOUTH", "Baker South", SITE)) %>%
  mutate(SITE = gsub("BAKER_NORTH", "Baker North", SITE)) %>%
  mutate(SITE = gsub("LITTLE_BREWSTER", "Little Brewster", SITE)) %>%
  mutate(SITE = gsub("NE_APPLEDORE", "NE Appledore", SITE)) %>%
  mutate(SITE = gsub("NE APPLEDORE", "NE Appledore", SITE)) %>%
  mutate(SITE = gsub("NW_APPLEDORE", "NW Appledore", SITE)) %>%
  mutate(SITE = gsub("SW_APPLEDORE", "SW Appledore", SITE)) %>%
  mutate(SITE = gsub("CALF_ISLAND", "Calf Island", SITE)) %>%
  mutate(SITE = gsub("SCHO", "Schoodic", SITE)) %>%
  mutate(SITE = gsub("Nubble$", "Nubble Lighthouse", SITE)) %>%
  mutate(SITE = gsub("Canoe$", "Canoe Beach", SITE)) %>%
  mutate(SITE = gsub("Pemaquid, ME", "Pemaquid", SITE)) %>%
  mutate(SITE = gsub("PEMA", "Pemaquid", SITE)) %>%
  mutate(SITE = gsub("Pema$", "Pemaquid", SITE)) %>%
  mutate(SITE = gsub("Scho$", "Schoodic", SITE)) %>%
  mutate(SITE = gsub("HURR$", "Hurricane Island", SITE)) %>%
  mutate(SITE = gsub("Hurr$", "Hurricane Island", SITE)) %>%
  mutate(SITE = gsub("Pumphouse$", "Pumphouse Beach", SITE)) %>%
  mutate(SITE = gsub("PHB", "Pumphouse Beach", SITE)) %>%
  mutate(SITE = gsub("Fort Wetherill", "Fort Weatherill", SITE)) %>%
  mutate(SITE = gsub("Fort Wetherill", "Fort Weatherill", SITE)) %>%
  mutate(SITE = gsub("Fort Wetherill", "Fort Weatherill", SITE)) %>%
  mutate(SITE = str_remove(SITE, ", Nahant, MA")) %>%
  mutate(TRANSECT = ifelse(SITE=="Canoe Beach", paste0("Canoe Beach ", TRANSECT), TRANSECT)) %>%
  mutate(TRANSECT = ifelse(SITE=="Pumphouse Beach", paste0("Pumphouse Beach ", TRANSECT), TRANSECT)) %>%
  mutate(SITE = gsub("Canoe Beach", "Nahant", SITE)) %>%
  mutate(SITE = gsub("Pumphouse Beach", "Nahant", SITE)) %>%
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
  mutate(TRANSECT = gsub("North Smith$", "North Smith's Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("South Smith$", "South Smith's Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("SOUTH_PEPPERRELL", "South Pepperrell", TRANSECT)) %>%
  mutate(TRANSECT = gsub("SOUTH_SMITHS_COVE", "South Smith's Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Broad cove", "Broad Cove", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Broadway$", "Broadway and 42nd", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Sandpiper$", "Sandpiper Beach", TRANSECT)) %>%
  mutate(TRANSECT = gsub("Sandpiper$", "Sandpiper Beach", TRANSECT)) 

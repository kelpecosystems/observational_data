##########################################################################################
#' Script for making a WORMS validated species list
#' for use with later data processing
##########################################################################################

library(tidyverse)
library(readxl)
library(taxizesoap)

spList <- read_excel("../species_lists/KEEN ONE New England Species List v1.3.4.xlsx", 
                     sheet=1, skip=4)[,c(1:6,8)] %>%
  mutate(`Species Code` = trimws(`Species Code`))

names(spList) <- make.names(names(spList))

#fill down
spList <- spList %>% 
  select(Kingdom, Division.Family , Common.Division.Name, Species.Code, Species, Size, Common.name) %>%
  fill(Kingdom, Division.Family, Common.Division.Name) %>%
  filter(!is.na(Species.Code)) %>%
  #edit
  mutate(Species = trimws(Species)) %>%
  #remove generic
  mutate(Species = gsub("spp[\\.,]$", "", Species)) %>%
  mutate(Species = gsub("sp[\\.,]$", "", Species)) %>%
  mutate(Species = trimws(Species)) 

spListCut <- spList %>%
  group_by(Species) %>%
  slice(1L) %>%
  ungroup()



#for testing
#spList <- spList[1:10,]

######
#Get WORMS classifications
######

#First, get valid names 
#and, hey, taxonomy is a bonus!
validNames <- worms_records(scientific=spListCut$Species) %>%
  filter(status != "deleted") %>%
  filter(!grepl("subsp\\.", valid_name)) %>%
  filter(!grepl("var\\.", valid_name))%>%
  filter(!grepl("var\\.", scientificname))%>%
  filter(!grepl("f\\.", valid_name)) %>%
  filter(!is.na(valid_name)) %>% #weird that I even have to do this
  group_by(inputid) %>%
  arrange(status) %>% #in case one is accepted
  slice(1L) %>%
  ungroup() %>%
  select(inputid, valid_name) %>%
  rename(Species = inputid)

#OK, now do something with those valid names to get
#the correct phylogeny
reallyValidNames <- worms_records(scientific=validNames$valid_name) %>%
  #  tmp_rvn %>%
  group_by(inputid) %>%
  arrange(status) %>% #in case one is accepted
  slice(1L) %>%
  ungroup() %>%
  select(inputid, kingdom, phylum, class, order, family, genus, valid_name) %>%
  rename(species = valid_name) %>%
  right_join(validNames, by=c("species" = "valid_name"))#for joining back to original in case of spelling error

# 
#now join this with the spList
spListFull <- left_join(spList,
                        reallyValidNames, 
                        by=c("Species" = "Species")) %>%
  mutate(species = ifelse(is.na(species), Species, species)) %>%
  select(-Species, -Kingdom) %>%
  mutate(GROUP = ifelse(grepl("(Algae|Plant|Algal)", Common.Division.Name), "Algae", "Invertebrate")) %>%
  mutate(GROUP = ifelse(grepl("Fish", Common.Division.Name), "Fish", GROUP))

names(spListFull) <- toupper(names(spListFull))

write.csv(spListFull, "../derived_data/merged_sp_list_keenone.csv", row.names=F)


# ##### Evaluation
# na_sp <- spListFull %>%
#   filter(is.na(KINGDOM))
# 
# 
# test <- worms_records("Polynices duplicata") %>%
#   filter(status != "deleted") %>%
#   filter(!grepl("subsp\\.", valid_name)) %>%
#   filter(!grepl("var\\.", valid_name))%>%
#   filter(!grepl("var\\.", scientificname)) %>%
#   filter(!grepl("f\\.", valid_name)) %>%
#   filter(!is.na(valid_name)) %>%
#   group_by(inputid) %>%
#   arrange(status) %>% #in case one is accepted
#   slice(1L) %>%
#   ungroup() %>%
#   select(inputid, valid_name) %>%
#   rename(Species = inputid)
# 
# test2 <- worms_records(scientific=test$valid_name) %>%
#   group_by(inputid) %>%
#   arrange(status) %>% #in case one is accepted
#   slice(1L) %>%
#   ungroup() %>%
#   select(inputid, kingdom, phylum, class, order, family, genus, valid_name) %>%
#   rename(species = valid_name) %>%
#   right_join(test, by=c("species" = "valid_name"))#for joining back to original in case of spelling error
# 
# right_join(spList,
#            test2, 
#            by=c("Species" = "Species")) %>%
#   as.data.frame
# 
# bad_join <- anti_join(spList,
#                       reallyValidNames, 
#                       by=c("Species" = "inputid"))

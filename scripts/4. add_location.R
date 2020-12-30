# D. J. van Os 
# 21/10/2020

# Load libraries
library(tidyverse)
library(googlesheets4)
library(ggmap)

register_google(key = "AIzaSyDd2A2sNOFvYqp1TmmnKKqe4hV8MAIbeFk")

# Download rated data from googlesheets 
evo_ID <- gs4_find("hbes_3_extract_user")$id
evo_rated <- read_sheet(evo_ID, sheet = 1)

# Load other datasets
social <- read_rds("data/3 - users extracted/spsp_3_extract_user.rds")
bio <- read_rds("data/3 - users extracted/evoldir_3_extract_user.rds")

# Save only columns present in all 3 datasets 
names <- intersect(names(evo_rated), names(social))
names <- intersect(names, names(bio))

# Prepare datasets for merging
evo <- evo_rated %>% 
  slice(-1) %>% 
  filter(selected != 0) %>% 
  mutate(location = ifelse(location == "NA", NA, location)) %>% 
  select(all_of(names)) %>% 
  mutate(dataset = "evo",
         dataset_code = 0) 

social <- social %>% 
  select(all_of(names)) %>% 
  mutate(dataset = "social",
         dataset_code = 1) 

bio <- bio %>% 
  select(all_of(names)) %>% 
  mutate(dataset = "bio",
         dataset_code = 1)

# Extract location
location <- function(data){
  row = 0
  while(row <= nrow(data)){
    tryCatch({
      print(row)
      row = row + 1
      
      if(!is.na(data[row, "location"])){
        latlon <- as.numeric(geocode(as.character(data[row, "location"]))) 
        location <- revgeocode(location = latlon, output = "all")$results[[1]]
        location <- as_tibble(unlist(location))
        country <- as.character(location %>% slice(which(location$value == "country") - 2))
        
        
        data[row, "lat"] <- latlon[1]
        data[row, "lon"] <- latlon[2]
        data[row, "country"] <- country
      }
      
    },
    # printing errors
    error = function(cond) {
      print(paste('Error caught:', cond));
      return(NA)
    })
  }
  return(data)
}


evo <- location(evo)
bio <- location(bio)
social <- location(social)

saveRDS(evo, file = "data/4 - location extracted/evo_location_extracted.rds")
saveRDS(bio, file = "data/4 - location extracted/bio_location_extracted.rds")
saveRDS(social, file = "data/4 - location extracted/social_location_extracted.rds")



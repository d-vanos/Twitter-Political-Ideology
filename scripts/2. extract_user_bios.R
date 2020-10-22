# D. J. van Os
# 20/10/2019
# Twitter Political Ideology Analysis

################-
#### Method ####
################-

# 1. Find names of social psychologists from the SPSP members list, evolutionary biologists from
#    the Evol Dir members list, and the evolutionary psychologists from the HBES members list.
# 2. Extract twitter handles by matching keywords in a custom-made dictionary with the bios of
#    the first 100 name matches on twitter.
# 3. To determine whether the function was properly selecting academics who had accounts, the first
#    50 names in the HBES list were manually searched on twitter.
# 4. For cases where there was more than one user, these were differentiated by manually comparing
#    the bios.

###############################-
#### Preparing the dataset ####
###############################-

# Clear environment
rm(list = ls())

# Libraries
library(tidyverse)
library(googlesheets4)
library(gender)
library(rtweet)


# To install genderdata (a requirement for gender) you can use this installer:
# install.packages("genderdata", type = "source", repos = "http://packages.ropensci.org")

# token <- read_rds("scripts/googlesheets_token.rds")
# gs4_auth(token = token)

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "dvanos@outlook.com.au"
)

sheets_auth()

twitter_token <- read_rds("twitter_token.rds")

# Import datasets
social <- read_csv("data/2 - clean/social_2_clean.csv")
bio <- read_csv("data/2 - clean/EvolDir_Clean.csv")
evo <- read_csv("data/2 - clean/HBES_Clean.csv")

# social <- social %>% slice(1:11)
# bio <- bio %>% slice(1:11)
# evo <- evo %>% slice(1:11)


##################################-
##### Creating the dictionary ####
##################################-

# Dictionary of common academic words in Twitter Bio
# the space before and after the words is necessary to
# prevent words such as 'community' being matched
# Adding version with no white space, then collapsing so it is one long string for grepl()

dictionary <- read_csv("data/dictionary.csv")
dictionary <- dictionary %>% 
  arrange(dictionary) %>% 
  as_vector() %>% 
  paste(., collapse = "|")


###################-
#### Functions ####
###################-

# To extract the user #
extract_user <- function(data, data_row)  {

  # get the member's twitter details
  name = as.character(data[data_row, "Name"])
  user = search_users(name, token = twitter_token)

  # getting the 'User' variable ready
  if (!missing("user") && nrow(user) != 0) {
    user$description = paste(" ", user$description)
  }
  
  # Check bio against dictionary
  user$dict_match <- grepl(toupper(dictionary), toupper(user$description)) # 'toupper' prevents capitalisation being an issue
  
  return(user)
}


# To select the correct user (the one that matched the dictionary) 
correct_user <- function(data, data_row, user){
  
  # if 0 handles returned 
  if (is.null(user)) {

    data[data_row, "Results"] = 0
  }
  
  else {
    
    # select only users that matched the dictionary
    match = user %>% filter(dict_match == TRUE)
    if(nrow(match) > 0){
      match <- match %>% 
        select(description, name, friends_count, followers_count, location, account_created_at, statuses_count, retweet_count, account_lang, screen_name, dict_match) %>% 
        mutate_all( ~ as.character(.))
      match[match == "" | is.na(match)] <- "NA"
    }
    
    data[data_row, "Twitter_Bio"] = paste(match$description, sep = '', collapse = "//////")
    data[data_row, "Twitter_Name"] = paste(match$name, sep = '', collapse = "//////")
    data[data_row, "n_friends"]  = paste(match$friends_count, sep = '', collapse = "//////")
    data[data_row, "n_followers"] = paste(match$followers_count, sep = '', collapse = "//////")
    data[data_row, "location"] = paste(match$location, sep = '', collapse = "//////")
    data[data_row, "account_creation_date"] = paste(match$account_created_at, sep = '', collapse = "//////")
    data[data_row, "n_twitter_posts"] = paste(match$statuses_count, sep = '', collapse = "//////")
    data[data_row, "retweet_count"] = paste(match$retweet_count, sep = '', collapse = "//////")
    data[data_row, "account_language"]= paste(match$account_lang, sep = '', collapse = "//////")
    

    # if 0 matched handles
    if (nrow(match) == 0) {
      
      data[data_row, "Results"] = 0
      
    }

    # if >= 1 handle - add to main dataset
    else if (nrow(match) >= 1) {
      
      handle = as_vector(match %>% select(screen_name))
      n = nrow(match %>% filter(dict_match == TRUE))
      data[data_row, "Twitter_Handle"] = paste(handle, sep = '' , collapse = "//////")
      data[data_row, "Results"] = n
      
    }
  }
  
  return(data)
}


# Determine gender 
gender_add <- function(data, data_row){
  name <- as.character(data[data_row, "Name"])
  name <- gsub("(\\w+).*", "\\1", name) # select only the first name
  gender_name <- gender(name, years = c(1940, 2000)) # extract gender info from name
  data[data_row, "gender"] <- ifelse(nrow(gender_name) == 0, NA, gender_name["gender"]) # if a gender was found, add this
  data[data_row, "gender_prop"] <- ifelse(nrow(gender_name) == 0, NA, gender_name[paste0("proportion_", gender_name$gender)]) # Proportion male if male, proportion female if female
  
  return(data)
}

# Determine location
twitter_location <- function(data, data_row){
  
  data[data_row, "latlon"] <- as.numeric(geocode(data[data_row, "location"]))
  revgeocode(latlon)
  
}

# Extracting twitter handles and adding them to dataset
add_user <- function(data){
  data_row = 1
  while (data_row < nrow(data)){
    tryCatch({
      # Extract user and match to dictionary 
      user <- extract_user(data, data_row)
      data <- correct_user(data, data_row, user)
      data <- gender_add(data, data_row)

      # Pause the loop if we reached the API limit
      # Twitter limits the number of users you can extract to 900 every 15 minutes, so this needs to be accounted for
      # Every 10 rows: 
      if(data_row %% 10 == 0) { 
        rate <- rate_limit(token= twitter_token) %>% filter(query == "users/search") # get info on how many users you can search 
        print(paste0("Requests remaining: ", rate$remaining))
        print(paste0("Number of requests to make: ", nrow(data)-data_row))
        print(paste0("Minutes until reset: ", as.numeric(round(rate$reset, 2)), " mins"))
        print(paste0("User limits will reset at: ", rate$reset_at))
        if(rate$remaining == 0) { 
          data_row = data_row - 11 # Go back 10 (-1 because one added below) to make sure we haven't missed anyone 
          Sys.sleep(time = as.numeric(round(rate$reset, 1) * 60)) # Stop the loop until the rate resets
          } 
      }
      data_row = data_row + 1
  },

    # printing errors
    error = function(cond) {
    print(paste('Error caught:', cond));
    return(NA)        
  })
  print(data_row)
  }
  return(data)
}



# Run functions 
evo_extracted <- add_user(evo)
social_extracted <- add_user(social)
bio_extracted <- add_user(bio)

# Remove people who don't have twitter and spread out multiple hits into separate rows
filter_and_spread <- function(data){
  data %>%
    filter(!is.na(Twitter_Bio) & Twitter_Bio != "") %>% 
    mutate(ID = seq.int(nrow(.)), 
           selected = -99) %>% 
    select(ID, everything()) %>% 
    separate_rows(-c("ID", "Name", "Results", "gender", "gender_prop"), sep = "//////")
  
}

evo_extracted <- filter_and_spread(evo_extracted)
social_extracted <- filter_and_spread(social_extracted)
bio_extracted <- filter_and_spread(bio_extracted)

# Save 
# CSV
write_csv(x = evo_extracted, path = "data/3 - users extracted/hbes_3_extract_user.csv", na = "")
write_csv(x = social_extracted, path = "data/3 - users extracted/spsp_3_extract_user.csv", na = "")
write_csv(x = bio_extracted, path = "data/3 - users extracted/evoldir_3_extract_user.csv", na = "")

# RDS
saveRDS(evo_extracted, file = "data/3 - users extracted/hbes_3_extract_user.rds")
saveRDS(social_extracted, file = "data/3 - users extracted/spsp_3_extract_user.rds")
saveRDS(bio_extracted, file = "data/3 - users extracted/evoldir_3_extract_user.rds")

# Googlesheets
# Remove previous version
gs4_find("hbes_3_extract_user") %>% 
  googledrive::drive_trash()
gs4_find("spsp_3_extract_user") %>% 
  googledrive::drive_trash()
gs4_find("evoldir_3_extract_user") %>% 
  googledrive::drive_trash()

# Add new versions 
gs4_create(name = "hbes_3_extract_user", sheets = evo_extracted)
gs4_create(name = "spsp_3_extract_user", sheets = social_extracted)
gs4_create(name = "evoldir_3_extract_user", sheets = bio_extracted)





















#latlon <- as.numeric(geocode(as.character(data[data_row, "location"]))) # do this later as some rows have multiple people on them 
# data[data_row, "lat"] <- latlon[1]
# data[data_row, "lon"] <- latlon[2]


















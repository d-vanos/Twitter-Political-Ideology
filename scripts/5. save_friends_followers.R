# D. J. van Os 
# 19/10/2020
# Extract Friends and Followers

# Load libraries
library(tidyverse)
library(googlesheets4)
library(rtweet)
library(ggmap) # for location

register_google(key = "AIzaSyDd2A2sNOFvYqp1TmmnKKqe4hV8MAIbeFk")

# Remove current environment
rm(list = ls())

# Download rated data from googlesheets 
evo_ID <- gs4_find("hbes_3_extract_user")$id
evo_rated <- read_sheet(evo_ID, sheet = 1)

social_ID <- gs4_find("spsp_3_extract_user")$id
social_rated <- read_sheet(social_ID, sheet = 1)

bio_ID <- gs4_find("evoldir_3_extract_user")$id
bio_rated <- read_sheet(bio_ID, sheet = 1)

# Filter out those who were not correct
evo_rated <- evo_rated %>% 
  slice(-1) %>% 
  filter(selected != 0) %>% 
  mutate(location = ifelse(location == "NA", NA, location))

social_rated <- social_rated %>% 
  slice(-1) %>% 
  filter(selected != 0) %>% 
  mutate(location = ifelse(location == "NA", NA, location))

bio_rated <- bio_rated %>% 
  slice(-1) %>% 
  filter(selected != 0) %>% 
  mutate(location = ifelse(location == "NA", NA, location))

# System sleep if rate exceeded
wait <- function(row, filter_by, n_check, go_back){
  if(row %% n_check == 0) { 
    rate <- rate_limit() %>% filter(query == filter_by) # get info on how many users you can search 
    print(paste0("Requests remaining: ", rate$remaining))
    print(paste0("Number of requests to make: ", nrow(data)-row))
    print(paste0("Minutes until reset: ", as.numeric(round(rate$reset, 2)), " mins"))
    print(paste0("User limits will reset at: ", rate$reset_at))
    if(rate$remaining == 0) { 
      row = row - go_back # Go back 10 (-1 because one added below) to make sure we haven't missed anyone 
      Sys.sleep(time = as.numeric(round(rate$reset, 1) * 60)) # Stop the loop until the rate resets
    } 
  }
}



# Extract twitter IDs (necessary to request friends and followers)
get_user_ID <- function(data){
  tryCatch({
    row = 1
    while (row <= nrow(data)){
      print(row)
      
      # Extract IDs
      handle <- as.character(data[row, 'Twitter_Handle'])
      user_data <- lookup_users(handle)
      data[row, 'User_ID'] <- as.numeric(user_data[1, 'user_id'])
      
      # Wait if twitter API limits have been exceeded 
      wait(row, filter_by = "users/lookup", n_check = 10, go_back = 11)
      row = row + 1
    }
    # printing errors
    error = function(cond) {
      print(paste('Error caught:', cond));
      return(NA)
      }
    })  
  return(data)
}

extract_friends_followers <- function(data){
  
  tryCatch({
    
    new_data <- tibble(to = NULL, from = NULL)
    row = 1 
    print(row)
    
    while (row <= nrow(data)){
      
      # Extract followers 
      followers <- get_followers(as.numeric(data[row, "User_ID"]))
      
      if(nrow(followers) > 0){
        followers <- followers %>% 
          mutate(to = as.numeric(data[row, "User_ID"])) %>% 
          rename(from = user_id)
        
        new_data <- rbind(new_data, followers)
      }
      
      # Extract friends
      friends <- get_friends(as.numeric(data[row, "User_ID"]))
      
      if(nrow(friends) > 0){
      friends <- friends %>%
        rename(from = user,
               to = user_id)
      
      new_data <- rbind(new_data, friends)
      }
      
      # Wait if twitter API limits have been exceeded 
      wait(row, filter_by = "followers/ids", n_check = 15, go_back = 0)
      row = row + 1
      
    }
    # printing errors
    error = function(cond) {
      print(paste('Error caught:', cond));
      return(NA)
      
    }
  }) 
  
  return(new_data)
}




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

# Libraries
library(tidyverse)
library(rtweet)
library(googlesheets4)

token <- read_rds("scripts/googlesheets_token.rds")
gs4_auth(token = token)

# Import datasets
social <- read_csv("data/2 - clean/SPSP_Clean.csv")
bio <- read_csv("data/2 - clean/EvolDir_Clean.csv")
evo <- read_csv("data/2 - clean/HBES_Clean.csv")


##################################-
##### Creating the dictionary ####
##################################-

# Dictionary of common academic words in Twitter Bio
# the space before and after the words is necessary to
# prevent words such as 'community' being matched

dictionary <- c(" uni ",
                " university ",
                " academia",
                " Dr ",
                " Dr. ",
                " PhD ",
                " PhD. ",
                " Professor ",
                " Researcher ",
                " Research ",
                " Academic ",
                " Science ",
                " Lab ",
                " Laboratory ",
                " Scientist ",
                " Research ",
                " Psychologist ",
                " Biologist ",
                " Lecturer ",
                " Prof ",
                " Prof. ",
                " Doctor ",
                " Postdoc ",
                " Researching ",
                " Psychology ",
                " Biology ",
                " Social Psychology ",
                " Evolutionary Biology ",
                " Evolutionary Psychology ",
                " graduate student ",
                " grad student ",
                " Behavioral ",
                " Behavioural ",
                " Biological ")

# Adding version with no white space, then collapsing so it is one long string for grepl()
#dictionary <- c(dictionary, trimws(dictionary))
dictionary <- paste(dictionary, collapse = "|")



# dictionary <-           c("\\<uni\\>",
#                           "\\<university\\>",
#                           "\\<academia\\>",
#                           "\\<Dr\\>",
#                           "\\<Dr.\\>",
#                           "\\<PhD\\>",
#                           "\\<Professor\\>",
#                           "\\<Researcher\\>",
#                           "\\<Research\\>",
#                           "\\<Academic\\>",
#                           "\\<Science\\>",
#                           "\\<Lab\\>",
#                           "\\<Laboratory\\>",
#                           "\\<Scientist\\>",
#                           "\\<Research\\>",
#                           "\\<Psychologist\\>",
#                           "\\<Biologist\\>",
#                           "\\<Lecturer\\>",
#                           "\\<Prof\\>",
#                           "\\<Prof.\\>",
#                           "\\<Doctor\\>",
#                           "\\<Postdoc\\>",
#                           "\\<Researching\\>",
#                           "\\<Psychology\\>",
#                           "\\<Biology\\>",
#                           "\\<Social Psychology\\>",
#                           "\\<Evolutionary Biology\\>",
#                           "\\<Evolutionary Psychology\\>",
#                           "\\<graduate student\\>",
#                           "\\<grad student\\>",
#                           "\\<Behavioral\\>",
#                           "\\<Behavioural\\>",
#                           "\\<Biological\\>")




###################-
#### Functions ####
###################-

# To extract the user #
extract_user <- function(data, data_row)  {

  # get the member's twitter details
  name = as.character(data[data_row, "Name"])
  user = search_users(name)

  # getting the 'User' variable ready
  if (!missing("user") && nrow(user) != 0) {
    user <- user %>%
      select(screen_name, name, location, description, followers_count, friends_count,
             listed_count, statuses_count, favourites_count, account_created_at) # remove this line later
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
    data[data_row, "Twitter_Bio"] = paste(match$description, sep = '', collapse = "///")
    data[data_row, "Twitter_Name"] = paste(match$name, sep = '', collapse = "///")

    # if 0 matched handles
    if (nrow(match) == 0) {
      
      data[data_row, "Results"] = 0
      
    }

    # if >= 1 handle - add to main dataset
    else if (nrow(match) >= 1) {
      
      handle = as_vector(match %>% select(screen_name))
      n = nrow(match %>% filter(dict_match == TRUE))
      data[data_row, "Twitter_Handle"] = paste(handle, sep = '' , collapse = ' ')
      data[data_row, "Results"] = n
      
    }
  }
  
  return(data)
}



# Extracting twitter handles and adding them to dataset
add_user <- function(data){
  data_row = 1
  while (data_row < nrow(data)){
    tryCatch({
      # Extract user and match to dictionary 
      user <- extract_user(data, data_row)
      data <- correct_user(data, data_row, user)
      
      # Pause the loop if we reached the API limit
      # Twitter limits the number of users you can extract to 900 every 15 minutes, so this needs to be accounted for
      # Every 10 rows: 
      if(data_row %% 10 == 0) { 
        rate <- rate_limit() %>% filter(query == "users/search") # get info on how many users you can search 
        print(paste0("Remaining: ", rate$remaining))
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

# Save 
# CSV
write_csv(x = evo_extracted, path = "data/hbes_2_extract_user.csv")
write_csv(x = social_extracted, path = "data/spsp_2_extract_user.csv")
write_csv(x = bio_extracted, path = "data/evoldir_2_extract_user.csv")

# RDS
saveRDS(evo_extracted, file = "data/hbes_2_extract_user.rds")
saveRDS(social_extracted, file = "data/spsp_2_extract_user.rds")
saveRDS(bio_extracted, file = "data/evoldir_2_extract_user.rds")

# Googlesheets
# Remove previous version
gs4_find("hbes_2_extract_user") %>% 
  googledrive::drive_trash()
gs4_find("spsp_2_extract_user") %>% 
  googledrive::drive_trash()
gs4_find("evoldir_2_extract_user") %>% 
  googledrive::drive_trash()

# Add new versions 
gs4_create(name = "hbes_2_extract_user", sheets = evo_extracted)
gs4_create(name = "spsp_2_extract_user", sheets = social_extracted)
gs4_create(name = "evoldir_2_extract_user", sheets = bio_extracted)




















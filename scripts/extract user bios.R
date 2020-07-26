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

# Import datasets
social <- read_csv("data/SPSP_Clean.csv")
bio <- read_csv("data/EvolDir_Clean.csv")
evo <- read_csv("data/HBES_Clean.csv")

# add columns
# evo <- evo %>%
#   mutate(Twitter_Handle = "",
#          Twitter_Bio = "",
#          Twitter_Name = "",
#          Results = NA)

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
                          " PhD ",
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
  user$dict_match <- grepl(dictionary, user$description)
  
  return(user)
}




# To select the correct user (the one that matched the dictionary) #
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
  for (data_row in 1:nrow(data)){
  tryCatch({
    user <- extract_user(data, data_row)
    data <- correct_user(data, data_row, user)
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
evo <- add_user(evo)
social <- add_user(social)
bio <- add_user(bio)


















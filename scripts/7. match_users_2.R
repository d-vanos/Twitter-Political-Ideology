

# Remove previous environment
rm(list = ls())

# Load relevant libraries
library(tidyverse)
library(MatchIt) # for pair matching
library(mice) # for missing data
library(googlesheets4) # for uploading to google sheets

# Read in data
evo_full <- read_rds("data/4 - location extracted/evo_location_extracted.rds")
social_full <- read_rds("data/4 - location extracted/social_location_extracted.rds")
evo_social_rated <- read_csv("data/6 - bio and social rated/evo_social_filtered_rated.csv")

##############################- 
#### Remove already rated ####
##############################-

unrated <- evo_social_rated %>% 
  group_by(subclass) %>% 
  summarise(rated = sum(selected)) %>% 
  filter(rated != 1) %>%  
  ungroup()

unrated_row_n <- as_vector(unrated %>% select(subclass))

# Keep only those who don't have a match yet
evo_full <- evo_full %>% 
  mutate(row_n = seq.int(nrow(.))) %>% 
  filter(row_n %in% unrated_row_n) %>% 
  select(-row_n)

# Remove matched users from the social psych dataset as well
selected <- evo_social_rated %>% 
  filter(selected == 1) %>% 
  select(Twitter_Handle)

selected <- as_vector(selected)

social_full <- social_full %>% 
  filter(!Twitter_Handle %in% selected) 


#######################-
#### Preprocessing ####
#######################-

# Filter out >1 Result
social_full <- social_full %>% 
  mutate(Results = as.numeric(Results)) %>% 
  filter(Results == 1)

# create one big dataset
evo_social <- rbind(evo_full, social_full)

evo_social <- evo_social %>% 
  mutate(country = case_when(
    country == "c(\"Antarctica\", \"-180\")" ~ "Antarctica",
    country == "c(\"Antarctica\", \"-180\")" ~ "Antarctica",
    country == "c(\"Spain\", \"-18.2648001\")" ~ "Spain",
    country == "c(\"Philippines\", \"116.1474999\")" ~ "Philippines",
    TRUE ~ as.character(country)))

evo_social <- evo_social %>%
  mutate(dataset_code = ifelse(dataset == "social", yes = 0 , no = 1),
         country = ifelse(country == "character(0)", yes = NA, no = country)) %>%
  mutate_at(c("n_friends", "n_followers", "n_twitter_posts", "retweet_count"), as.numeric) %>%
  mutate_at(c("gender", "dataset"), as.factor) %>% 
  select(-c(ID, account_creation_date, account_language, Results, selected)) 


######################-
#### Missing Data ####
######################-

# See pattern of missing data
md.pattern(evo_social, rotate.names = TRUE)

# Mode imputation for categorical data 
# Function to calculate mode
compute_mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

impute_missing <- function(data){
  
  # See pattern of missing data
  md.pattern(data, rotate.names = TRUE)
  
  # Mode imputation for categorical data 
  # Function to calculate mode
  compute_mode <- function(x) {
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    ux[tab == max(tab)]
  }
  
  # Save mode
  mode <- as.character(compute_mode(data$country))
  
  # Impute missing country data with mode
  data <- data %>% 
    mutate(country = as.factor(ifelse(is.na(country), yes = mode, no = country)))
  
  # Impute remaining missing continuous data 
  impute_missing <-  mice(data, maxit = 40)
  
  # Save new dataset with imputed values 
  data_complete <- complete(impute_missing)
  
  # Recode gender (after gender imputation)
  data_complete <- data_complete %>%
    mutate(gender_code = ifelse(gender == "female", yes = 1, no = 2))
  
  # Check pattern of missing data after imputation
  md.pattern(data_complete, rotate.names = TRUE)
  
  return(data_complete)
}

evo_social_complete <- impute_missing(data = evo_social)


######################-
#### Match Groups ####
######################-

# Check numbers in each group
table(evo_social$dataset_code)

# Match pairs 
matched <- matchit(dataset_code ~ n_friends + n_followers + n_twitter_posts + retweet_count + country + gender_code, 
                   data = evo_social_complete, 
                   method = "optimal", 
                   distance = "glm",
                   ratio = 8) # 5 social matches per evo person 

# Check accuracy of matching 
print(matched)
evo_social_match_summary_filtered <- summary(matched)
evo_social_match_summary_filtered
save(evo_social_match_summary_filtered, file = "data/5 - matched/evo_social_match_summary_filtered_round2.rda")

# Visually display matching 
plot(summary(matched))

plot(matched, type = "jitter", interactive = FALSE)

# obtain matched dataset
evo_social_matched <- match.data(matched)

# Arrange by subclass (the matched groups) and distance (how closely they resemble)
evo_social_matched <- evo_social_matched %>% 
  arrange(subclass, distance)

# Add rating column
evo_social_matched <- evo_social_matched %>% 
  mutate(selected = -99) %>% 
  filter(dataset == "social")

# Save 
saveRDS(evo_social_matched, file = "data/5 - matched/evo_social_matched_filtered_round2.rds")

# Googlesheets
# Remove previous version
gs4_find("evo_social_6_match_user_filtered_round2") %>%
  googledrive::drive_trash()

# Add new versions 
gs4_create(name = "evo_social_6_match_user_filtered_round2", sheets = evo_social_matched)







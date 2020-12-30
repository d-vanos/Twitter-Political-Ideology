
# Match evolutionary psychologists with social psychologists and evolutionary biologists
# D. J. van Os 

# Remove previous environment
rm(list = ls())

# Load relevant libraries
library(tidyverse)
library(MatchIt) # for pair matching
library(mice) # for missing data

######################-
#### Prepare Data ####
######################-
# Read in the data 
evo <- read_rds("data/4 - location extracted/evo_location_extracted.rds")
social <- read_rds("data/4 - location extracted/social_location_extracted.rds")
bio <- read_rds("data/4 - location extracted/bio_location_extracted.rds")

# create one big dataset
# EVO_SOCIAL
evo_social <- rbind(evo, social)

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

# EVO_BIO
evo_bio <- rbind(evo, bio)

evo_bio <- evo_bio %>% 
  mutate(country = case_when(
    country == "c(\"Philippines\", \"116.1474999\")" ~ "Philippines",
    country == "c(\"Maldives\", \"71.751709\")" ~ "Maldives",
    country == "c(\"Saint Vincent and the Grenadines\", \"-61.4822\")" ~ "Saint Vincent and the Grenadines",
    country == "c(\"Cuba\", \"-85.1715001\")" ~ "Cuba",
    TRUE ~ as.character(country)))

evo_bio <- evo_bio %>%
  mutate(dataset_code = ifelse(dataset == "bio", yes = 0 , no = 1),
         country = ifelse(country == "character(0)", yes = NA, no = country)) %>%
  mutate_at(c("n_friends", "n_followers", "n_twitter_posts", "retweet_count"), as.numeric) %>%
  mutate_at(c("gender", "dataset"), as.factor) %>% 
  select(-c(ID, account_creation_date, account_language, Results, selected)) 


######################-
#### Missing Data ####
######################-

# See pattern of missing data
md.pattern(evo_social, rotate.names = TRUE)
md.pattern(evo_bio, rotate.names = TRUE)
  
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
evo_bio_complete <- impute_missing(data = evo_bio)

######################-
#### Match Groups ####
######################-

#### EVO_SOCIAL ####
# Check numbers in each group
table(evo_social$dataset_code)

# Match pairs 
matched <- matchit(dataset_code ~ n_friends + n_followers + + n_twitter_posts + retweet_count + country + gender_code, 
                   data = evo_social_complete, 
                   method = "optimal", 
                   distance = "glm",
                   ratio = 5) # 5 social matches per evo person 

# Check accuracy of matching 
print(matched)
summary(matched)

# Visually display matching 
plot(summary(matched))

plot(matched, type = "jitter", interactive = FALSE)

# obtain matched dataset
evo_social_matched <- match.data(matched)

# Arrange by subclass (the matched groups) and distance (how closely they resemble)
evo_social_matched <- evo_social_matched %>% 
  arrange(subclass, distance)

#### EVO_BIO ####
# Check numbers in each group
table(evo_bio$dataset_code)

# Match pairs 
matched <- matchit(dataset_code ~ n_friends + n_followers + + n_twitter_posts + retweet_count + country + gender_code, 
                   data = evo_bio_complete, 
                   method = "optimal", 
                   distance = "glm",
                   ratio = 5) # 5 social matches per evo person 

# Check accuracy of matching 
print(matched)
summary(matched)

# Visually display matching 
plot(summary(matched))

plot(matched, type = "jitter", interactive = FALSE)

# obtain matched dataset
evo_bio_matched <- match.data(matched)

# Arrange by subclass (the matched groups) and distance (how closely they resemble)
evo_bio_matched <- evo_bio_matched %>% 
  arrange(subclass, distance)
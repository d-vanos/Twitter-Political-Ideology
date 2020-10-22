# D. J. van Os 
# 21/10/2020

# Clear environment
rm(list = ls())

# Load libraries
library(MatchIt)
library(tidyverse)

# Read in the data 
evo <- read_rds("data/4 - location extracted/evo_location_extracted.rds")
social <- read_rds("data/4 - location extracted/social_location_extracted.rds")
bio <- read_rds("data/4 - location extracted/bio_location_extracted.rds")

# create one big dataset 
evo_social <- rbind(evo, social)

# TEMPORARY: remove people with no gender!!!!
evo_social <- evo_social %>% 
  mutate(gender_code = ifelse(gender == "female", 0, 1),
         country = as_factor(ifelse(location == "NA", NA, country))) %>%
  filter(!is.na(gender), !is.na(n_friends), !is.na(n_followers), !is.na(n_twitter_posts), !is.na(country)) %>% 
  mutate(row = 1:nrow(.)) %>% 
  mutate_at(c("n_friends", "n_followers", "n_twitter_posts"), as.numeric) %>% 
  select(row, Twitter_Handle, dataset_code, gender, n_friends, n_followers, n_twitter_posts, country)

# match data
match_evo_social <- matchit(as.factor(dataset_code) ~ as.factor(gender)+ n_friends + n_followers + n_twitter_posts, data = evo_social, method = "nearest", m.order = "random") # country
# Look at matching
summary(match_evo_social)
mm_evo_social <- match_evo_social$match.matrix
row <- rownames(mm_evo_social)
mm_evo_social <- as_tibble(mm_evo_social) 
mm_evo_social <- mm_evo_social %>%
  mutate(social_row = as.numeric(row),
         evo_row = as.numeric(`1`)) %>% 
  select(-`1`) %>% 
  filter(!is.na(evo_row))

evo_social_rows <- as.numeric(c(mm_evo_social$evo_row, mm_evo_social$social_row))

evo_social_match <- evo_social %>% 
  filter(row %in% evo_social_rows) # not coming up with the right number of rows  

# Figure out the Twitter Name of the partner rows 
handle <- evo_social_match %>% 
  select(row, Twitter_Handle)

# Add in partner row 
mm_evo_social <- left_join(mm_evo_social, handle, by = c("evo_row" = "row")) # Not working 
mm_evo_social <- mm_evo_social %>% rename(evo_match = Twitter_Handle)
mm_evo_social <- left_join(mm_evo_social, handle, by = c("social_row" = "row")) # Not working 
mm_evo_social <- mm_evo_social %>% rename(social_match = Twitter_Handle) %>% select(-c(social_row, evo_row))

evo_social_match <- evo_social_match %>% 
  left_join(mm_evo_social, by = c("Twitter_Handle" = "evo_match")) %>% 
  left_join(mm_evo_social, by = c("Twitter_Handle" = "social_match")) %>% 
  unite(match, evo_match, social_match, na.rm = TRUE)



### REPEAT FOR BIO
# create one big dataset 
evo_bio <- rbind(evo, bio)

# TEMPORARY: remove people with no gender!!!!
evo_bio <- evo_bio %>% 
  mutate(gender_code = ifelse(gender == "female", 0, 1),
         country = as_factor(ifelse(location == "NA", NA, country))) %>%
  filter(!is.na(gender), !is.na(n_friends), !is.na(n_followers), !is.na(n_twitter_posts), !is.na(country)) %>% 
  mutate(row = 1:nrow(.)) %>% 
  mutate_at(c("n_friends", "n_followers", "n_twitter_posts"), as.numeric) %>% 
  select(row, Twitter_Handle, dataset_code, gender, n_friends, n_followers, n_twitter_posts, country)

# match data
match_evo_bio <- matchit(as.factor(dataset_code) ~ as.factor(gender)+ n_friends + n_followers + n_twitter_posts, data = evo_bio, method = "nearest", m.order = "random") # country
# Look at matching
summary(match_evo_bio)
mm_evo_bio <- match_evo_bio$match.matrix
row <- rownames(mm_evo_bio)
mm_evo_bio <- as_tibble(mm_evo_bio) 
mm_evo_bio <- mm_evo_bio %>%
  mutate(bio_row = as.numeric(row),
         evo_row = as.numeric(`1`)) %>% 
  select(-`1`) %>% 
  filter(!is.na(evo_row))

evo_bio_rows <- as.numeric(c(mm_evo_bio$evo_row, mm_evo_bio$bio_row))

evo_bio_match <- evo_bio %>% 
  filter(row %in% evo_bio_rows) # not coming up with the right number of rows  

# Figure out the Twitter Name of the partner rows 
handle <- evo_bio_match %>% 
  select(row, Twitter_Handle)

# Add in partner row 
mm_evo_bio <- left_join(mm_evo_bio, handle, by = c("evo_row" = "row")) # Not working 
mm_evo_bio <- mm_evo_bio %>% rename(evo_match = Twitter_Handle)
mm_evo_bio <- left_join(mm_evo_bio, handle, by = c("bio_row" = "row")) # Not working 
mm_evo_bio <- mm_evo_bio %>% rename(bio_match = Twitter_Handle) %>% select(-c(bio_row, evo_row))

evo_bio_match <- evo_bio_match %>% 
  left_join(mm_evo_bio, by = c("Twitter_Handle" = "evo_match")) %>% 
  left_join(mm_evo_bio, by = c("Twitter_Handle" = "bio_match")) %>% 
  unite(match, evo_match, bio_match, na.rm = TRUE)



#### MERGE ALL
evo_social_match <- evo_social_match %>% 
  mutate(dataset = ifelse(dataset_code == 1, "social", "evo"),
         match = ifelse(dataset_code == 0, NA, match)) %>% 
  select(-dataset_code)


evo_bio_match <- evo_bio_match %>% 
  filter(dataset_code == 1) %>% 
  mutate(dataset = "bio") %>% 
  select(-dataset_code)

data <- rbind(evo_social_match, evo_bio_match)

data <- data %>% 
  rename(evo_match = match) %>% 
  select(-row)

write_csv(data, "data/5 - matched/data_matched.csv", na = "")















# Data Cleaning
# D. J. van Os
# 01/11/2019

# reformatting the names so they can be analysed

# Load library
library(tidyverse)

######################-
#### HBES Members ####
######################-

# read in the file
HBES <- read_csv(file = "Data/HBES.csv")

# add ID column
HBES <- HBES %>%
  mutate("ID" = NA,
         "Type" = NA) %>%
  select(ID, Type, Data)

# add ID numbers
ID = 0

for (row in 1:nrow(HBES)){
  Text = HBES[row,"Data"]
  if (str_detect(Text, "^Country") == TRUE){
    ID = ID + 1
    HBES[row, "ID"] = ID
  }
}

for(row in 1:nrow(HBES)){
  if (is.na(HBES[row, "ID"])){
    HBES[row, "ID"] = HBES[row-1, "ID"]
  }
}

# Labels
for (row in 1:nrow(HBES)){
  Text = HBES[row,"Data"]
  if (str_detect(Text, "^Country") == TRUE){
    HBES[row, "Type"] = "Country"}
  else if (str_detect(Text, "^Discipline") == TRUE){
    HBES[row, "Type"] = "Discipline"}
  else if (Text == "View Profile"){
    HBES[row, "Type"] = "Profile"}
  else if(is.na(HBES[row, "Type"])){
    HBES[row, "Type"] = "Name"}
}


# spread
HBES <- pivot_wider(HBES, names_from = "Type", member_lists_from = "Data")

HBES <- HBES %>%
  select(ID, Name, Country, Discipline)

# write csv
write_csv(HBES, "Data/HBES_Clean.csv", na = "")


#########################-
#### EvolDir Members ####
#########################-

# read in csv
Bio <- read_csv("Data/EvolDir.csv")

# separate into name and email
Bio <- Bio %>%
  separate(col = "Names", into = c("Name", "Email"), sep = "- ")

# deal with cases separated by '*'
for (row in 1:nrow(Bio)){
  if(is.na(Bio[row, "Email"])){
    input = Bio[row, "Name"]
    output = str_split(input, "\\*")
    name = output[[1]][1]
    email = output[[1]][2]
    Bio[row, "Name"] = name
    Bio[row, "Email"] = email
  }
}

# check if there are still NAs
Bio %>%
  filter(is.na(Email))

# swap first and last name
Bio <- separate(Bio, col = "Name", into = c("Last", "First"), sep = ",")
Bio$First <- word(Bio$First, start = 1, end = 2)
Bio <- unite(Bio, col = Name, First, Last, sep = " ", remove = TRUE)

# write cleaned file
write_csv(Bio, "Data/EvolDir_Clean.csv", na = "")



######################-
#### SPSP Members ####
######################-

social <- read_csv("data/1 - original/SPSP.csv", skip_empty_rows = FALSE)

social <- social %>%
  mutate(category = "", ID = as.numeric(NA)) %>%
  select(ID, category, member_list)

# add IDs and category labels 
row = 1
while(row <= nrow(social)){
  social[row, "category"] = "Name"
  social[row, "ID"] = row
  row = row + 1
  social[row, "category"] = "Country"
  social[row, "ID"] = row - 1
  row = row + 3
}

# Remove empty rows
social <- social %>% 
  filter(!is.na(ID), !is.na(member_list))

# Transpose
social <- pivot_wider(social, names_from = "category", values_from = "member_list")

# Remove duplicates
social <- social %>% 
  select(-ID) %>% 
  distinct()

# write csv
write_csv(social, "data/2 - clean/social_2_clean.csv", na = "")
saveRDS(social, "data/2 - clean/social_2_clean.rds")







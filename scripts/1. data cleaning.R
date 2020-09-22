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
HBES <- pivot_wider(HBES, names_from = "Type", values_from = "Data")

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

SPSP <- read_csv("Data/SPSP_200_Members.csv", skip_empty_rows = FALSE)

SPSP <- SPSP %>%
  mutate(Category = NA, ID = NA) %>%
  select(ID, Category, Value)

for(row in 1:nrow(SPSP)){
  # for those who have an email address
  if (!is.na(SPSP[row, "Value"]) &&
      !is.na(SPSP[row + 1, "Value"]) &&
      !is.na(SPSP[row + 2, "Value"]) &&
      !is.na(SPSP[row + 3, "Value"])){
    SPSP[row, "Category"] = "Name"
    SPSP[row + 1, "Category"] = "Email"
    SPSP[row + 2, "Category"] = "Place"
    SPSP[row + 3, "Category"] = "Country"
  }
  # for those without email
  else if(is.na(SPSP[row - 1, "Value"]) &&
      !is.na(SPSP[row, "Value"]) &&
      !is.na(SPSP[row + 1, "Value"]) &&
      !is.na(SPSP[row + 2, "Value"]) &&
      is.na(SPSP[row + 3, "Value"])){
    SPSP[row, "Category"] = "Name"
    SPSP[row + 1, "Category"] = "Place"
    SPSP[row + 2, "Category"] = "Country"
  }
}

# check for NAs
SPSP %>%
  filter(is.na(Category) & (!is.na(Value)))

# Add IDs
ID = 0

for (row in 1:nrow(SPSP)){
  if(is.na(SPSP[row - 1, "Value"]) &&
     !is.na(SPSP[row, "Value"]) &&
     !is.na(SPSP[row + 1, "Value"]) &&
     !is.na(SPSP[row + 2, "Value"])){
    ID = ID + 1
    SPSP[row, "ID"] = ID
    SPSP[row + 1, "ID"] = ID
    SPSP[row + 2, "ID"] = ID
    SPSP[row + 3, "ID"] = ID
  }
}


# remove rows with all NAs
SPSP <- SPSP %>%
  filter(!is.na(Category) & (!is.na(Value)))

# Transpose
SPSP <- pivot_wider(SPSP, names_from = "Category", values_from = "Value")

# write csv
write_csv(SPSP, "Data/SPSP_Clean.csv", na = "")


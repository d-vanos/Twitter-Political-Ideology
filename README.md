Twitter Political Ideology Project
================

## Background

The goal of this project is to determine the political ideology of
psychology and biology academics on Twitter, and explore whether there
are differences in the social networks of academic on the basis of
political ideology.

## Data Collection

### Social Psychologists

Names of social psychologists were collected from the [Member
Directory](http://connect.spsp.org/network/members) of the [Society for
Personality and Social Psychology (SPSP)](https://www.spsp.org/) using
the search bar. Every letter of the alphabet was entered into the search
bar for first name and last name (e.g., First Name: A, Last Name: A,
First Name: B, Last Name: B, etc). The first 200 names displayed (the
maximum number displayed) were copied.

Data collection date: 13 October 2020  
Twitter extraction date: 17 October 2020

Sample size:

  - Number extracted from SPSP: **7448**
  - After removing people who did not specify country: **7406 (-42)**
  - After removing duplicates: **5117 (-2289)**
  - Matching Twitter names: **2756 (-2361)**

### Evolutionary Biologists

Names of evolutionary biologists were collected from the Member
Directory of the [Evolution Directory
(Evoldir)](https://evol.mcmaster.ca/evoldir.html).

Data collection date: 18 November 2019  
Twitter extraction date: 17 October 2020

Sample size:

  - Number extracted from Evoldir: **10749**
  - Matching Twitter names: **5186 (-5563)**

### Evolutionary Psychologists

Names of evolutionary psychologists were collected from the Member
Directory of the [Human Behavior & Evolution Society
(HBES)](https://www.hbes.com/).

Data collection date: 18 November 2019  
Twitter extraction date: 17 October 2020

Sample size:

  - Number extracted from HBES: **579**
  - Matching Twitter names: **336 (-243)**

## R Script Summaries

### 1\. data\_cleaning.R

This script takes the raw names, countries, and text copied from the
databases and converts it into tidy datasets that are easier to work
with.

### 2\. extract\_user\_bios.R

In this script, the names extracted from the databases is searched on
Twitter. Any matches are saved, and Twitter bios are searched to
determine if they contain any keywords that might indicate that they are
an academic (e.g., academia, PhD, researcher). Other info extracted
includes the number of friends, followers, Twitter location, account
creation date, and number of twitter posts. Their likely gender (based
on historical name data) is added. The Twitter API only permits 900
requests per 15 minutes, so the script pauses once this limit is reached
and starts again when the limit resets. Where there are multiple
results, these are saved as separate rows. The data are saved as csv
files and google sheet files, ready to be manually rated using a Shiny
app.

### 3\. rate\_evo\_psychs

This is a Shiny App that allows users to manually rate which of multiple
Twitter users extracted in the script above, if any, is an evolutionary
psychologist. Users are displayed in a table, together with their
Twitter name and Twitter bio. The user selects the correct user, or
skips if none appear to be academics. Results are saved in a
googlesheets file, which can then be downloaded.

### 4\. add\_location.R

Twitter locations, where they exist, can include countries, cities, or
suburbs. In this script the Google Maps API is used to standardize
location by extracting latitude, longitude, and country from location
data.

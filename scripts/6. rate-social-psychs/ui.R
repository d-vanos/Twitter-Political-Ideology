

library(shiny)

# Text paragraphs 

info_text <- "<h3> General Information </h3>
This app allows a user to manually rate the twitter bios of evolutionary psychology, biology, and social psychology academics/researchers. 
Toggle through the menu at the top to access these tasks.<br> <br> 

<b> Taking a break: </b> You are welcome to close the app (website) whenever you wish. It will start back up where you left off. <br><br>

<b> Loading: </b> Because of the size of the data it takes a while to load. Please be patient. <br><br>

<b> Saving: </b> The data for each of the tasks will be automatically uploaded to google sheets every 10 profiles. If you would like to exit the program,
or you want to upload the data at any point yourself, you can do so using 'Manual Upload'.<br><br>

<b> About the app: </b> The purpose of this set of apps is to create a dataset of evolutionary psychologists, biologists, and social psychologists. 
This particular app focuses on social psychologists. In cases where there is only one option, you are asked to decide whether this person is a
social psychologist. If there are multiple options, you are asked to decide which - if any - is a social psychologist. If there are no correct options,
do not click on any names and instead press 'Next'."

social_text_instructions <- "<h3> Instructions - Social Psychology Dataset </h3>
In this task you are asked to make a decision about which of a list of twitter users is most likely to be a social psychologist (e.g., researcher, 
academic), based on their twitter bio and whether their twitter name matches the name present in the member directory for the Society for Personality and
Social Psychology (SPSP). There may be cases where there is only one user. In that case,
simply decide whether that person is a social psychologist. If none of the twitter users appear to be a social psychologists, or if you are 
unsure, simply press 'Next'. If you accidentally select a user, click on their name again to deselect. You can use the 'Back' and 'Next' buttons to toggle 
through the users. <br> <br>

<ul> Based on their twitter bio: 
<li> <b> If 1 option: </b> Is this a social psychologist? 
<li> <b> If 2+ options: </b> Which person, if any, is a social psychologist?
</ul>

<br> <br>"

social_text_info <- "<br> <br>Concerned about issues with the data entry? <a href='https://docs.google.com/spreadsheets/d/1VrZfZEhRAK-_sHVmtvKNTIieE47Fkgpk9FrDL68sAE0/edit?usp=sharing'>
See the raw dataset here.</a> Warning: right-click and copy this URL to avoid exiting the app and having to reload the data. <br><br> " 

shinyUI(fluidPage(
    
    # App title
    titlePanel("Validate Social Psychologist Twitter Profiles"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Info",
                         HTML(info_text)),
                
                tabPanel("Social dataset",
                         HTML(social_text_instructions),
                         column(12, align = "center",
                                actionButton("social_decrease", "Back"), 
                                actionButton("social_increase", "Next")
                         ),
                         DT::dataTableOutput("social_table"),
                         textOutput("social_count"),
                         textOutput("social_total"),
                         HTML("<br>"),
                         actionButton("social_upload", "Manual Upload"),
                         downloadButton("social_download", "Download"),
                         HTML(social_text_info))
                )
))

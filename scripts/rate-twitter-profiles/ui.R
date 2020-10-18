

library(shiny)

# Text paragraphs 

info_text <- "<h3> General Information </h3>
This app allows a user to manually rate the twitter bios of evolutionary psychology, biology, and social psychology academics/researchers. 
Toggle through the menu at the top to access these tasks.<br> <br> 

<b> Taking a break: </b> You are welcome to close the app (website) whenever you wish. It will start back up where you left off. <br><br>

<b> Loading: </b> Because of the size of the data it takes a while to load. Please be patient. <br><br>

<b> Saving: </b> The data for each of the tasks will be automatically uploaded to google sheets every 10 profiles. If you would like to exit the program,
or you want to upload the data at any point yourself, you can do so using 'Manual Upload'.<br><br>

<b> About the app: </b> The purpose of this app is to create a dataset of evolutionary psychologists, biologists, and social psychologists. In cases where 
there is only one option, you are asked to decide whether this person is [relevant academic e.g., evo psych]. If there are multiple options, you are asked 
to decide which - if any - is [relevant academic]. If there are no correct options, do not click on any names and instead press 'Next'."

evo_text_instructions <- " <h3> Instructions - Evolutionary Psychology Dataset </h3>
In this task you are asked to make a decision about which of a list of twitter users is most likely to be an evolutionary psychologist (e.g., researcher, 
academic), based on their twitter bio and whether their twitter name matches the Human Behaviour and Evolution Society (HBES) list name. There may be cases 
where there is only one user. In that case,
simply decide whether that person is an evolutionary psychologist. If none of the twitter users appear to be an evolutionary psychologists, or if you are 
unsure, simply press 'Next'. If you accidentally select a user, click on their name again to deselect. You can use the 'Back' and 'Next' buttons to toggle 
through the users. <br> <br>

<ul> Based on their twitter bio: 
<li> <b> If 1 option: </b> Is this an evolutionary psychologist? 
<li> <b> If 2+ options: </b> Which person, if any, is an evolutionary psychologist?
</ul>

<br> <br>"

evo_text_info <- "<br> <h4> How this dataset was created </h4>
This dataset was created by searching for users whose name matched (according to Twitter's search algorithm) with a name in the HBES list. This list was 
refined by filtering out users whose Twitter bios did not match at least one 'academic' keyword (e.g., evolution, academic, research, PhD, Dr, etc.).
Sometimes, more than one person matched both these criteria (came up in the search, and had an academic keyword). Other times, people whose name did not 
match the HBES list were still extracted, due to the way Twitter searches. Last, some extracted users are not academics, even though they matched one of 
the keywords. Hence, this task is to make sure that the final dataset consists only of evolutionary psychologists. <br> <br>

Concerned about issues with the data entry? <a href='https://docs.google.com/spreadsheets/d/15c_7wz0Mt62nLHFGtyWfkAX4BexDwVncYtUIyCYgnZQ/edit?usp=sharing'>
See the raw dataset here.</a> <br><br> "

bio_text_instructions <- "<h3> Instructions - Evolutionary Biology Dataset </h3>
In this task you are asked to make a decision about which of a list of twitter users is most likely to be an evolutionary biologist (e.g., researcher, 
academic), based on their twitter bio and whether their twitter name matches the name present in the member directory of the Evolution Directory (EvolDir). 
There may be cases where there is only one user. In that case,
simply decide whether that person is an evolutionary biologist. If none of the twitter users appear to be an evolutionary biologists, or if you are 
unsure, simply press 'Next'. If you accidentally select a user, click on their name again to deselect. You can use the 'Back' and 'Next' buttons to toggle 
through the users. <br> <br>

<ul> Based on their twitter bio: 
<li> <b> If 1 option: </b> Is this an evolutionary biologist? 
<li> <b> If 2+ options: </b> Which person, if any, is an evolutionary biologist?
</ul>

<br> <br>"


bio_text_info <- "<br> <br>Concerned about issues with the data entry? <a href='https://docs.google.com/spreadsheets/d/17qnzHRX4RnJQfVCF1XmL0JZVruaFYROHph8GZjxIALU/edit?usp=sharing'>
See the raw dataset here.</a> <br><br> "

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
See the raw dataset here.</a> <br><br> " 

shinyUI(fluidPage(
    
    # App title
    titlePanel("Validate Twitter Academic Profiles"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Info",
                         HTML(info_text)),
                tabPanel("Evo dataset",
                         HTML(evo_text_instructions),
                         column(12, align = "center",
                                actionButton("evo_decrease", "Back"), 
                                actionButton("evo_increase", "Next")
                                ),
                         DT::dataTableOutput("evo_table"),
                         textOutput("evo_count"),
                         textOutput("evo_total"),
                         HTML("<br>"),
                         actionButton("evo_upload", "Manual Upload"),
                         downloadButton("evo_download", "Download"),
                         HTML(evo_text_info)),
                
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
                         HTML(social_text_info)),
    
                tabPanel("Bio dataset",
                         HTML(bio_text_instructions),
                         column(12, align = "center",
                                actionButton("bio_decrease", "Back"), 
                                actionButton("bio_increase", "Next")
                         ),
                         DT::dataTableOutput("bio_table"),
                         textOutput("bio_count"),
                         textOutput("bio_total"),
                         HTML("<br>"),
                actionButton("bio_upload", "Manual Upload"),
                downloadButton("bio_download", "Download"),
                HTML(bio_text_info))),
    )
)

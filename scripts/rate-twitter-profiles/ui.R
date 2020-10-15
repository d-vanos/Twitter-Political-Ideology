

library(shiny)

# Text paragraphs 

info_text <- "<h3> General Information </h3>
This app allows a user to manually rate the twitter bios of evolutionary psychology, biology, and social psychology academics/researchers. 
Toggle through the menu at the top to access these tasks. Please rate the Evo dataset first, as the Bio and Social Psych datasets require
a fully rated, completely accurate Evo dataset. This app comes with associated datasets, sorted and saved in folders. Please make sure that
you do not change the file names, as this will cause issues when you try to restart a session. <br><br>

<b> Taking a break: </b> You are welcome to close the app (website) whenever you wish. It will start back up where you left off. <br><br>

<b> Saving: </b> The data for each of the tasks will be automatically saved in a .csv file every 50 profiles. It will also be saved when you 
close the program. You are also welcome to save the data at any time.<br><br>

<b> About the app: </b> The purpose of this app is to create a dataset of evolutionary psychologists, biologists, and social psychologists, 
that are as closely matched as possible based on number of twitter friends, number of twitter followers, gender, and country specified on Twitter.<br>
    &emsp; First, the user (you) will select whether a Twitter user appears to be an evolutionary psychologist, based on their Twitter bio. Once complete, a
dataset of users likely to be biology and social psychology researchers are matched with dataset of evolutionary psychologists. The user is then 
asked to go through the biology and social psychology datasets to determine whether the users appear to be biologists or social psychologists, respectively.<br>
    &emsp;Matching of biologists and social psychologists with evolutionary psychologists occurs using the pairmatch function from the optmatch library,
which 'finds a pairing of treatment units to controls that minimizes the sum of discrepancies.' Because the biology and social psychology datasets 
are very large and there are many cases where there appears to be more than one correct user, only cases with only 1 correct user were included. 
These were matched to the evolutionary psychology dataset, and will be manually checked."

evo_text_instructions <- " <h3> Instructions - Evo Psych Dataset </h3>
In this task you are asked to make a decision about which of a list of twitter users is most likely to be an evolutionary psychologist (e.g., researcher, 
academic).based on their twitter bio and whether their twitter name matches the HBES list name. There may be cases where there is only one user. In that case,
simply decide whether that person is an evolutionary psychologist. If none of the twitter users appear to be an evolutionary psychologists, or if you are 
unsure, simply press 'Next'. If you accidentally select a user, click on their name again to deselect. Your current selection will be displayed under the 
table. You can use the 'Back' and 'Next' buttons to toggle through the users. <br> <br>

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

Concerned about issues with the data entry? <a href='https://docs.google.com/spreadsheets/d/1rqghfdHF3ybOcnwI75O3Dsu-i_j8fIG0jCHL48zLJbc/edit?usp=sharing'>
See the raw dataset here.</a> <br><br> "

bio_text <- "<h3> Instructions - Biology Dataset </h3>
In this task you are asked to make a decision about whether a twitter bio belongs to a biologist (e.g., researcher, academic). If you believe the twitter
bio belongs to a biologist, press 'Yes' button. If you believe it is not the twitter bio of a biologist, press the 'No' button. You will automatically be
presented with a new bio each time you make a decision, but if you make a wrong decision, you can use the 'Back' and 'Next' buttons to toggle through the 
users.<br><br>

The program automatically matches Twitter users likely to be biologists with evolutionary psychologists. The user (you) then confirms whether they are 
actually biologists based on the description on their Twitter bio. If they do not appear to be biologists, these people are removed from the dataset, and 
matching occurs again with the new dataset. Eventually, after multiple rounds of manual evaluations, a full list of biologists should be matched to a list
of evolutionary psychologists. At this point, it will say 'You're done!'"

social_text <- "<h3> Instructions - Social Psychology Dataset </h3>
In this task you are asked to make a decision about whether a twitter bio belongs to a social psychologist (e.g., researcher, academic). This task is 
exactly the same as the biologist task, but with social psychologists. If you believe the twitter bio belongs to a social psychologist, press 'Yes' button.
If you believe it is not the twitter bio of a social psychologist, press the 'No' button. You will automatically be presented with a new bio each time you 
make a decision, but if you make a wrong decision, you can use the 'Back' and 'Next' buttons to toggle through the users.<br><br>

The program automatically matches Twitter users likely to be social psychologists with evolutionary psychologists. The user (you) then confirms whether they
are actually social psychologists based on the description on their Twitter bio. If they do not appear to be social psychologists, these people are removed 
from the dataset, and matching occurs again with the new dataset. Eventually, after multiple rounds of manual evaluations, a full list of social psychologists
should be matched to a list of evolutionary psychologists. At this point, it will say 'You're done!'"

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # App title
    titlePanel("Validate Twitter Academic Profiles"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Info",
                         HTML(info_text)),
                tabPanel("Evo dataset",
                         HTML(evo_text_instructions),
                         column(12, align = "center",
                                actionButton("decrease", "Back"), 
                                actionButton("increase", "Next")
                                ),
                         DT::dataTableOutput("evo_table"),
                         textOutput("count"),
                         HTML(evo_text_info),
                         actionButton("upload", "Manual Upload"),
                         downloadButton("downloadData", "Download")),
                
                
                
                tabPanel("Bio dataset",
                         HTML(bio_text)),
                
                
                tabPanel("Social psych dataset",
                         HTML(social_text)))
    )
)

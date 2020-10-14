#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(DT)
library(googlesheets4)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Initialising counter
    evo_counter <- reactiveValues(countervalue = 0) 

    # Import data 
    # evo_ID <- gs4_find("hbes_3_extract_user")$id
    # evo_extracted <- read_sheet(evo_ID, sheet = 1)
    # evo_extracted <- evo %>% select(ID, Name, Twitter_Bio)
    data <- reactiveValues()
    data$evo_extracted <- evo_extracted

    # Creating tables 
    output$evo_table = DT::renderDataTable({data$evo_extracted %>% 
                                                filter(ID == evo_counter$countervalue) %>% 
                                                select(-ID)},
            selection = 'single',
            options = list(dom = 't', # Only shows table (no search, number of entries, etc.)
                           ordering=F)) # Stops sorting columns 
    
    # Click Next and Back 
    observeEvent(input$increase, {
        row_ID <- which(data$evo_extracted$'ID' == evo_counter$countervalue)
        data$evo_extracted$'selected'[row_ID] <- 0
        data$evo_extracted$'selected'[row_ID[1] + input$evo_table_rows_selected - 1] <- 1 # Of the people with a particular ID, selecting the correct row
        evo_counter$countervalue = evo_counter$countervalue + 1
    })
    
    observeEvent(input$decrease, {
        evo_counter$countervalue = evo_counter$countervalue - 1
    })
    
    # Saving the data every 10 people
    observeEvent(evo_counter$countervalue %% 10 == 0, {
        if(evo_counter$countervalue > 5 & evo_counter$countervalue %% 10 == 0){ # Otherwise it has an issue with the countervalue 
            
            start_row <- which(data$evo_extracted$'ID' == evo_counter$countervalue - 10)[1] # The first row of the countervalue 5 people ago 
            current_row <- which(data$evo_extracted$'ID' == evo_counter$countervalue)[length(which(data$evo_extracted$'ID' == evo_counter$countervalue))] # The last row of the most recent countervalue
            
            save_data <- as_tibble(data$evo_extracted$'selected'[start_row:current_row])
            range <- paste0("R", start_row+1, ":R", current_row+1)
            
            range_write(ss = evo_ID,
                        data = save_data,
                        range = range,
                        col_names = FALSE)
        }
    })
    
    # Save button so people can upload the data 
    
    # Download
    output$downloadData <- downloadHandler(
        filename = "evo_extracted.csv",
        content = function(file) {
            write.csv(data$evo_extracted, file, row.names = FALSE)
        })

})

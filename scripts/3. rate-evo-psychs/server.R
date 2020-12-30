
library(shiny)
library(tidyverse)
library(DT)
library(googlesheets4)
library(googledrive)

shinyServer(function(input, output) {

    options(
        gargle_oauth_cache = ".secrets",
        gargle_oauth_email = "dvanos@outlook.com.au"
    )
    
    drive_auth()
    sheets_auth()
    
    # FUNCTIONS
    # Reduce datasets down
    reduce <- function(data){
        data <- data %>%
            select(ID, Name, Twitter_Bio, Twitter_Handle, selected) %>%
            mutate(ID = as.numeric(ID),
                   selected = as.numeric(selected),
                   selected = ifelse(selected == -99, NA, selected))
        return(data)
    }
    
    # Round
    round_down <- function(num){floor(num/10)*10}
    
    # Set reactive values
    data <- reactiveValues()
    
    # EVO
    # Import data
    evo_ID <- gs4_find("hbes_3_extract_user")$id
    evo_extracted <- read_sheet(evo_ID, sheet = 1)
    
    # Format
    evo_extracted <- reduce(evo_extracted)
    
    # Initialising counter
    evo_counter <- reactiveValues(countervalue = evo_extracted$'ID'[which(is.na(evo_extracted$selected))[1]])
    evo_NAs <- ifelse(is.na(evo_extracted$'ID'[which(is.na(evo_extracted$selected))[1]]), 0, evo_extracted$'ID'[which(is.na(evo_extracted$selected))[1]])
    evo_counter_upload <- reactiveValues(countervalue = 10-evo_NAs)
    
    data$evo_extracted <- evo_extracted
    
    # Creating tables
    output$evo_table = DT::renderDataTable({data$evo_extracted %>%
            filter(ID == evo_counter$countervalue) %>%
            select(-ID)},
            selection = 'single',
            options = list(dom = 't', # Only shows table (no search, number of entries, etc.)
                           ordering=F)) # Stops sorting columns
    
    # Number of entries before upload
    output$evo_count <- renderText({
        paste("Number of entries before upload:", evo_counter_upload$countervalue)   # print the latest value stored in the reactiveValues object
    })
    
    output$evo_total <- renderText({
        paste("Total left:", (max(data$evo_extracted$'ID') - evo_counter$countervalue))  
    })

    #Click Next
    observeEvent(input$evo_increase, {
        row_ID <- which(data$evo_extracted$'ID' == evo_counter$countervalue)
        data$evo_extracted$'selected'[row_ID] <- 0
        data$evo_extracted$'selected'[row_ID[1] + input$evo_table_rows_selected - 1] <- 1 # Of the people with a particular ID, selecting the correct row
        evo_counter$countervalue = evo_counter$countervalue + 1
        evo_counter_upload$countervalue = evo_counter_upload$countervalue - 1
    })


    # Click Back
    observeEvent(input$evo_decrease, {
        evo_counter$countervalue = evo_counter$countervalue - 1
    })

    # Upload
    observeEvent(input$evo_upload, {
        evo_current_row <- which(data$evo_extracted$'ID' == evo_counter$countervalue)[length(which(data$evo_extracted$'ID' == evo_counter$countervalue))]
        evo_last_saved_row <- which(data$evo_extracted$'ID' == round_down(evo_counter$countervalue))[1]
        evo_save_data <- as_tibble(data$evo_extracted$'selected'[evo_last_saved_row:evo_current_row])
        colnames(evo_save_data) <- "selected"
        evo_save_data <- evo_save_data %>% mutate(selected = ifelse(is.na(selected), -99, selected))
        evo_range <- paste0("R", evo_last_saved_row+1, ":R", evo_current_row+1)

        range_write(ss = evo_ID,
                    data = evo_save_data,
                    range = evo_range,
                    col_names = FALSE)
    })


    # Saving the data every 10 people
    observeEvent(evo_counter$countervalue %% 10 == 0, {
        if(evo_counter$countervalue > 5 & evo_counter$countervalue %% 10 == 0){ # Otherwise it has an issue with the countervalue

            start_row <- which(data$evo_extracted$'ID' == evo_counter$countervalue - 10)[1] # The first row of the countervalue 5 people ago
            current_row <- which(data$evo_extracted$'ID' == evo_counter$countervalue-1)[length(which(data$evo_extracted$'ID' == evo_counter$countervalue-1))] # The last row of the most recent countervalue

            evo_save_data <- as_tibble(data$evo_extracted$'selected'[start_row:current_row])
            evo_range <- paste0("R", start_row+1, ":R", current_row+1)

            range_write(ss = evo_ID,
                        data = evo_save_data,
                        range = evo_range,
                        col_names = FALSE)

            evo_counter_upload$countervalue = 10
        }
    })
    

    # Download
    output$evo_download <- downloadHandler(
        filename = "evo_extracted.csv",
        content = function(file) {
            write_csv(data$evo_extracted, na = "", file)
        })

})

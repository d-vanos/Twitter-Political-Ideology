
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
    
    # SOCIAL
    social_ID <- gs4_find("spsp_3_extract_user")$id
    social_extracted <- read_sheet(social_ID, sheet = 1)
    
    # Format
    social_extracted <- reduce(social_extracted)

    # Initialising counter
    social_counter <- reactiveValues(countervalue = social_extracted$'ID'[which(is.na(social_extracted$selected))[1]])
    social_NAs <- ifelse(is.na(social_extracted$'ID'[which(is.na(social_extracted$selected))[1]]), 0, social_extracted$'ID'[which(is.na(social_extracted$selected))[1]])
    social_counter_upload <- reactiveValues(countervalue = 10-social_NAs)
    
    data$social_extracted <- social_extracted
    
    # Creating table
    output$social_table = DT::renderDataTable({data$social_extracted %>%
            filter(ID == social_counter$countervalue) %>%
            select(-ID)},
            selection = 'single',
            options = list(dom = 't', 
                           ordering=F)) 
    
   

    # Number of entries before upload
    output$social_count <- renderText({
        paste("Number of entries before upload:", social_counter_upload$countervalue) 
    })
    
    output$social_total <- renderText({
        paste("Total left:", (max(data$social_extracted$'ID') - social_counter$countervalue))  
    })
    
    #Click Next
    observeEvent(input$social_increase, {
        row_ID <- which(data$social_extracted$'ID' == social_counter$countervalue)
        data$social_extracted$'selected'[row_ID] <- 0
        data$social_extracted$'selected'[row_ID[1] + input$social_table_rows_selected - 1] <- 1 # Of the people with a particular ID, selecting the correct row
        social_counter$countervalue = social_counter$countervalue + 1
        social_counter_upload$countervalue = social_counter_upload$countervalue - 1
    })
    
    # Click Back
    observeEvent(input$social_decrease, {
        social_counter$countervalue = social_counter$countervalue - 1
    })
    
    # Upload
    observeEvent(input$social_upload, {
        social_current_row <- which(data$social_extracted$'ID' == social_counter$countervalue)[length(which(data$social_extracted$'ID' == social_counter$countervalue))]
        social_last_saved_row <- which(data$social_extracted$'ID' == round_down(social_counter$countervalue))[1]
        social_save_data <- as_tibble(data$social_extracted$'selected'[social_last_saved_row:social_current_row])
        colnames(social_save_data) <- "selected"
        social_save_data <- social_save_data %>% mutate(selected = ifelse(is.na(selected), -99, selected))
        social_range <- paste0("Q", social_last_saved_row+1, ":Q", social_current_row+1)
        
        range_write(ss = social_ID,
                    data = social_save_data,
                    range = social_range,
                    col_names = FALSE)
    })

    # Saving the data every 10 people
    observeEvent(social_counter$countervalue %% 10 == 0, {
        if(social_counter$countervalue > 5 & social_counter$countervalue %% 10 == 0){ # Otherwise it has an issue with the countervalue

            start_row <- which(data$social_extracted$'ID' == social_counter$countervalue - 10)[1] # The first row of the countervalue 5 people ago
            current_row <- which(data$social_extracted$'ID' == social_counter$countervalue-1)[length(which(data$social_extracted$'ID' == social_counter$countervalue-1))] # The last row of the most recent countervalue

            social_save_data <- as_tibble(data$social_extracted$'selected'[start_row:current_row])
            social_range <- paste0("Q", start_row+1, ":Q", current_row+1)

            range_write(ss = social_ID,
                        data = social_save_data,
                        range = social_range,
                        col_names = FALSE)

            social_counter_upload$countervalue = 10
        }
    })


    # Download
    output$social_download <- downloadHandler(
        filename = "social_extracted.csv",
        content = function(file) {
            write_csv(data$social_extracted, na = "", file)
        })
})

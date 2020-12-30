
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
    
    # BIO
    bio_ID <- gs4_find("evoldir_3_extract_user")$id
    bio_extracted <- read_sheet(bio_ID, sheet = 1)
    
    # Format
    bio_extracted <- reduce(bio_extracted)

    # Initialising counter
    bio_counter <- reactiveValues(countervalue = bio_extracted$'ID'[which(is.na(bio_extracted$selected))[1]])
    bio_NAs <- ifelse(is.na(bio_extracted$'ID'[which(is.na(bio_extracted$selected))[1]]), 0, bio_extracted$'ID'[which(is.na(bio_extracted$selected))[1]])
    bio_counter_upload <- reactiveValues(countervalue = 10-bio_NAs)

    data$bio_extracted <- bio_extracted
    
    # Creating table
    output$bio_table = DT::renderDataTable({data$bio_extracted %>%
            filter(ID == bio_counter$countervalue) %>%
            select(-ID)},
            selection = 'single',
            options = list(dom = 't', 
                           ordering=F)) 

    # Number of entries before upload
    output$bio_count <- renderText({
        paste("Number of entries before upload:", bio_counter_upload$countervalue)   
    })
    
    output$bio_total <- renderText({
        paste("Total left:", (max(data$bio_extracted$'ID') - bio_counter$countervalue))  
    })

    #Click Next
    observeEvent(input$bio_increase, {
        row_ID <- which(data$bio_extracted$'ID' == bio_counter$countervalue)
        data$bio_extracted$'selected'[row_ID] <- 0
        data$bio_extracted$'selected'[row_ID[1] + input$bio_table_rows_selected - 1] <- 1 # Of the people with a particular ID, selecting the correct row
        bio_counter$countervalue = bio_counter$countervalue + 1
        bio_counter_upload$countervalue = bio_counter_upload$countervalue - 1
    })

    # Click Back
    observeEvent(input$bio_decrease, {
        bio_counter$countervalue = bio_counter$countervalue - 1
    })

    # Upload
    observeEvent(input$bio_upload, {
        bio_current_row <- which(data$bio_extracted$'ID' == bio_counter$countervalue)[length(which(data$bio_extracted$'ID' == bio_counter$countervalue))]
        bio_last_saved_row <- which(data$bio_extracted$'ID' == round_down(bio_counter$countervalue))[1]
        bio_save_data <- as_tibble(data$bio_extracted$'selected'[bio_last_saved_row:bio_current_row])
        colnames(bio_save_data) <- "selected"
        bio_save_data <- bio_save_data %>% mutate(selected = ifelse(is.na(selected), -99, selected))
        bio_range <- paste0("Q", bio_last_saved_row+1, ":Q", bio_current_row+1)
        
        range_write(ss = bio_ID,
                    data = bio_save_data,
                    range = bio_range,
                    col_names = FALSE)
    })

    # Saving the data every 10 people
    observeEvent(bio_counter$countervalue %% 10 == 0, {
        if(bio_counter$countervalue > 5 & bio_counter$countervalue %% 10 == 0){ # Otherwise it has an issue with the countervalue

            start_row <- which(data$bio_extracted$'ID' == bio_counter$countervalue - 10)[1] # The first row of the countervalue 5 people ago
            current_row <- which(data$bio_extracted$'ID' == bio_counter$countervalue-1)[length(which(data$bio_extracted$'ID' == bio_counter$countervalue-1))] # The last row of the most recent countervalue

            bio_save_data <- as_tibble(data$bio_extracted$'selected'[start_row:current_row])
            bio_range <- paste0("Q", start_row+1, ":Q", current_row+1)

            range_write(ss = bio_ID,
                        data = bio_save_data,
                        range = bio_range,
                        col_names = FALSE)

            bio_counter_upload$countervalue = 10
        }
    })

    # Download
    output$bio_download <- downloadHandler(
        filename = "bio_extracted.csv",
        content = function(file) {
            write_csv(data$social_extracted, na = "", file)
        })
    

})

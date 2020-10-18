
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
    output$evo_count <- renderText({
        paste("Number of entries before upload:", evo_counter_upload$countervalue)   # print the latest value stored in the reactiveValues object
    })
    
    output$evo_total <- renderText({
        paste("Total left:", (max(data$evo_extracted$'ID') - evo_counter$countervalue))  
    })
    
    output$social_count <- renderText({
        paste("Number of entries before upload:", social_counter_upload$countervalue) 
    })
    
    output$social_total <- renderText({
        paste("Total left:", (max(data$social_extracted$'ID') - social_counter$countervalue))  
    })
    
    output$bio_count <- renderText({
        paste("Number of entries before upload:", bio_counter_upload$countervalue)   
    })
    
    output$bio_total <- renderText({
        paste("Total left:", (max(data$bio_extracted$'ID') - bio_counter$countervalue))  
    })

    #Click Next
    observeEvent(input$evo_increase, {
        row_ID <- which(data$evo_extracted$'ID' == evo_counter$countervalue)
        data$evo_extracted$'selected'[row_ID] <- 0
        data$evo_extracted$'selected'[row_ID[1] + input$evo_table_rows_selected - 1] <- 1 # Of the people with a particular ID, selecting the correct row
        evo_counter$countervalue = evo_counter$countervalue + 1
        evo_counter_upload$countervalue = evo_counter_upload$countervalue - 1
    })
    
    observeEvent(input$social_increase, {
        row_ID <- which(data$social_extracted$'ID' == social_counter$countervalue)
        data$social_extracted$'selected'[row_ID] <- 0
        data$social_extracted$'selected'[row_ID[1] + input$social_table_rows_selected - 1] <- 1 # Of the people with a particular ID, selecting the correct row
        social_counter$countervalue = social_counter$countervalue + 1
        social_counter_upload$countervalue = social_counter_upload$countervalue - 1
    })
    
    observeEvent(input$bio_increase, {
        row_ID <- which(data$bio_extracted$'ID' == bio_counter$countervalue)
        data$bio_extracted$'selected'[row_ID] <- 0
        data$bio_extracted$'selected'[row_ID[1] + input$bio_table_rows_selected - 1] <- 1 # Of the people with a particular ID, selecting the correct row
        bio_counter$countervalue = bio_counter$countervalue + 1
        bio_counter_upload$countervalue = bio_counter_upload$countervalue - 1
    })

    # Click Back
    observeEvent(input$evo_decrease, {
        evo_counter$countervalue = evo_counter$countervalue - 1
    })
    
    observeEvent(input$social_decrease, {
        social_counter$countervalue = social_counter$countervalue - 1
    })
    
    observeEvent(input$bio_decrease, {
        bio_counter$countervalue = bio_counter$countervalue - 1
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
    output$evo_download <- downloadHandler(
        filename = "evo_extracted.csv",
        content = function(file) {
            write_csv(data$evo_extracted, na = "", file)
        })
    
    output$social_download <- downloadHandler(
        filename = "social_extracted.csv",
        content = function(file) {
            write_csv(data$social_extracted, na = "", file)
        })
    
    output$bio_download <- downloadHandler(
        filename = "bio_extracted.csv",
        content = function(file) {
            write_csv(data$social_extracted, na = "", file)
        })
    

})

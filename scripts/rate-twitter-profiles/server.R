
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

    # Import data
    evo_ID <- gs4_find("hbes_3_extract_user")$id
    evo_extracted <- read_sheet(evo_ID, sheet = 1)
    evo_extracted <- evo_extracted %>%
        select(ID, Name, Twitter_Bio, Twitter_Handle, selected) %>%
        mutate(ID = as.numeric(ID),
               selected = as.numeric(selected),
               selected = ifelse(selected == -99, NA, selected))
    data <- reactiveValues()
    data$evo_extracted <- evo_extracted


    # Initialising counter
    evo_counter <- reactiveValues(countervalue = evo_extracted$'ID'[which(is.na(evo_extracted$selected))[1]])
    evo_counter_upload <- reactiveValues(countervalue = 10-evo_extracted$'ID'[which(is.na(evo_extracted$selected))[1]])

    # Creating tables
    output$evo_table = DT::renderDataTable({data$evo_extracted %>%
                                                filter(ID == evo_counter$countervalue) %>%
                                                select(-ID)},
            selection = 'single',
            options = list(dom = 't', # Only shows table (no search, number of entries, etc.)
                           ordering=F)) # Stops sorting columns

    # Number of entries before upload
    output$count <- renderText({
        paste("Number of entries before upload:", evo_counter_upload$countervalue)   # print the latest value stored in the reactiveValues object
    })

    #Click Next and Back
    observeEvent(input$increase, {
        row_ID <- which(data$evo_extracted$'ID' == evo_counter$countervalue)
        data$evo_extracted$'selected'[row_ID] <- 0
        data$evo_extracted$'selected'[row_ID[1] + input$evo_table_rows_selected - 1] <- 1 # Of the people with a particular ID, selecting the correct row
        evo_counter$countervalue = evo_counter$countervalue + 1
        evo_counter_upload$countervalue = evo_counter_upload$countervalue - 1
    })

    observeEvent(input$decrease, {
        evo_counter$countervalue = evo_counter$countervalue - 1
    })

    observeEvent(input$upload, {
        current_row <- which(data$evo_extracted$'ID' == evo_counter$countervalue)[length(which(data$evo_extracted$'ID' == evo_counter$countervalue))]
        save_data <- as_tibble(data$evo_extracted$'selected'[1:current_row])
        colnames(save_data) <- "selected"
        range <- paste0("R2:R", current_row+1)

        range_write(ss = evo_ID,
                    data = save_data,
                    range = range)

    })

    # Saving the data every 10 people
    observeEvent(evo_counter$countervalue %% 10 == 0, {
        if(evo_counter$countervalue > 5 & evo_counter$countervalue %% 10 == 0){ # Otherwise it has an issue with the countervalue

            start_row <- which(data$evo_extracted$'ID' == evo_counter$countervalue - 10)[1] # The first row of the countervalue 5 people ago
            current_row <- which(data$evo_extracted$'ID' == evo_counter$countervalue-1)[length(which(data$evo_extracted$'ID' == evo_counter$countervalue-1))] # The last row of the most recent countervalue

            save_data <- as_tibble(data$evo_extracted$'selected'[start_row:current_row])
            range <- paste0("R", start_row+1, ":R", current_row+1)

            range_write(ss = evo_ID,
                        data = save_data,
                        range = range,
                        col_names = FALSE)

            evo_counter_upload$countervalue = 10
        }
    })

    # Download
    output$downloadData <- downloadHandler(
        filename = "evo_extracted.csv",
        content = function(file) {
            write.csv(data$evo_extracted, file, row.names = FALSE)
        })

})

##### study_v2_online\app.r ------
source('global.r', local = TRUE) ## Contains setup, global varibles and local functions.

####### Server function, for shiny app
server <- function(input, output, session) {
  ##### Reavtive value initialization -----
  rv                  <- reactiveValues()
  rv$pg               <- 1L
  rv$timer            <- 999L
  rv$stopwatch        <- 0L
  rv$timer_active     <- TRUE
  rv$pca_inter        <- 1L
  rv$manual_inter     <- 1L
  rv$resp_inter       <- 1L
  rv$task_ttr         <- NULL
  rv$task_response    <- NULL
  rv$save_file        <- NULL
  rv$resp_tbl         <- NULL
  rv$training_aes     <- FALSE
  rv$second_training  <- FALSE
  rv$curr_basis       <- NULL
  rv$manual_ls        <- list()
  rv$basis_ls         <- list()
  
  ##### Reactive functions -----
  sim_num <- reactive({
    
  })
  p    <- reactive({ ncol(dat()) })
  dat <- reactive({  })
 
  ?uiOutput()
  
  ##### Start observes -----
  
  ### Obs save reponses button -----
  observeEvent(input$save_resp, {
    filebase = paste("responses", this_group, Sys.info()[4], sep = "_")
    prefix = ""
    
    ## Write survey responses to rv$resp_tbl
    ins_row_start <- nrow(rv$resp_tbl) - l_survey_questions + 1
    ins_row_end   <- nrow(rv$resp_tbl)
    rv$resp_tbl$response[ins_row_start:ins_row_end] <- rv$task_response
    rv$resp_tbl$ttr[ins_row_start:ins_row_end] <- rv$task_ttr
    
    ## Do the actual saving
    save_base <- paste0(prefix, filebase, "_")
    save_num  <- 1
    save_name <- sprintf(paste0(save_base, "%03d"), save_num)
    save_file <- paste0(save_name, ".csv")
    while (file.exists(save_file)){ ## set the correct file number to use
      save_name <- sprintf(paste0(save_base, "%03d"), save_num)
      save_file <- paste0(save_name, ".csv")
      save_num  <- save_num + 1
    }
    assign(save_name, rv$resp_tbl)
    write.csv(get(save_name), file = save_file, row.names = FALSE)
    rv$save_file <- save_file
    
    save_msg <- paste0("Reponses saved as ", save_file, " (log file: ", log_file, "). Thank you for participating!")
    output$save_msg <- renderText(app_html_red(save_msg))
    
    if (prefix == "") {
      loggit("INFO", "Save button pressed.", 
             paste0("rv$save_file: ", rv$save_file, 
                    ". save_msg: ", save_msg,
                    "."))
    }
    if (prefix != "") {
      loggit("INFO", paste0("NOTE: Prefixed save script run. Prefix was '", prefix, "'."), 
             paste0("Save may not have been user initiated",
                    ". PREFIX USED: ", prefix,
                    ". rv$save_file: ", rv$save_file, 
                    ". save_msg: ", save_msg, "."))
    }
  })
  
  ### Obs browser -----
  observeEvent(input$browser, {browser()})
  
  ### Obs timer -----
  observe({
    invalidateLater(1000, session)
    isolate({
      rv$timer     <- rv$timer - 1
      rv$stopwatch <- rv$stopwatch + 1
    })
    # if(rv$timer < 0 & section_nm() == "task" & rv$timer_active == TRUE){
    #   rv$timer_active <- FALSE
    #   loggit("INFO", "Timer elapsed.")
    # }
  })
  
  
  ##### Outputs -----
  output$timer_disp <- renderText({
    if (rv$timer < 1) {
      return("Time has expired, please enter your best guess and proceed.")
    } 
    
  })
  
  ### Dev msg -----
  output$dev_msg <- renderPrint({
    cat("dev msg -- \n",
        page_info(),
        task_header(),
        timer_info(),
        pftb(),
    )
  }) 
  output$test_next_pg_button <- reactive({input$next_pg_button})
  outputOptions(output, "test_next_pg_button", suspendWhenHidden = FALSE)
} ## End server function
  
  ### Combine as shiny app.
  shinyApp(ui = ui, server = server)
  
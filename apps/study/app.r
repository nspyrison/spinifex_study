source('global.r', local = TRUE)
## Also try: shiny::runApp(appDir = "apps/study", display.mode = "showcase")
#?shiny::runApp(appDir = "apps/study", display.mode = "showcase")


####### Server function, for shiny app
server <- function(input, output, session){
  output$TMP <- renderText(input$TMP)

  ##### Reactive value initialization -----
  rv            <- reactiveValues()
  rv$pg         <- 1L ## SET STARTING PAGE HERE <<<
  rv$sec_on_pg  <- 0L
  rv$ctrl_inter <- 0L
  rv$resp_inter <- 0L
  rv$ttr        <- 0L
  rv$resp_tbl   <- make_resp_tbl(participant_num) ## in resp_tbl.r
  
  ##### Reactive functions -----
  resp_row <- reactive(rv$resp_tbl[, rv$pg])
  output$resp_row <- renderTable(resp_row())
  plot_active <- reactive({req(resp_row)
    resp_row$plot_active})
  sim_nm <- reactive({req(resp_row) 
    resp_row$sim_nm})
  #output$sim_nm <- renderText(paste("sim_nm(): ", sim_nm()))
  factor <- reactive({req(resp_row)
    resp_row$factor})
  eval <- reactive({req(resp_row)
    resp_row$eval})
  image_fp <- reactive({
    if(plot_active()){
      #dir_sim_nm <- paste0("../images/", sim_nm())
      fct_nm <- factor()
      if(fct_nm == "pca")
        fct_suffix <- paste0("pca_x", x_axis_num(), "_y", y_axis_num(), ".png")
      if(fct_nm == "grand")
        fct_suffix <- paste0("grand.gif")
      if(fct_nm == "radial")
        fct_suffix <- paste0("radial_mv", manip_var(), ".gif")
        return(paste(paste0("./www/images/", sim_nm()), "", ## for extra "_"
                     fct_suffix, sep = "_"))
    }
    return("plot_active() == false")
  })
  output$image_fp <- renderText({image_fp()})
  output$image_plot <- renderImage({
    fp <- "./www/white_placeholder.png" ## Default to thin white strip
    ## Reactive height and width
    # w <- session$clientData$output_image_plot_width
    # h <- session$clientData$output_image_plot_height
    if(plot_active() == TRUE)
      fp <- image_fp()
    list(src = normalizePath(fp),
         # width  = w,
         # height = h,
         alt = "image text!")
  }, deleteFile = FALSE)
  
  ui_row <- reactive({
    data.frame(
      pg         = rv$pg,
      section_pg = section_pg(),
      section_nm = section_nm(),
      sim_nm     = sim_nm(),
      grand_path = tpath_nm()
    )
  })
  output$ui_row <- renderTable(ui_row())
  
  dat <- reactive({ ## Simulation df with attachments.
    req(plot_active())
    if(plot_active() == TRUE){
      req(sim_nm())
      return(get(sim_nm()))
    }
    return("NA")
  })
  cl <- reactive({ ## Simulation df with attachments.
    req(plot_active())
    if(plot_active() == TRUE){
      req(dat())
      return(attr(dat(), "cluster"))
    }
    return("NA")
  })
  p <- reactive({
    req(plot_active())
    if(plot_active() == TRUE){
      return(ncol(dat()))
    }
    return("NA")
  })
  x_axis_num <- reactive(as.integer(substr(input$x_axis, 3, 3)))
  y_axis_num <- reactive(as.integer(substr(input$y_axis, 3, 3)))
  manip_var <- reactive({
    mv <- which(colnames(dat()) == input$manip_var_nm)
    return(max(mv, 1))
  })
  header <- reactive({
    req(eval())
    if(eval() %in% 1:6)
      return(paste0("Evaluation -- factor: ", factor()))
    if(eval() == "training")
      return(paste0("Training -- factor: ", factor()))
    return("")
  })
  time_left <- reactive({
    time_alotted - rv$sec_on_pg
  })
  timer_info <- reactive({
    paste0("rv$sec_on_pg of time_alotted: ", rv$sec_on_pg, " of ", time_alotted)
  })
  
  #### _Task evaluation -----
  ### Task Response
  response <- reactive({
    if(substr(section_nm(), 1, 6) == "period"){
      resp <- input$response
      if(is.null(resp)) return("NA")
      ## Vector of the numbers without 'V'
      return(as.integer(gsub(' |V', '', input$response)))
    }
    return("NA")
  })
  output$response <- renderPrint(response())
  ### Task scoring
  diff <- reactive({
    if(plot_active()){
      signal <- attr(dat(), "var_mean_diff_ab")
      bar    <- sum(signal) / p()
      return(signal - bar)
    }
    return("NA")
  })
  output$diff <- renderPrint({
    if(plot_active() == TRUE)
      round(diff(), 2)
  })
  var_marks <- reactive({
    if(plot_active()){
      req(response())
      diff   <- diff()
      ans    <- which(diff >= 0L)
      weight <- sign(diff) * sqrt(abs(diff))
      response   <- response()
      if(response == "NA") return(0)
      return(weight[response])
    }
    return("NA")
  })
  output$var_marks <- renderPrint({
    if(plot_active() == TRUE)
    round(var_marks(), 2)
  })
  marks <- reactive({
    req(is.logical(plot_active()))
    req(var_marks())
    if(plot_active() == TRUE)
      return(sum(var_marks()))
    return("NA")
  })
  output$marks <- renderPrint({
    req(marks())
    if(plot_active() == TRUE)
      round(marks(), 2)
    return("NA")
  })
  
  ##### Observers ----
  observeEvent({
    dat()
  }, {
    choices <- paste0("V", 1:p())
    updateCheckboxGroupInput(session, "response",
                             choices = choices, selected = "", inline = TRUE)
  })
  ### _Obs update axis/task choices -----
  observeEvent({
    dat()
  }, {
    ## Initialize axis choices when data changes
    if(factor() == "pca"){
      choices <- paste0("PC", 1:PC_cap)
      updateRadioButtons(session, "x_axis", choices = choices, selected = "PC1", inline = TRUE)
      updateRadioButtons(session, "y_axis", choices = choices, selected = "PC2", inline = TRUE)
      loggit("INFO", "Task data changed while pca active; updated PC axes choices.")
    }
    if(factor() == "radial"){
      choices <- paste0("V", 1:p())
      updateRadioButtons(session, "manip_var_nm", choices = choices, selected = "PC1", inline = TRUE)
      loggit("INFO", "Task data changed while radial active; updated manip var choices.")
    }
    choices <- paste0("V", 1:p())
    updateCheckboxGroupInput(session, "response",
                             choices = choices, inline  = TRUE)
    loggit("INFO", "Task data changed; updated responce choices.")
  })
  ## Bump x_axis when set to the same as y_axis
  observeEvent(input$x_axis, {
    output$plot_msg <- renderText("")
    if(factor() == "pca" & input$x_axis == input$y_axis){
      x_axis_out <- NULL
      choices <- paste0("PC", 1:PC_cap)
      x_axis_num <- x_axis_num()
      if(x_axis_num <= 3){x_axis_out <- paste0("PC", x_axis_num + 1)
      }else{x_axis_out <- paste0("PC", x_axis_num - 1)}
      
      updateRadioButtons(session, "x_axis", choices = choices, selected = x_axis_out, inline = TRUE)
      loggit("INFO", paste0("x_axis set to ", input$x_axis,
                            ", same as y_axis; x_axis bumped to ", x_axis_out, "."),
             pfs()
      )
      
      output$plot_msg <- renderText(
        app_html_red(paste0("Please select different principal components.
                            X axis randomed selected to ", x_axis_out, "."))
      )
    }
  })
  ## Bump y_axis when set to the same as x_axis
  observeEvent(input$y_axis, {
    output$plot_msg <- renderText("")
    if(input$x_axis == input$y_axis){
      y_axis_out <- NULL
      choices <- paste0("PC", 1:PC_cap)
      y_axis_num <- y_axis_num()
      if(y_axis_num <= 3){y_axis_out <- paste0("PC", y_axis_num + 1)
      }else{y_axis_out <- paste0("PC", y_axis_num - 1)}
      
      updateRadioButtons(session, "y_axis", choices = choices, selected = y_axis_out, inline = TRUE)
      loggit("INFO", paste0("y_axis set to ", input$y_axis,
                            ", same as x_axis; y_axis bumped to ", y_axis_out, "."),
             pfs()
      )
      
      output$plot_msg <- renderText(
        app_html_red(paste0("Must select different principal components."))
      )
    }
  })
  
  ### _Obs radial update manip_var_nm choices -----
  observeEvent({
    dat()
  }, {
    ## Init manip_var_nm choices on data change.
    if(factor() == "radial"){
      these_colnames <- colnames(dat())
      updateRadioButtons(session, "manip_var_nm", choices = these_colnames,
                         selected = these_colnames[1], inline = TRUE)
      loggit("INFO", paste0("Task data or training factor changed; input$manip_var_nm choices updated."))
    }
  })
  
 
  
  ##### _Obs task responses -----
  ### task responses & ttr
  observeEvent(input$response, {
    if(rv$sec_on_pg > 1){
      rv$ttr[1] <- rv$sec_on_pg
      rv$response[1] <- paste(input$response, collapse = ", ")
      loggit("INFO", "Task response changed.",
             paste0("Response: ", rv$response[1],
                    ". ttr: ", rv$ttr[1], ".")
      )
    }
  })
  
  ### _Obs interaction count -----
  observeEvent({
      input$x_axis
      input$y_axis
    }, {
      rv$ctrl_inter <- rv$ctrl_inter + 1L
    }
  )
  observeEvent({
      input$manip_var_nm
    }, {
      rv$ctrl_inter <- rv$ctrl_inter + 1L
    }
  )
  observeEvent({
      input$response
    }, {
      rv$interactions <- rv$interactions + 1L
    }
  )
  
  ##### _Obs survey answers -----
  ### <REMOVED, see comments_medium.r>
  
  ### _Obs next page button -----
  observeEvent(input$next_pg_button, {
    if((rv$sec_on_pg > 1L & do_log == TRUE) | do_log == FALSE){
      cat(paste0("in loop, top --", rv$pg))
      response <- response()
      marks <- marks()
      
      ### __Training evaluation -----
      ### <REMOVED, see comments_medium.r>
      
      ##### __rv$resp_tbl -----
      ## Write responses and ttr to resp_tbl
      if(substr(section_nm(), 1L, 6L) == "period"){
      #TODO
      } ## End of writing to resp_tbl
      cat("!!! ITERATED PG!!!")
      
      
      ### __New page ----
      ## Advance to the next page, reset variables
      rv$pg <- rv$pg + 1L
      ## Reset responses, ttr, and timer for next task
      output$plot_msg     <- renderText("")
      rv$interactions     <- 0L
      rv$ttr              <- 0L
      rv$response         <- NULL
      rv$answer           <- NULL
      rv$marks            <- NULL
      if(rv$pg == survey_pg) shinyjs::hide("next_pg_button")
      
      ## Set structure for writing to resp_tbl
      ## cluster seperation task:
      ##TODO: response table writing.
      n_rows <- 1L
      if(section_nm() == "survey"){
        def <- c(rep("decline to answer (default)", 3L),
                 rep("5 (default)", n_survey_questions - 3L))
      }
      rv$response <- "none (default)"
      rv$ttr      <- 0
      loggit("INFO", paste0("Next page:"), pfs())
      cat("bottom of loop -- ")
    }
    cat("after loop \n")
  })
  
  
  ### _Obs save responses button -----
  observeEvent(input$save_resp, {
    filebase = paste("responses", this_group, Sys.info()[4L], sep = "_")
    prefix = ""
    
    # ## Write survey responses to rv$resp_tbl
    # ins_row_start <- nrow(rv$resp_tbl) - n_survey_questions + 1L
    # ins_row_end   <- nrow(rv$resp_tbl)
    # rv$resp_tbl$response[ins_row_start:ins_row_end] <- rv$response
    # rv$resp_tbl$ttr[ins_row_start:ins_row_end] <- rv$ttr
    
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
    # assign(save_name, rv$resp_tbl)
    write.csv(get(save_name), file = save_file, row.names = FALSE)
    rv$save_file <- save_file
    
    save_msg <- paste0("Reponses saved as ", save_file, " (log file: ", log_file, "). Thank you for participating!")
    output$save_msg <- renderText(app_html_red(save_msg))
    
    if(prefix == ""){
      loggit("INFO", "Save button pressed.",
             paste0("rv$save_file: ", rv$save_file,
                    ". save_msg: ", save_msg,
                    "."))
    }
    if(prefix != ""){
      loggit("INFO", paste0("NOTE: Prefixed save script run. Prefix was '", prefix, "'."),
             paste0("Save may not have been user initiated",
                    ". PREFIX USED: ", prefix,
                    ". rv$save_file: ", rv$save_file,
                    ". save_msg: ", save_msg, "."))
    }
  })
  
  ### _Obs browser -----
  observeEvent(input$browser, browser())
  
  ### _Obs timer -----
  observe({
    invalidateLater(1000L, session) ## Every 1000 ms; ie. every second
    isolate({
      rv$sec_on_pg <- rv$sec_on_pg + 1L
    })
  })
  
  ##### Outputs -----
  output$timer_disp <- renderText({
    if(section_nm() == "task"){ ## Disp timer counting down if on a task.
      if(time_left() < 1){return("Time has expired, please enter your best guess and proceed.")
      }else{
        return(
          paste0("Time remaining: ", lubridate::seconds_to_period(time_left()),
                 " of ", lubridate::seconds_to_period(time_alotted))
        )
      }
    }
    if(eval() == "training"){ ## Disp timer Counting up if in training.
      return(paste0("Time elapsed this page: ", lubridate::seconds_to_period(rv$sec_on_pg)))
    }
  })
  
  ### Condition handling for ui coditionalPanels
  output$is_saved    <- reactive(if(is.null(rv$save_file)){0L}else{1L}) ## Control save_msg.
  output$pg          <- reactive(rv$pg)        ## For hiding ui next_task button
  output$section_nm  <- reactive(section_nm()) ## For ui between sections
  output$factor   <- reactive(factor())  ## For sidebar inputs
  output$section_pg  <- reactive(section_pg()) ## For navigating training
  output$plot_active <- reactive(plot_active()) ## For display of the task response.
  output$eval        <- reactive(eval())       ## For sidebar display
  output$dev_tools   <- reactive({             ## For JS eval of R boolean...
    return(do_disp_dev_tools)
  }) 
  #input$response
  
  outputOptions(output, "pg",          suspendWhenHidden = FALSE) ## Eager evaluation for ui conditionalPanel
  outputOptions(output, "section_pg",  suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "section_nm",  suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "factor",   suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "plot_active", suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "eval",        suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "dev_tools",   suspendWhenHidden = FALSE) ## Eager evaluation for ui conditionalPanel

  ### General task outputs
  ## height: ggplot applies on renderPlot(), plotly applies to a plotly option.
  output$header <- renderText(header())
  
  ### dev_tools display -----
  output$dev_msg  <- renderPrint({
    if(do_disp_dev_tools == TRUE){
      cat("dev msg -- \n",
          page_info(),
          header(),
          timer_info(),
          pfs()
      )
    }
  })
} ## End server function

### Combine as shiny app.
shinyApp(ui = ui, server = server)

source('global.r', local = TRUE)
source('resp_tbl.r', local = TRUE) ## Needs initialization from global.r.
resp_tbl <- make_resp_tbl(participant_num)

## Also try: shiny::runApp(appDir = "apps/study", display.mode = "showcase")
#?shiny::runApp(appDir = "apps/study", display.mode = "showcase")

####### Server function, for shiny app
server <- function(input, output, session){

  ## Google sheets authentication
  tryCatch({
    googledrive::drive_auth(email = "nicholas.spyrison@monash.edu")
  }, error = function(e){
    txt <- "App could not authenticate to Google sheet. Please try again in 5 minutes. Closing app in 15 seconds."
    showNotification(txt, type = "error")
    warning(txt)
    Sys.sleep(15)
    stopApp()
    return(NULL)
  })
  
  ## onStop() This code will be run after the client has disconnected
  session$onSessionEnded(function() {
    cat(context_msg)
    message("ran session$onSessionEnded(f()).")
    ##TODO: add save on close here.
  })

  ##### Reactive value initialization -----
  rv             <- reactiveValues()
  rv$pg          <- 4L ## SET STARTING PAGE HERE <<<
  rv$sec_on_pg   <- 0L
  rv$resp_tbl    <- make_resp_tbl(participant_num) ## in resp_tbl.r
  ## Below are not needed, but to be explicit,
  rv$input_inter <- 0L
  rv$resp_inter  <- 0L
  rv$ttr         <- 0L
  
  ##### Reactive functions -----
  resp_tbl <- reactive(rv$resp_tbl)
  output$resp_tbl <- renderTable(resp_tbl())
  resp_row <- reactive(rv$resp_tbl[rv$pg, ])
  output$resp_row <- renderTable(resp_row())
  key <- reactive({req(resp_row)
    resp_row()$key
  })
  plot_active <- reactive({req(resp_row)
    resp_row()$plot_active})
  factor <- reactive({req(resp_row)
    resp_row()$factor})
  eval <- reactive({req(resp_row)
    resp_row()$eval})
  sim_nm <- reactive({req(resp_row)
    resp_row()$sim_nm})
  section_nm <- reactive({req(resp_row)
    resp_row()$section_nm})
  
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
        return(paste(paste0("./www/images/", sim_nm()), "", ## for extra "_" sep
                     fct_suffix, sep = "_"))
    }
    return("plot_active() == false")
  })
  output$image_fp <- renderText({image_fp()})
  output$image_plot <- renderImage({
    fp <- "./www/white_placeholder.png" ## Default thin white strip .png
    if(plot_active() == TRUE)
      fp <- image_fp()
    list(src = normalizePath(fp),
         alt = "image text!")
  }, deleteFile = FALSE)
  
  dat <- reactive({ ## Simulation data (in df) with attributes
    req(plot_active())
    if(plot_active() == TRUE){
      req(sim_nm())
      return(get(sim_nm()))
    }
    return("NA")
  })
  cl <- reactive({ ## Vector containing the class.
    req(plot_active())
    if(plot_active() == TRUE){
      req(dat())
      return(attr(dat(), "cluster"))
    }
    return("NA")
  })
  p <- reactive({ ## Scalar number of variables
    req(plot_active())
    if(plot_active() == TRUE){
      return(ncol(dat()))
    }
    return("NA")
  })
  x_axis_num <- reactive(as.integer(substr(input$x_axis, 3, 3)))
  y_axis_num <- reactive(as.integer(substr(input$y_axis, 3, 3)))
  manip_var  <- reactive({
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
  var_resp <- reactive({
    if(substr(section_nm(), 1, 6) == "period"){
      resp <- input$var_resp
      if(is.null(resp)) return(NA)
      ## Vector of the numbers without 'V'
      return(as.integer(gsub(' |V', '', input$var_resp)))
    }
    return(NA)
  })
  output$var_resp <- renderPrint(var_resp())
  ### Task scoring
  var_diff <- reactive({
    if(plot_active()){
      var_signal <- attr(dat(), "var_mean_diff_ab")
      bar <- sum(var_signal) / p()
      return(var_signal - bar)
    }
    return("NA")
  })
  output$var_diff <- renderPrint({
    if(plot_active() == TRUE)
      round(var_diff(), 2)
  })
  var_marks <- reactive({
    if(plot_active()){
      req(var_resp())
      var_diff <- var_diff()
      ans <- which(var_diff >= 0L)
      weight <- sign(var_diff) * sqrt(abs(var_diff))
      var_resp <- var_resp()
      if(is.na(var_resp[1])) return(0)
      return(weight[var_resp])
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
  
  ##### Observers -----
  observeEvent({
    dat()
  }, {
    choices <- paste0("V", 1:p())
    updateCheckboxGroupInput(session, "var_resp",
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
    }
    if(factor() == "radial"){
      choices <- paste0("V", 1:p())
      updateRadioButtons(session, "manip_var_nm", choices = choices, selected = "PC1", inline = TRUE)
    }
    choices <- paste0("V", 1:p())
    updateCheckboxGroupInput(session, "var_resp",
                             choices = choices, inline  = TRUE)
  })
  ## When x_axis set, disable corresponding y_axis opt.
  observeEvent(input$x_axis, {
    disable(selector = paste0("#y_axis button:eq(", 
                              as.integer(substr(input$x_axis, 3, 3)),
                              ")")
    )
  })
  ## When y_axis set, disable corresponding x_axis opt.
  observeEvent(input$y_axis, {
    disable(selector = paste0("#x_axis button:eq(", 
                              as.integer(substr(input$y_axis, 3, 3)),
                              ")")
    )
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
    }
  })
  
  
  ##### _Obs responses and counts -----
  ### task responses & ttr
  observeEvent(input$var_resp, {
    if(rv$sec_on_pg > 1){
      rv$ttr[1] <- rv$sec_on_pg
      rv$var_resp[1] <- paste(input$var_resp, collapse = ", ")
    }
  })
  
  ### _Obs interaction count
  observeEvent({
      input$x_axis
      input$y_axis
      input$manip_var_nm
    }, {
      rv$input_inter <- rv$input_inter + 1L
    }
  )
  
  observeEvent({
      input$var_resp
    }, {
      rv$resp_inter <- rv$resp_inter + 1L
    }
  )
  
  ### _Obs next page button -----
  observeEvent(input$next_pg_button, {
    if((rv$sec_on_pg > 1L & do_disp_dev_tools == TRUE) | do_disp_dev_tools == FALSE){
      
      ##### __rv$resp_tbl -----
      ## Write responses and ttr to resp_tbl
      if(is.na(factor()) == FALSE){
        this_row <- resp_row()
        this_row$input_inter <- rv$input_inter
        this_row$resp_inter  <- rv$resp_inter
        this_row$ttr         <- rv$ttr
        this_row$var_resp    <- list(var_resp())
        this_row$var_marks   <- list(var_marks())
        this_row$marks       <- marks()
        
        ## Save local and remote line
        rv$resp_tbl[rv$pg, ] <- this_row
        googlesheets4::sheet_append(ss_id, this_row)
        message("Data row apended for page: ", rv$pg, " -- ", substr(Sys.time(), 12, 16))
        browser()
      } ## End of writing to resp_tbl

      
      ### __New page ----
      ## Advance to the next page, reset other rv variables
      rv$pg           <- rv$pg + 1L
      output$plot_msg <- renderText("")
      rv$input_inter  <- 0L
      rv$resp_inter   <- 0L
      rv$ttr          <- 0L
      if(rv$pg == survey_pg) shinyjs::hide("next_pg_button")
    }
  })
  

  ### _Obs save responses button -----
  observeEvent(input$save_resp, {
    ## THIS IS THE FINAL SAVE BUTTON NOT THE NEXT PAGE BUTTON.
    
    ## Do the actual saving
    ## TODO: the saving, see googlesheets4::sheet_append( ss_id)
    
    ## Message back
    save_msg <- paste0("Reponses saved. Thank you for participating!")
    showNotification(save_msg, type = "message", duration = 10)
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
    if(eval() == "training"){ ## Disp timer counting up if in training.
      return(paste0("Time elapsed this page: ", lubridate::seconds_to_period(rv$sec_on_pg)))
    }
  })
  
  ### Condition handling for ui coditionalPanels
  output$is_saved    <- reactive(if(is.null(rv$save_file)){0L}else{1L}) ## Control save_msg.
  output$pg          <- reactive(rv$pg)         ## For hiding ui next_task button
  output$factor      <- reactive(factor())      ## For sidebar inputs
  output$plot_active <- reactive(plot_active()) ## For display of the task response.
  output$eval        <- reactive(eval())        ## For sidebar display
  output$dev_tools   <- reactive({              ## For JS eval of R boolean...
    return(do_disp_dev_tools)
  }) 
  
  outputOptions(output, "pg",          suspendWhenHidden = FALSE) ## Eager evaluation for ui conditionalPanel
  outputOptions(output, "factor",      suspendWhenHidden = FALSE) ##  "
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
          ", header(): ", header(),
          ", timer_info(): ", timer_info(),
          ", key(): ", key()
      )
    }
  })
} ## End server function

### Combine as shiny app.
shinyApp(ui = ui, server = server)

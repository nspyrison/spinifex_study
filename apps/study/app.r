source('global.r', local = TRUE) 

## Also try: shiny::runApp(appDir = "apps/study", display.mode = "showcase")
#?shiny::runApp(appDir = "apps/study", display.mode = "showcase")

####### Server function, for shiny app
server <- function(input, output, session){
  ## Google sheets authentication
  ## for setup see "./.secrets/save_token.r
  tryCatch({
    ## For the first time running the app in R to get the OAuth token:
    #googlesheets4::gs4_auth(cache = ".secrets")
    ## Downstream runs use:
    googlesheets4::gs4_auth(
      cache = ".secrets", email = "nicholas.spyrison@monash.edu", use_oob = TRUE)
  }, error = function(e){
    txt <- "App could not authenticate to Google sheet. Please try again in 5 minutes. Closing app in 15 seconds."
    showNotification(txt, type = "error", duration = 15L)
    warning(txt)
    Sys.sleep(15L)
    stopApp()
    return(NULL)
  })
  
  ## onStop() This code will be run after the client has disconnected
  session$onSessionEnded(function() {
    cat(context_msg)
    message("Ran session$onSessionEnded(f()). Coerecing app off with stopApp().")
    stopApp()
    ### CANNOT USE REACTIVE OR EVEN READ rv$pg, within the onStop().
    ##### May be able to read from sheet and find last, also may not be worth it.
    ##### Will have to clean in analysis.
  })
  
  ##### Reactive value initialization -----
  rv             <- reactiveValues()
  rv$pg          <- 1L ## SET STARTING PAGE HERE <<<
  rv$sec_on_pg   <- 0L
  ## Below are not needed, but to be explicit,
  rv$input_inter <- 0L
  rv$resp_inter  <- 0L
  rv$sec_to_resp <- 0L
  rv$resp_tbl    <- init_resp_tbl
  rv$survey_tbl  <- init_survey_tbl
  
  ##### Reactive functions -----
  resp_tbl        <- reactive(rv$resp_tbl)
  output$resp_tbl <- renderTable(resp_tbl())
  resp_row        <- reactive(rv$resp_tbl[rv$pg, ])
  output$resp_row <- renderTable(resp_row())
  survey_tbl        <- reactive(rv$survey_tbl)
  output$survey_tbl <- renderTable(survey_tbl())
  save_survey       <- reactive(input$save_survey)
  output$save_survey <- renderText(save_survey())
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
  section_pg <- reactive({req(resp_row)
    resp_row()$section_pg})
  
  image_fp <- reactive({
    if(plot_active() & time_left() > 0){
      fct_nm <- factor()
      if(fct_nm == "pca")
        fct_suffix <- paste0("pca_x", x_axis_num(), "y", y_axis_num(), ".png")
      if(fct_nm == "grand")
        fct_suffix <- paste0("grand.gif")
      if(fct_nm == "radial")
        fct_suffix <- paste0("radial_mv", manip_var(), ".gif")
        return(paste(paste0("./www/images/", sim_nm()), "", ## for extra "_" sep
                     fct_suffix, sep = "_"))
    }
    return("./www/white_placeholder.png") ## Thin white strip .png as a silent placeholder
  })
  output$image_fp <- renderText({image_fp()})
  output$image_plot <- renderImage({
    list(src = normalizePath(image_fp()),
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
  p <- reactive({ ## Scalar number of variables
    req(plot_active())
    if(plot_active() == TRUE){
      return(as.integer(substr(resp_row()$p_dim, 2L, 2L)))
    }
    return("NA")
  })
  x_axis_num <- reactive(as.integer(substr(input$x_axis, 3L, 3L)))
  y_axis_num <- reactive(as.integer(substr(input$y_axis, 3L, 3L)))
  manip_var  <- reactive({
    mv <- which(colnames(dat()) == input$manip_var_nm)
    return(max(mv, 1L))
  })
  header <- reactive({
    req(eval())
    if(eval() %in% 1L:6L)
      return(paste0("Evaluation -- ", factor()))
    if(substr(eval(), 1L, 1L) == "t")
      return(paste0("Training -- ", factor()))
    return("")
  })
  output$header <- renderText(header())
  time_left <- reactive({
    time_alotted - rv$sec_on_pg
  })
  timer_info <- reactive({
    paste0("rv$sec_on_pg of time_alotted: ", rv$sec_on_pg, " of ", time_alotted)
  })
  
  #### _Task evaluation -----
  ### Task Response
  var_resp <- reactive({
    if(substr(section_nm(), 1L, 6L) == "period"){
      resp <- input$var_resp
      if(is.null(resp)) return(NA)
      ## Vector of the numbers without 'V'
      return(as.integer(gsub(' |V', '', input$var_resp)))
    }
    return(NA)
  })
  output$var_resp <- renderPrint(var_resp())
  v1_resp <- reactive({
    req(var_resp())
    return(1L %in% var_resp())
  })
  v2_resp <- reactive({
    req(var_resp())
    return(2L %in% var_resp())
  })
  v3_resp <- reactive({
    req(var_resp())
    return(3L %in% var_resp())
  })
  v4_resp <- reactive({
    req(var_resp())
    return(4L %in% var_resp())
  })
  v5_resp <- reactive({
    req(var_resp())
    return(5L %in% var_resp())
  })
  v6_resp <- reactive({
    req(var_resp())
    return(6L %in% var_resp())
  })
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
      round(var_diff(), 2L)
  })
  var_weight <- reactive({
    if(plot_active()){
      req(var_resp())
      var_diff <- var_diff()
      ans <- which(var_diff >= 0L)
      var_weight <- sign(var_diff) * sqrt(abs(var_diff))
      return(var_weight)
    }
    return("NA")
  })
  var_marks <- reactive({
    if(plot_active()){
      var_weight <- var_weight()
      var_resp <- var_resp()
      if(is.na(var_resp[1L])) return(0L)
      return(var_weight[var_resp])
    }
    return("NA")
  })
  output$var_marks <- renderPrint({
    if(plot_active() == TRUE)
      round(var_marks(), 2L)
  })
  v1_marks <- reactive({
    req(var_marks())
    return(v1_resp() * var_weight()[1L])
  })
  v2_marks <- reactive({
    req(var_marks())
    return(v2_resp() * var_weight()[2L])
  })
  v3_marks <- reactive({
    req(var_marks())
    return(v3_resp() * var_weight()[3L])
  })
  v4_marks <- reactive({
    req(var_marks())
    return(v4_resp() * var_weight()[4L])
  })
  v5_marks <- reactive({
    req(var_marks())
    return(v5_resp() * var_weight()[5L])
  })
  v6_marks <- reactive({
    req(var_marks())
    return(v6_resp() * var_weight()[6L])
  })
  task_marks <- reactive({
    req(is.logical(plot_active()))
    req(var_marks())
    if(plot_active() == TRUE)
      return(sum(var_marks()))
    return("NA")
  })
  output$task_marks <- renderPrint({
    req(task_marks())
    if(plot_active() == TRUE)
      round(task_marks(), 2L)
    return("NA")
  })
  
  ##### Observers -----
  observeEvent({
    dat()
  }, {
    choices <- paste0("V", 1L:p())
    updateCheckboxGroupInput(session, "var_resp",
                             choices = choices, selected = "", inline = TRUE)
  })
  
  ### _Obs update axis/task choices -----
  observeEvent({
    dat()
  }, {
    ## Initialize axis choices when data changes
    if(factor() == "pca"){
      updateRadioButtons(session, "x_axis", choices = pca_choices,
                         selected = "PC1", inline = TRUE)
      updateRadioButtons(session, "y_axis", choices = pca_choices,
                         selected = "PC2", inline = TRUE)
    }
    choices <- paste0("V", 1L:p())
    if(factor() == "radial")
      updateRadioButtons(session, "manip_var_nm", choices = choices,
                         selected = "PC1", inline = TRUE)
    updateCheckboxGroupInput(session, "var_resp", choices = choices,
                             inline = TRUE)
  })
  ## When x_axis changes, bump y_axis if needed
  observeEvent(input$x_axis, {
    if(input$x_axis == input$y_axis){
      selec <- pca_choices[!(pca_choices %in% input$x_axis)][1] ## First option not eq to x_axis
      updateRadioButtons(session, "y_axis", choices = pca_choices,
                         selected = selec, inline = TRUE)
      showNotification("Do not select the same PC axes.", type = "message")
        }
  })
  ## When y_axis changes, bump x_axis if needed
  observeEvent(input$y_axis, {
    if(input$y_axis == input$x_axis){
      selec <- pca_choices[!(pca_choices %in% input$y_axis)][1] ## First option not eq to y_axis
      updateRadioButtons(session, "x_axis", choices = pca_choices,
                         selected = selec, inline = TRUE)
      showNotification("Do not select the same PC axes.", type = "message")
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
                         selected = these_colnames[1L], inline = TRUE)
    }
  })
  
  ##### _Obs responses and counts -----
  ### task responses & sec_to_resp
  observeEvent(input$var_resp, {
    if(rv$sec_on_pg > 1L){
      rv$sec_to_resp[1L] <- rv$sec_on_pg
      rv$var_resp[1L] <- paste(input$var_resp, collapse = ", ")
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
  output_row <- reactive({
    this_row <- resp_row()
    this_row$input_inter  <- rv$input_inter
    this_row$resp_inter   <- rv$resp_inter
    this_row$sec_to_resp  <- rv$sec_to_resp
    this_row$sec_on_pg    <- rv$sec_on_pg
    this_row$write_dt     <- as.character(Sys.time())
    if(plot_active()){
      this_row$task_marks <- task_marks()
      this_row$v1_resp    <- v1_resp()
      this_row$v2_resp    <- v2_resp()
      this_row$v3_resp    <- v3_resp()
      this_row$v4_resp    <- v4_resp()
      this_row$v5_resp    <- v5_resp()
      this_row$v6_resp    <- v6_resp()
      this_row$v1_marks   <- v1_marks()
      this_row$v2_marks   <- v2_marks()
      this_row$v3_marks   <- v3_marks()
      this_row$v4_marks   <- v4_marks()
      this_row$v5_marks   <- v5_marks()
      this_row$v6_marks   <- v6_marks()
    }
    return(this_row)
  })
  
  ### Captures responses and times to the survey:
  observeEvent(input$survey1, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[1L]    <- input$survey1
      rv$survey_tbl$sec_to_resp[1L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey2, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[2L]    <- input$survey2
      rv$survey_tbl$sec_to_resp[2L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey3, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[3L]    <- input$survey3
      rv$survey_tbl$sec_to_resp[3L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey4, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[4L]    <- input$survey4
      rv$survey_tbl$sec_to_resp[4L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey5, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[5L]    <- input$survey5
      rv$survey_tbl$sec_to_resp[5L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey6, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[6L]    <- input$survey6
      rv$survey_tbl$sec_to_resp[6L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey7, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[7L]    <- input$survey7
      rv$survey_tbl$sec_to_resp[7L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey8, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[8L]    <- input$survey8
      rv$survey_tbl$sec_to_resp[8L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey9, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[9L]    <- input$survey9
      rv$survey_tbl$sec_to_resp[9L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey10, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[10L]    <- input$survey10
      rv$survey_tbl$sec_to_resp[10L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey11, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[11L]    <- input$survey11
      rv$survey_tbl$sec_to_resp[11L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey12, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[12L]    <- input$survey12
      rv$survey_tbl$sec_to_resp[12L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey13, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[13L]    <- input$survey13
      rv$survey_tbl$sec_to_resp[13L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey14, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[14L]    <- input$survey14
      rv$survey_tbl$sec_to_resp[14L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey15, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[15L]    <- input$survey15
      rv$survey_tbl$sec_to_resp[15L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey16, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[16L]    <- input$survey16
      rv$survey_tbl$sec_to_resp[16L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey17, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[17L]    <- input$survey17
      rv$survey_tbl$sec_to_resp[17L] <- rv$sec_on_pg
    }
  })
  observeEvent(input$survey18, {
    if(rv$sec_on_pg > 0L){
      rv$survey_tbl$response[18L]    <- input$survey18
      rv$survey_tbl$sec_to_resp[18L] <- rv$sec_on_pg
    }
  })
  
  ### _Obs next page button -----
  observeEvent(input$next_pg_button, {
    if((rv$sec_on_pg > 1L & do_disp_dev_tools == FALSE) |
       do_disp_dev_tools == TRUE){
      ##### __ eval training
      ## Quality proofing; super low ball?
      # if(eval() == "t1" &
      #    task_marks() <= 0L){ ## If FIRST training and marks less than/eq to 0, fail early.
      #   txt <- paste0("You did not meet the required threshold for the training data set.
      #   Please enter the following code to <blah, blah>. On rv$pg ", rv$pg, ".")
      #   showNotification(txt, type = "error", duration = 30L)
      #   warning(txt)
      #   # browser()
      #   # Sys.sleep(30L)
      #   # stopApp()
      #   # return(NULL)
      # }
      if(rv$pg == 1){
        rv$resp_tbl$prolific_id   <- input$prolific_id
        rv$survey_tbl$prolific_id <- input$prolific_id
      }
      ##### __rv$resp_tbl -----
      ## Write responses and sec_to_resp to resp_tbl
      this_row <- output_row()
      ## Update local table and write to Google sheet.
      rv$resp_tbl[rv$pg, ] <- this_row
      googlesheets4::sheet_append(ss_id, this_row, 1L)
      message("rv$resp_tbl, data row writen locally and to gsheets. pg:", rv$pg, " -- ", Sys.time())
      ## End of writing to resp_tbl
      
      ### __New page ----
      ## Advance to the next page, reset other rv variables
      rv$pg           <- rv$pg + 1L
      output$plot_msg <- renderText("")
      rv$input_inter  <- 0L
      rv$resp_inter   <- 0L
      rv$sec_on_pg    <- 0L
    }
  })
  
  ### _Obs save_survey button -----
  ## resp_tbl writes every line with the next page, this is for SURVEY ONLY.
  observeEvent(input$save_survey, {
    ## Write response tbl to sheet 1
    googlesheets4::sheet_append(ss_id, rv$resp_tbl, 1L)
    ## Write survey tbl to sheet 2
    googlesheets4::sheet_append(ss_id, rv$survey_tbl, 2L)
    message("survey_tbl, all questions apended to gsheet -- ",
            substr(Sys.time(), 12L, 16L))
    ## Save message
    save_msg <- paste0("Reponses saved. Thank you for participating!")
    showNotification(save_msg, type = "message", duration = 10L)
    output$save_msg <- renderText(save_msg)
  })
  
  ### _Obs browser -----
  observeEvent(input$browser, browser())
  ### _Obs timer -----
  observe({
    invalidateLater(1000L, session) ## Every 1000 ms, increment a second
    isolate({
      rv$sec_on_pg <- rv$sec_on_pg + 1L
    })
  })
  
  ##### Outputs -----
  output$timer_disp <- renderText({
    if(eval() %in% as.character(1L:6L)){ ## Timer display counting down if on a task.
      if(time_left() < 1L){
        return("Time has expired, please enter your best guess and proceed.")
      }else{
        return(
          paste0("Seconds remaining: ", time_left(),
                 " of ", time_alotted, " seconds total.")
        )
      }
    }
    if(section_nm() %in% paste0("t", 1L:3L)){ ## Disp timer counting up if in training.
      return(paste0("Time on this page: ", lubridate::seconds_to_period(rv$sec_on_pg)))
    }
  })
  
  output$pg_next_pg <-
    renderPrint(paste0("rv$pg: ", rv$pg,
                      ". next_pg_button: ", input$next_pg_button, "."))
  ### Condition handling for ui coditionalPanels
  output$pg          <- reactive(rv$pg)         ## Hiding ui next_task button
  output$factor      <- reactive(factor())      ## Sidebar inputs
  output$section_nm  <- reactive(section_nm())  ## Text
  output$section_pg  <- reactive(section_pg())  ## Training
  output$plot_active <- reactive(plot_active()) ## Task response
  output$eval        <- reactive(eval())        ## Sidebar
  output$is_intermission <- reactive({          ## Intermission
    req(eval())
    if(substr(eval(), 1L, 12L) == "intermission") return(TRUE)
    return(FALSE)
  })
  output$is_saved <- reactive({              ## Save button was pressed?
    if(input$save_survey == 1L) return(TRUE)
    return(FALSE)
  })
  output$do_disp_prolific_code <- reactive({ ## Prolific pay code, do dislplay?
    if(input$prolific_id == "")
      return(FALSE)
    return(TRUE)
  })
  output$do_disp_dev_tools <- reactive({     ## JS eval of R boolean...
    return(do_disp_dev_tools)
  }) 
  
  ## Eager evaluation for correct ui conditionalPanel functionality
  outputOptions(output, "pg",                    suspendWhenHidden = FALSE)
  outputOptions(output, "factor",                suspendWhenHidden = FALSE)
  outputOptions(output, "section_nm",            suspendWhenHidden = FALSE)
  outputOptions(output, "section_pg",            suspendWhenHidden = FALSE)
  outputOptions(output, "plot_active",           suspendWhenHidden = FALSE)
  outputOptions(output, "eval",                  suspendWhenHidden = FALSE)
  outputOptions(output, "do_disp_prolific_code", suspendWhenHidden = FALSE)
  outputOptions(output, "do_disp_dev_tools",     suspendWhenHidden = FALSE)
  outputOptions(output, "is_intermission",       suspendWhenHidden = FALSE)
  outputOptions(output, "is_saved",              suspendWhenHidden = FALSE)
  
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

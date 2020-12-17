source('global.r', local = TRUE)
source('resp_tbl.r', local = TRUE) ## Needs initialization from global.r.

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
    showNotification(txt, type = "error", duration = 15)
    warning(txt)
    Sys.sleep(15)
    stopApp()
    return(NULL)
  })
  
  ## onStop() This code will be run after the client has disconnected
  session$onSessionEnded(function() {
    cat(context_msg)
    message("ran session$onSessionEnded(f()).")
    
    reactive({
      if(input$save_survey < 1){
        ### Attempt to save, when closed early
        this_row <- output_row()
        
        extra_row <- tibble::tibble(
          key = "!!App ended without save button!!",
          participant_num = as.integer(participant_num),
          full_perm_num   = as.integer(full_perm_num),
          pg              = NA_real_,
          section_pg      = NA_real_,
          section_nm      = paste0("rv$pg on close: ", rv$pg, ", ", 
                                   round(100L * rv$pg / survey_pg, 2L), "% of the pages."),
          period          = NA_real_,
          plot_active     = NA_real_,
          eval            = paste0("!Participant num ", participant_num, " dind't save app!"),
          factor          = paste0("!Participant num ", participant_num, " dind't save app!"),
          vc              = paste0("!Participant num ", participant_num, " dind't save app!"),
          p_dim           = paste0("!Participant num ", participant_num, " dind't save app!"),
          location        = paste0("!Participant num ", participant_num, " dind't save app!"),
          sim_nm          = paste0("!Participant num ", participant_num, " dind't save app!"),
          input_inter     = NA_integer_,
          resp_inter      = NA_integer_,
          ttr             = NA_integer_,
          marks           = NA_real_,
          write_dt        = Sys.time(),
          v1_resp    = NA_integer_,
          v2_resp    = NA_integer_,
          v3_resp    = NA_integer_,
          v4_resp    = NA_integer_,
          v5_resp    = NA_integer_,
          v6_resp    = NA_integer_,
          v1_marks   = NA_real_,
          v2_marks   = NA_real_,
          v3_marks   = NA_real_,
          v4_marks   = NA_real_,
          v5_marks   = NA_real_,
          v6_marks   = NA_real_,
        )
        these_rows <- cbind(this_row, extra_row)
        
        ## Save local and remote line
        rv$resp_tbl[c(rv$pg, rv$pg + 1L), ] <- these_rows
        googlesheets4::sheet_append(ss_id, these_rows)
        message("onStop(): Data rows apended for page: ", rv$pg, " -- ", substr(Sys.time(), 12L, 16L))
        
      } ## End of writing to resp_tbl
      ## Else do nothing
    })
  })
  
  ##### Reactive value initialization -----
  rv             <- reactiveValues()
  rv$resp_tbl    <- make_resp_tbl(participant_num) ## func from ./resp_tbl.r
  rv$pg          <- 1L ## SET STARTING PAGE HERE <<<
  rv$sec_on_pg   <- 0L
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
  section_pg <- reactive({req(resp_row)
    resp_row()$section_pg})
  
  image_fp <- reactive({
    if(plot_active()){
      #dir_sim_nm <- paste0("../images/", sim_nm())
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
      if(is.na(var_resp[1L])) return(0)
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
      round(marks(), 2L)
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
      choices <- paste0("PC", 1:PC_cap)
      updateRadioButtons(session, "x_axis", choices = choices, selected = "PC1", inline = TRUE)
      updateRadioButtons(session, "y_axis", choices = choices, selected = "PC2", inline = TRUE)
    }
    if(factor() == "radial"){
      choices <- paste0("V", 1L:p())
      updateRadioButtons(session, "manip_var_nm", choices = choices, selected = "PC1", inline = TRUE)
    }
    choices <- paste0("V", 1L:p())
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
                         selected = these_colnames[1L], inline = TRUE)
    }
  })
  
  
  ##### _Obs responses and counts -----
  ### task responses & ttr
  observeEvent(input$var_resp, {
    if(rv$sec_on_pg > 1L){
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
  output_row <- reactive({
    if(is.na(factor()) == FALSE){
      this_row <- resp_row()
      this_row$prolific_id <- input$prolific_id
      this_row$input_inter <- rv$input_inter
      this_row$resp_inter  <- rv$resp_inter
      this_row$ttr         <- rv$ttr
      this_row$marks       <- marks()
      this_row$write_dt    <- as.character(Sys.time())
      this_row$v1_resp     <- v1_resp()
      this_row$v2_resp     <- v2_resp()
      this_row$v3_resp     <- v3_resp()
      this_row$v4_resp     <- v4_resp()
      this_row$v5_resp     <- v5_resp()
      this_row$v6_resp     <- v6_resp()
      this_row$v1_marks    <- v1_marks()
      this_row$v2_marks    <- v2_marks()
      this_row$v3_marks    <- v3_marks()
      this_row$v4_marks    <- v4_marks()
      this_row$v5_marks    <- v5_marks()
      this_row$v6_marks    <- v6_marks()
      return(this_row)
    }
  })
  
  ### _Obs next page button -----
  observeEvent(input$next_pg_button, {
    if((rv$sec_on_pg > 1L & do_disp_dev_tools == TRUE) | do_disp_dev_tools == FALSE){
      ##### __ eval training
      if(substr(eval(), 1L, 2L) == "t1" &
         marks() <= 0L){ ## If first training and marks less than/eq to 0, fail early.
        
        txt <- paste0("You did not meet the required thershold for the training data set. 
        Please enter the following code to <blah, blah>. On rv$pg ", rv$pg, ".")
        showNotification(txt, type = "error", duration = 30L)
        warning(txt)
        #browser()
        # Sys.sleep(30)
        # stopApp()
        # return(NULL)
      }
      
      ##### __rv$resp_tbl -----
      ## Write responses and ttr to resp_tbl
      if(is.na(factor()) == FALSE){
        this_row <- output_row()
        ## Update local table and write to google sheet.
        rv$resp_tbl[rv$pg, ] <- this_row
        googlesheets4::sheet_append(ss_id, this_row)
        message("Data row apended for page: ", rv$pg, " -- ", 
                substr(Sys.time(), 12L, 16L))
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
  observeEvent(input$save_survey, {
    ## resp_tbl writes every line with the next page, this is for SURVEY ONLY.
    
    ## Saves the survey info.
    ## TODO: the saving, see googlesheets4::sheet_append( ss_id)
    
    ## Message back
    save_msg <- paste0("Reponses saved. Thank you for participating!")
    showNotification(save_msg, type = "message", duration = 10)
    output$save_msg <- renderText(save_msg)
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
    if(section_nm() == "task"){ ## Timer display counting down if on a task.
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
  output$pg          <- reactive(rv$pg)         ## Hiding ui next_task button
  output$factor      <- reactive(factor())      ## Sidebar inputs
  output$section_nm  <- reactive(section_nm())  ## Controlling text display
  output$section_pg  <- reactive(section_pg())  ## Controlling training display
  output$plot_active <- reactive(plot_active()) ## Display of the task response.
  output$eval        <- reactive(eval())        ## Sidebar display
  output$p1_intermission <- reactive({
    req(period(), eval())
    if(period() == 1 & eval() == "intermission") return(TRUE)
    return(FALSE)
  })        ## Sidebar display
  output$p2_intermission <- reactive({
    req(period(), eval())
    if(period() == 2 & eval() == "intermission") return(TRUE)
    return(FALSE)
  })        ## Sidebar display
  output$is_saved    <- reactive({
    if(input$save_survey == 1L) return(TRUE)
    return(FALSE)
  }
  )
  output$do_disp_prolific_code <- reactive({    ## Display of prolific pay code
    if(is.na(input$prolific_id) == FALSE)
      return(TRUE)
    return(FALSE)
  })
  output$dev_tools   <- reactive({              ## For JS eval of R boolean...
    return(do_disp_dev_tools)
  }) 
  
  outputOptions(output, "pg",                    suspendWhenHidden = FALSE) ## Eager evaluation for ui conditionalPanel
  outputOptions(output, "factor",                suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "section_nm",            suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "section_pg",            suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "plot_active",           suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "eval",                  suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "do_disp_prolific_code", suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "dev_tools",             suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "p1_intermission",       suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "p2_intermission",       suspendWhenHidden = FALSE) ## Eager evaluation for ui conditionalPanel
  
  
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

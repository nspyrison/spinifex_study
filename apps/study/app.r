source('global.r', local = TRUE)
## Also try: shiny::runApp(appDir = "apps/study", display.mode = "showcase")
#?shiny::runApp(appDir = "apps/study", display.mode = "showcase")


####### Server function, for shiny app
server <- function(input, output, session){
  output$TMP <- renderText(input$TMP)

  ##### Reactive value initialization -----
  rv              <- reactiveValues()
  rv$pg           <- 4L  ## SET STARTING PAGE HERE <<<
  rv$sec_on_pg    <- 0L
  rv$interactions <- 0L
  rv$ttr          <- 0L
  rv$response     <- 0L
  # rv$resp_tbl         <- 
  #   data.frame(
  #     participant_num = participant_num,
  #     full_perm_num   = full_perm_num,
  #     period,
  #     eval
  #     factor          = rep(this_factor_nm_ord, length.out = 99),
  #     VC,
  #     p_dim,
  #     sim_nm,
  #     grand_path,
  #     interactions,
  #     ttr,
  #     response,
  #     answer,
  #     marks
  # )
  
  ##### Reactive functions -----
  section_pg <- reactive({ ## Current page num of this section.
    if(section_nm() == "intro"){return(rv$pg)}
    if(section_nm() == "period1")
      return(rv$pg - (min(period1_pgs) - 1L))
    if(section_nm() == "period2")
      return(rv$pg - (min(period2_pgs) - 1L))
    if(section_nm() == "period3")
      return(rv$pg - (min(period3_pgs) - 1L))
    if(section_nm() == "survey")
      return(rv$pg - (min(survey_pg)   - 1L))
  })
  section_nm <- reactive({ ## Text name of section
    req(rv$pg)
    if(rv$pg %in% intro_pgs){  return("intro")}
    if(rv$pg %in% period1_pgs){return("period1")}
    if(rv$pg %in% period2_pgs){return("period2")}
    if(rv$pg %in% period3_pgs){return("period3")}
    if(rv$pg %in% survey_pg){  return("survey")}
    return("!!SECTION NOT DEFINED!!")
  })

  period <- reactive({
    req(section_nm())
    if(substr(section_nm(), 1, 6) == "period")
      return(as.integer(substr(section_nm(), 7, 7)))
    return("NA")
  })
  eval <- reactive({
    req(section_pg())
    req(period())
    if(substr(section_nm(), 1, 6) == "period"){
      if(section_pg() == 1) return("training")
      if(section_pg() == 4) return("intermission")
      2 * (period() - 1) + (section_pg() - 1)
    }
    return("NA")
  }) 
  factor_nm <- reactive({ ## ~ for group 1: pca, grand, radial
    req(period(), section_nm(), eval())
    ## "intro" and "survey" NA
    if(substr(section_nm(), 1, 6) != "period") return("NA")
    ## If dev_tools on, give button selector
    if(substr(section_nm(), 1, 6) == "period" &
       section_pg() == 1 & do_disp_dev_tools == TRUE)
      return(input$factor)
    ## Else, return factor name
    return(this_factor_nm_ord[eval()])
  })
  location_nm <- reactive({
    req(eval())
    this_location[eval()]
  })
  vc_nm <- reactive({
    req(eval())
    this_vc_nm_ord[eval()]
  })
  p_dim_nm <- reactive({
    req(eval())
    this_p_dim_nm_ord[eval()]
  })
  sim_nm <- reactive({
    req(section_nm())
    if(substr(section_nm(), 1, 6) == "period"){ #& do_disp_dev_tools == TRUE){
      if(eval() == "training")
        return(paste0("EEE_p4_0_1_t", period()))
      return(paste(vc_nm(), p_dim_nm(), location_nm(), 
                   paste0("rep", period()), sep = "_"))
    }
  })
  output$sim_nm <- renderText(paste("sim_nm(): ", sim_nm()))
  image_fp <- reactive({
    req(sim_nm())
    req(factor_nm())
    img_nm <- paste0("./images/", factor_nm(), "_", sim_nm(), '.png')
    return(normalizePath(img_nm))
  })
  output$image_fp <- renderText({image_fp()})
  output$image_plot <- renderImage({
    ## Reactive height and width
    width  <- session$clientData$output_image_plot_width
    height <- session$clientData$output_image_plot_height*2
    
    list(src = image_fp(),
         width = width,
         height = height)
  }, deleteFile = FALSE)

  tpath_nm <- reactive({
    req(factor_nm())
      if(factor_nm() == "grand"){
        if(eval() == "training")
          return(paste0("tpath_p4_t"))
        return(paste("tpath", p_dim_nm(), sep = "_"))
      } ## else factor not grand
    return(NA)
  })
  output$tpath_nm <- renderText(paste("tpath_nm(): ", tpath_nm()))
  resp_row <- reactive({
    data.frame(
      participant_num = participant_num,
      full_perm_num   = full_perm_num,
      period          = period(),
      eval            = eval(), 
      factor          = factor_nm(),
      vc              = vc_nm(),
      p_dim           = p_dim_nm(),
      sim_nm          = sim_nm(),
      grand_path      = tpath_nm(),
      interactions    = rv$interactions,
      ttr             = rv$ttr,
      response        = paste(response(), collapse = ", "), ## Collapse vector of var nums to, 1 string.
      ## Answer is difference from avg.
      answer          = paste(diff(), collapse = ", "),   ## Collapse vector of var nums to, 1 string.
      marks           = marks()
    )
  })
  output$resp_row <- renderTable(resp_row())
  ui_row <- reactive({
    data.frame(
      pg         = rv$pg,
      section_pg = section_pg(),
      section_nm = section_nm(),
      sim_nm          = sim_nm(),
      grand_path      = tpath_nm()
    )
  })
  output$ui_row <- renderTable(ui_row())
  
  dat <- reactive({ ## Simulation df with attachments.
    req(sim_nm())
    return(get(sim_nm()))
  })
  cl <- reactive({ ## Simulation df with attachments.
    req(dat())
    return(attr(dat(), "cluster"))
  })
  p <- reactive(ncol(dat()))
  tpath <- reactive({
    sim_nm <- sim_nm()
    req(sim_nm)
    return(get(paste0("tpath_", sim_nm)))
  })
  manip_var <- reactive({
    mv <- which(colnames(dat()) == input$manip_var_nm)
    return(max(mv, 1))
  })
  any_active <- reactive({
    req(factor_nm())
    if(factor_nm() %in% c("pca", "grand", "radial"))
      return(TRUE)
    return(FALSE)
  })
  header <- reactive({
    req(eval())
    if(eval() %in% 1:6)
      return(paste0("Evaluation -- factor: ", factor_nm()))
    if(eval() == "training")
      return(paste0("Training -- factor: ", factor_nm()))
    return("")
  })
  time_left <- reactive({
    time_alotted - rv$sec_on_pg
  })
  timer_info <- reactive({
    paste0("rv$sec_on_pg of time_alotted: ", rv$sec_on_pg, " of ", time_alotted)
  })
  page_info <- reactive({
    paste0(paste0("rv$pg, section_pg(), section_nm(): ", rv$pg,
                  ", ", section_pg(), ", ", section_nm()),
           paste0("header(): ", header()),
           sep = " /n")
  })
  pfs <- reactive({
    paste("period(), factor_nm(), sim_nm(): ", period(), factor_nm(), sim_nm())
  })
  
  #### _Task evaluation -----
  ### Task Response
  response <- reactive({
    if(substr(section_nm(), 1, 6) == "period")
      ## Vector of the numbers without 'V'
      return(as.integer(gsub(' |V', '', input$response)))
    return("NA")
  })
  output$response <- renderPrint(response())
  ### Task scoring
  diff <- reactive({
    if(any_active()){
      signal <- attr(dat(), "var_mean_diff_ab")
      bar    <- sum(signal) / p()
      return(signal - bar)
    }
    return("NA")
  })
  output$diff <- renderPrint({
    if(any_active() == TRUE)
      round(diff(), 2)
  })
  var_marks <- reactive({
    if(any_active()){
      req(response())
      diff   <- diff()
      ans    <- which(diff >= 0L)
      weight <- sign(diff) * sqrt(abs(diff))
      response   <- response()
      if(response == "NA") return(integer(0))
      return(weight[response])
    }
    return("NA")
  })
  output$var_marks <- renderPrint({
    if(any_active() == TRUE)
    round(var_marks(), 2)
  })
  marks <- reactive({
    req(is.logical(any_active()))
    req(var_marks())
    if(any_active())
      return(sum(var_marks()))
    return("NA")
  })
  output$marks <- renderPrint({
    req(marks())
    if(any_active() == TRUE)
      round(marks(), 2)
  })
  
  
  ### _PCA plot reactive -----
  plot_height <- function(){
    if(any_active() == TRUE){
      return(height_px)
    } else return(1L)
  }
  
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
    input$factor
  }, {
    ## Initialize axis choices when data changes
    if(factor_nm() == "pca"){
      choices <- paste0("PC", 1:PC_cap)
      updateRadioButtons(session, "x_axis", choices = choices, selected = "PC1", inline = TRUE)
      updateRadioButtons(session, "y_axis", choices = choices, selected = "PC2", inline = TRUE)
      loggit("INFO", "Task data changed while axes active; updated PC axes choices.")
    }
    choices <- paste0("V", 1:p())
    updateCheckboxGroupInput(session, "response",
                             choices = choices, inline  = TRUE)
    loggit("INFO", "Task data changed; updated responce choices.")
  })
  ## Bump x_axis when set to the same as y_axis
  observeEvent(input$x_axis, {
    output$plot_msg <- renderText("")
    if(input$x_axis == input$y_axis){
      x_axis_out <- NULL
      choices <- paste0("PC", 1:PC_cap)
      x_axis_num <- as.integer(substr(input$x_axis, 3, 3))
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
      y_axis_num <- as.integer(substr(input$x_axis, 3, 3))
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
  
  ### _Obs radial basis -----
  observeEvent({
    dat()
    input$x_axis
    input$y_axis
    factor_nm()
    input$factor
  }, {
    if(factor_nm() == "radial"){
      dat_std <- tourr::rescale(dat())
      pca <- prcomp(dat_std)
      x <- input$x_axis
      y <- input$y_axis
      x_num <- as.integer(substr(x, nchar(x), nchar(x)))
      y_num <- as.integer(substr(y, nchar(y), nchar(y)))
      rv$curr_basis <- pca$rotation[, c(x_num, y_num)]
      loggit("INFO",
             paste0("New basis set (from dat/axes) "),
             paste0("x_axis: ", x, ", y_axis: ", y, ". rv$curr_basis: ",
                    paste0(round(rv$curr_basis, 2), e = ", "),
                    pfs()
             )
      )
    }
  })
  
 
  
  ### _Obs radial update manip_var_nm choices -----
  observeEvent({
    dat()
    input$factor
  }, {
    ## Init manip_var_nm choices on data change.
    if(factor_nm() == "radial"){
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
      rv$pca_inter <- rv$pca_inter + 1L
    }
  )
  observeEvent({
      input$manip_var_nm
    }, {
      rv$radial_inter <- rv$radial_inter + 1L
    }
  )
  observeEvent({
      input$response
    }, {
      rv$resp_inter <- rv$resp_inter + 1L
    }
  )
  
  ##### _Obs survey answers -----
  ### <REMOVED, see comments_medium.r>
  
  ### _Obs next page button -----
  observeEvent(input$next_pg_button, {
    if((rv$sec_on_pg > 1L & do_log == TRUE) | do_log == FALSE){
      cat(paste0("in loop, top --", rv$pg))
      response  <- response()
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
      def <- "none (default)"
      if(section_nm() == "survey"){
        def <- c(rep("decline to answer (default)", 3L),
                 rep("5 (default)", n_survey_questions - 3L))
      }
      rv$response <- def
      rv$ttr      <- "(default)"
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
  output$is_saved   <- reactive(if(is.null(rv$save_file)){0L}else{1L}) ## Control save_msg.
  output$pg         <- reactive(rv$pg)        ## For hiding ui next_task button
  output$section_nm <- reactive(section_nm()) ## For ui between sections
  output$factor_nm  <- reactive(factor_nm())  ## For sidebar inputs
  output$section_pg <- reactive(section_pg()) ## For navigating training
  output$any_active <- reactive(any_active()) ## For display of the task response.
  output$eval       <- reactive(eval())       ## For sidebar display
  output$dev_tools  <- reactive({             ## For JS eval of R boolean...
    return(do_disp_dev_tools)
  }) 
  #input$response
  
  outputOptions(output, "pg",         suspendWhenHidden = FALSE) ## Eager evaluation for ui conditionalPanel
  outputOptions(output, "section_pg", suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "section_nm", suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "factor_nm",  suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "any_active", suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "eval",       suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "dev_tools",  suspendWhenHidden = FALSE) ## Eager evaluation for ui conditionalPanel

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

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
  rv$answer       <- 0L
  rv$marks        <- 0L
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
  section_nm <- reactive({ ## Text name of section
    req(rv$pg)
    if(rv$pg %in% intro_pgs){  return("intro")}
    if(rv$pg %in% period1_pgs){return("period1")}
    if(rv$pg %in% period2_pgs){return("period2")}
    if(rv$pg %in% period3_pgs){return("period3")}
    if(rv$pg %in% survey_pg){  return("survey")}
    return("!!SECTION NOT DEFINED!!")
  })
  section_pg <- reactive({ ## Current page num of this section.
    if(section_nm() == "intro"){return(rv$pg)}
    if(section_nm() == "period1")
      return(rv$pg - (min(period1_pgs) - 1L))
    if(section_nm() == "period2")
      return(rv$pg - (min(period2_pgs) - 1L))
    if(section_nm() == "period3")
      return(rv$pg - (min(period3_pgs) - 1L))
    if(section_nm() == "survey")
      return(rv$pg - (min(survey_pg) - 1L))
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
  tpath_nm <- reactive({
    req(section_nm())
    if(substr(section_nm(), 1, 6) == "period"){ #& do_disp_dev_tools == TRUE){
      if(eval() == "training")
        return(paste0("tpath_p4_t"))
      return(paste("tpath", p_dim_nm(), sep = "_"))
    }
  })
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
  var_score <- reactive({
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
  output$var_score <- renderPrint({
    if(any_active() == TRUE)
    round(var_score(), 2)
  })
  score <- reactive({
    req(is.logical(any_active()))
    req(var_score())
    if(any_active())
      return(sum(var_score()))
    return("NA")
  })
  output$score <- renderPrint({
    req(score())
    if(any_active() == TRUE)
      round(score(), 2)
  })
  
  ### _PCA plot reactive -----
  plot_height <- function(){
    if(any_active() == TRUE){
      return(height_px)
    } else return(1L)
  }
  pca_plot <- reactive({
    if(factor_nm() == "pca"){
      req(sim_nm())
      req(input$x_axis)
      req(input$y_axis)
      
      x_num  <- as.integer(substr(input$x_axis, 3L, 3L))
      y_num  <- as.integer(substr(input$y_axis, 3L, 3L))
      
      plot_single_pca(sim_nm(), x_num, y_num)
    }
  })
  
  ### _Grand tour plotly reactive -----
  grand_plot_ls <- reactive({
    if(factor_nm() == "grand"){
      ## Initialize
      dat_std <- dat()
      cluster <- cl()
      tpath   <- tpath()
      ##
      angle <- .1
      fps   <- 6L
      max_frames <- 90L ## 90 frame for 15 sec @ fps = 6
      axes_position <- "left"
      ##
      cluster     <- rep(cluster, max_frames)
      tour_array  <- tourr::interpolate(basis_set = tpath, angle = angle)
      attr(tour_array, "class") <- "array"
      
      gg_ls <- list()
      for(i in 1L:90L){
        gg_ls[[i]] <- 
          view_frame(basis = tour_array[,, i], data = dat_std,
                     axes = "left",
                     aes_args = list(color = cluster, shape = cluster),
                     identity_args = list(size = 3L),
                     ggproto = list(theme_spinifex(),
                                    theme(legend.position = "none"),
                                    scale_colour_manual(values = pal)
                     )
          )
      }
      
      return(gg_ls)
    }
  })
  
  
  ### _Radial tour plotly reactive -----
  radial_plot_ls <- reactive({
    if(factor_nm() == "radial"){
      ## Data init
      dat_std <- dat()
      bas <- basis_pca(dat_std)
      mvar <- manip_var()
      cluster <- cl()
      fps <- 6L
      axes_position <- "left"
      ## Needed only for setting scales and aspect ratio
      angle <- seq(0L, 2L * pi, length = 360L)
      circ  <- scale_axes(data.frame(x = cos(angle), y = sin(angle)),
                          axes_position, dat_std)
      x_range <- c(min(dat_std[, 1L], circ[, 1L]), max(dat_std[, 1L], circ[, 1L]))
      y_range <- c(min(dat_std[, 2L], circ[, 2L]), max(dat_std[, 2L], circ[, 2L]))
      ##
      tour_array <- manual_tour(basis = bas, manip_var = mvar)
      num_bas <- dim(tour_array)[3L]
      
      gg_ls <- list()
      for(i in 1L:num_bas){
        gg_ls[[i]] <- 
          view_frame(basis = tour_array[,, i], data = dat_std, 
                     manip_var = mvar, axes = "left",
                     aes_args =  list(color = cluster, shape = cluster),
                     identity_args = list(size = 3L),
                     ggproto = list(theme_spinifex(), 
                                    theme(legend.position = "none"),
                                    scale_colour_manual(values = pal)
                     )
          )
      }
      
      return(gg_ls)
    } ## End of factor_nm() == "radial"
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
    input$factor
  }, {
    ## Init axis choices when data changes
    if(factor_nm == "pca"){
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
  observeEvent(input$survey1, {
    if(rv$sec_on_pg > 1){
      i <- 1
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey1
      loggit("INFO", "Survey 1 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey2, {
    if(rv$sec_on_pg > 1){
      i <- 2
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey2
      loggit("INFO", "Survey 2 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey3, {
    if(rv$sec_on_pg > 1){
      i <- 3
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey3
      loggit("INFO", "Survey 3 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey4, {
    if(rv$sec_on_pg > 1){
      i <- 4
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey4
      loggit("INFO", "Survey 4 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey5, {
    if(rv$sec_on_pg > 1){
      i <- 5
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey5
      loggit("INFO", "Survey 5 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey6, {
    if(rv$sec_on_pg > 1){
      i <- 6
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey6
      loggit("INFO", "Survey 6 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey7, {
    if(rv$sec_on_pg > 1){
      i <- 7
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey7
      loggit("INFO", "Survey 7 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey8, {
    if(rv$sec_on_pg > 1){
      i <- 8
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey8
      loggit("INFO", "Survey 8 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey9, {
    if(rv$sec_on_pg > 1){
      i <- 9
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey9
      loggit("INFO", "Survey 9 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey10, {
    if(rv$sec_on_pg > 1){
      i <- 10
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey10
      loggit("INFO", "Survey 10 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey11, {
    if(rv$sec_on_pg > 1){
      i <- 11
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey11
      loggit("INFO", "Survey 11 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey12, {
    if(rv$sec_on_pg > 1){
      i <- 12
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey12
      loggit("INFO", "Survey 12 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey13, {
    if(rv$sec_on_pg > 1){
      i <- 13
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey13
      loggit("INFO", "Survey 13 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey14, {
    if(rv$sec_on_pg > 1){
      i <- 14
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey14
      loggit("INFO", "Survey 14 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey15, {
    if(rv$sec_on_pg > 1){
      i <- 15L
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey15
      loggit("INFO", "Survey 15 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey16, {
    if(rv$sec_on_pg > 1L){
      i <- 16L
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey16
      loggit("INFO", "Survey 16 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey17, {
    if(rv$sec_on_pg > 1L){
      i <- 17L
      rv$ttr[i] <- rv$sec_on_pg
      rv$response[i] <- input$survey17
      loggit("INFO", "Survey 17 entered.",
             paste0("Response: ", rv$response[i],
                    ". ttr: ", rv$ttr[i], ".")
      )
    }
  })
  
  ### _Obs next page button -----
  observeEvent(input$next_pg_button, {
    if((rv$sec_on_pg > 1L & do_log == TRUE) | do_log == FALSE){
      cat(paste0("in loop, top --", rv$pg))
      response  <- response()
      score <- score()
      
      ### __Training evaluation -----
      ## if training section, evaluate response
      ## TODO: NEED TO CHANGE to eval() == "training"
      # if(eval() == "training"){
      #   this_char <- "" ## Init
      #   ## Evaluation of the training for task
      #   if(rv$training_aes == FALSE){
      #     rv$training_aes <- TRUE
      #     bar   <- -.5
      #     if(score < bar){ ## score not passing
      #       this_char <-
      #         "That seems a little off. Remember that the importance of a
      #         variable for distinguishing a group is related to variables in a
      #         separating direction with large magnitudes in the projection, but
      #         variables with small contributions cannot be ruled out, multiple
      #         projections most be looked at."
      #     }
      #     if(score >= bar){ ## score is passing
      #       this_char <-
      #         "Very good! As a reminder, the importance of a variable for
      #         distinguishing a group is related to variables in a separating
      #         direction with large magnidutes in the projection, but variables
      #         with small contributions cannot be ruled out, multiple
      #         projections most be looked at."
      #     }
      #     output$plot_msg <- renderText(
      #       app_html_red(this_char)
      #     )
      #     return("NA")
      #   }
      # } ## end of training section evaluation
      
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
      return(paste0("Time elapsed this task: ", lubridate::seconds_to_period(rv$sec_on_pg)))
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
  output$the_plot <- renderPlot({
    if(factor_nm == "pca") pca_plot()
    if(factor_nm == "grand") grand_plot()
    if(factor_nm == "radial") radial_plot()
  }, height = plot_height)
  output$plot_ui <- renderUI({
    if(factor_nm() == "pca"){
      ## Initialize
      output$the_plot <- renderPlot({pca_plot()}, height = plot_height)
      
      ## UI
      fluidRow(
        plotOutput("the_plot", height = "100%")
      ) ## End fluidRow()
    } ## End if(active)
    if(factor_nm() == "grand"){
      req(grand_plot_ls())
      ## Initialize
      output$the_plot <-
        renderPlot({grand_plot_ls()[[input$grand_slider]]}, height = plot_height)
      
      ## UI
      fluidRow(
        plotOutput("the_plot", height = "100%"),
        sliderInput("grand_slider", "Grand tour frame:",
                    min = 1L, max = 90L,
                    value = 1L, step = 1L,
                    animate =
                      animationOptions(interval = 167L, loop = FALSE))
      ) ## End fluidRow()
    } ## End if(active)
    if(factor_nm() == "radial"){
      req(radial_plot_ls())
      ## Initialize
      output$the_plot <-
        renderPlot({radial_plot_ls()[[input$radial_slider]]}, height = plot_height)
      
      ## UI
      fluidRow(
        plotOutput("the_plot", height = "100%"),
        sliderInput("radial_slider", "Radial tour frame:",
                    min = 1L, max = p(),
                    value = 1L, step = 1L,
                    animate =
                      animationOptions(interval = 167L, loop = FALSE))
      ) ## End fluidRow()
    } ## End if(active)
  }) ## End renderUI(), assigning output$plot_ui
  output$radial_slider <- renderUI({
    req(factor_nm())
    if(factor_nm() == "radial"){
      ## Initialize
      req(radial_plot_ls())
      n_frames <- length(radial_plot_ls())
      #n_frames <- 40
      
      # output$radial_plot <-
      #   renderPlot({radial_plot_ls()[[input$radial_slider]]}, height = plot_height)
      
      fluidRow(
        # # plotOutput("radial_plot", height = "100%"),
        # radioButtons(inputId = "manip_var_nm", label = "Manip variable:",
        #              choices =  "V1", selected = "V1"),
        sliderInput("radial_slider", "Radial tour frame:",
                    min = 1L, max = n_frames,
                    value = 1L, step = 1L,
                    animate =
                      animationOptions(interval = 167L, loop = FALSE))
      )
    } ## End if(active)
  }) ## End renderUI(), assigning output$radial_input
  # output$radial_slider <- renderUI({
  #   if(factor_nm() == "radial"){
  #     ## Initialize
  #     req(radial_plot_ls())
  #     n_frames <- length(radial_plot_ls())
  #     
  #     sliderInput("radial_slider", "Radial tour frame:",
  #                 min = 1, max = n_frames,
  #                 value = 1, step = 1,
  #                 animate =
  #                   animationOptions(interval = 167L, loop = FALSE))
  #   } ## End if(active)
  # }) ## End renderUI(), assigning output$radial_input
  
  output$radial_plot <-
    renderPlot({radial_plot_ls()[[input$radial_slider]]}, height = plot_height)
  
  dummy_txt <- reactive({
    paste0("dummy slider value: ", input$dummy)
  })
  output$dummy          <- renderPrint(dummy_txt())
  # output$resp_tbl       <- renderTable({rv$resp_tbl})
  # 
  # ### dev_tools display -----
  # output$resp_tbl <- renderTable({
  #   if(do_disp_dev_tools == TRUE){
  #     rv$resp_tbl
  #   }
  # })
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

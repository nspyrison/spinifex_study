source('global.r', local = TRUE)
## Also try: shiny::runApp(appDir = "apps/study", display.mode = "showcase")


####### Server function, for shiny app
server <- function(input, output, session){
  output$TMP <- renderText(input$TMP)

  ##### Reavtive value initialization -----
  rv                  <- reactiveValues()
  ## SET STARTING PAGE HERE <<<
  rv$pg               <- 3L 
  rv$timer            <- 999L
  rv$stopwatch        <- 0L
  rv$timer_active     <- TRUE
  rv$pca_ls           <- list()
  rv$grand_ls         <- list()
  rv$radial_ls        <- list()
  rv$pca_inter        <- 0L
  rv$grand_inter      <- 0L
  rv$radial_inter     <- 0L
  rv$resp_inter       <- 0L
  rv$task_ttr         <- NULL
  rv$task_response    <- NULL
  rv$save_file        <- NULL
  rv$training_aes     <- FALSE
  rv$second_training  <- FALSE
  rv$curr_basis       <- NULL
  rv$radial_tour_ls   <- list()
  rv$resp_tbl         <- 
    data.frame(
      participant_num = participant_num,
      full_perm_num   = full_perm_num, 
      factor          = rep(this_factor_nm_ord, length.out = 99),
      block_location  = rep(this_location, length.out = 99),
      block_vc        = rep(this_vc_nm_ord, length.out = 99), 
      block_p_dim     = rep(p_dim_nms, length.out = 99),
      sim_nm          = rep(this_sim_nms, length.out = 99),
      #sim_nm          = patse(block_vc, block_p_dim, block_location, sep = "_"),
      pca_inter       = NA,
      radial_inter    = NA,
      grand_inter     = NA, 
      resp_inter      = NA,
      plot_elapsed    = NA,
      ttr             = NA,
      response        = NA,
      answer          = NA,
      marks           = NA
  )
  
  
  ##### Reactives -----
  section_nm <- reactive({ ## text name of section
    req(rv$pg)
    if(rv$pg %in% 1L:2L){return("intro")}
    .pgs <- training_start_pg:(task_start_pg - 1L)
    if(rv$pg %in% .pgs){return("training")}
    .pgs <- task_start_pg:(survey_start_pg - 1L)
    if(rv$pg %in% .pgs){return("task")}
    if(rv$pg == survey_start_pg){return("survey")}
    return("!!SECTION NOT DEFINED!!")
  })
  section_pg <- reactive({ ## current page num of this section.
    if(section_nm() == "intro"){return(rv$pg)}
    if(section_nm() == "training"){
      return(rv$pg - (training_start_pg - 1L))
    }
    if(section_nm() == "task"){
      return(rv$pg - (task_start_pg - 1L))
    }
    if(section_nm() == "survey"){return(1L)}
  })
  period <- reactive({
    if(section_nm() == "training" | section_nm() == "task")
      1L + (section_pg() - 1L) %/% (n_p_dim)
  })
  task   <- reactive({
    if(section_nm() == "training" | section_nm() == "task")
      1L + (section_pg() - 1L) %% (n_p_dim)
  })
  factor_nm <- reactive({ ## ~ for group 1: pca, grand, radial
    if(section_nm() == "training" & do_disp_dev_tools == TRUE) return(input$factor)
    if(section_nm() == "training" & do_disp_dev_tools == FALSE) 
      return(input$factor)
    
    if(section_nm() == "task") return(this_factor_nm_ord[period()])
    return("NONE / NA")
  })
  block_location_nm <- reactive({
    req(task())
    this_location[task()]
  })
  block_vc_nm <- reactive({
    req(task())
    this_vc_nm_ord[task()]
  })
  block_p_dim_nm <- reactive({
    req(task())
    p_dim_nm_ord[task()]
  })
  
  sim_nm <- reactive({
    if(section_nm() == "training"){ #& do_disp_dev_tools == TRUE){
      req(input$simVc, input$simP, input$simLocation)
      return(paste(input$simVc, input$simP, input$simLocation, sep = "_"))
    }
    # #TODO
    # if(section_nm() == "training"){
    #   req(input$simVc, input$simP, input$simLocation)
    #   return(this_sim_nms[1])
    # }
    sim_num <- 1 + (section_pg() - 1) %% length(this_sim_nms)
    return(this_sim_nms[sim_num])
  })

  task_time <- reactive({
    req(factor_nm())
    if(factor_nm() == "grand"){adj <- 1L
    } else adj <- 0L
    return(180L + adj)
  })
  time_elapsed <- reactive({ as.integer(task_time() - rv$timer) })
  dat <- reactive({ ## Simulation df with attachments.
    req(sim_nm())
    return(get(sim_nm()))
  })
  cl <- reactive({ ## Simulation df with attachments.
    req(dat())
    return(attr(dat(), "cluster"))
  })
  p <- reactive({ ncol(dat()) })
  task_tpath <- reactive({
    sim_nm <- sim_nm()
    req(sim_nm)
    return(get(paste0("tpath_", sim_nm)))
  })
  manip_var <- reactive({
    req(input$manip_var_nm)
    return(which(colnames(dat()) == input$manip_var_nm))
  })
  pca_active <- reactive({
    if(factor_nm() == "pca"){
      return(TRUE)
    } else return(FALSE)
  })
  grand_active <- reactive({
    if(factor_nm() == "grand"){
      return(TRUE)
    } else return(FALSE)
  })
  radial_active <- reactive({
    if(factor_nm() == "radial"){
      return(TRUE)
    }else return(FALSE)
  })
  any_active <- reactive({
    return(pca_active() | grand_active() | radial_active())
  })
  task_header <- reactive({paste0("Evaluation -- factor: ", factor_nm())})
  timer_info <- reactive({
    paste0("time_elapsed() of task_time(): ", time_elapsed(), " of ", task_time())
  })
  page_info <- reactive({
    paste0(sep = " /n",
           paste0("rv$pg, section_pg(), section_nm(): ", rv$pg, 
                  ", ", section_pg(), ", ", section_nm()),
           paste0("task_header(): ", task_header())
    )
  })
  pfs <- reactive({
    paste("period(), factor_nm(), sim_nm(): ", period(), factor_nm(), sim_nm())
  })
  
  #### _Task evaluation -----
  ### Task Response
  task_resp <- reactive({
    if(section_nm() %in% c("training", "task"))
      ## Vector of the numbers without 'V'
      return(as.integer(gsub(' |V', '', input$task_response)))
  })
  output$task_resp <- renderPrint(task_resp())
  ### Task answer
  var_mean_diff_ab <- reactive({
    if(any_active())
      attr(dat(), "var_mean_diff_ab")
  })
  output$var_mean_diff_ab <- renderPrint({
    round(var_mean_diff_ab(), 2)
  })
  ### bar to beat
  avg_mean_diff_ab <- reactive({
    if(any_active())
      sum(var_mean_diff_ab()) / p()
  })
  output$avg_mean_diff_ab <- renderPrint({
    round(avg_mean_diff_ab(), 2)
  })
  task_ans <- reactive({
    task_resp()
  })
 
  task_score <- reactive({
    if(section_nm() %in% c("training", "task")){
      ans  <- task_ans()
      resp <- task_resp()
      #TODO need to look at clSep .rmd
      "<stuff here>"
    }
    return(0L)
  })
  
  ### _PCA plot reactive -----
  pca_height <- function(){
    if(pca_active() == TRUE){
      return(height_px)
    } else return(1L)
  }
  pca_plot <- reactive({
    if(pca_active() == TRUE){
      req(input$x_axis, input$y_axis)
      dat_std <- dat()
      cluster <- cl()
      axes_position <- "left"
      
      x_axis <- input$x_axis
      y_axis <- input$y_axis
      x_num  <- as.integer(substr(x_axis, 3L, 3L))
      y_num  <- as.integer(substr(y_axis, 3L, 3L))
      x_pc   <- paste0("PC", x_num)
      y_pc   <- paste0("PC", y_num)
      
      pca     <- prcomp(dat_std)
      pca_x   <- as.data.frame(pca$x)
      pca_rot <- data.frame(pca$rotation[ , c(x_num, y_num)])
      pca_rot <- scale_axes(pca_rot, axes_position, pca_x)
      pca_pct_var <- round(100L * pca$sdev^2L / sum(pca$sdev^2L), 1L)
      x_lab <- paste0(x_axis, " (", pca_pct_var[x_num], "% Var)")
      y_lab <- paste0(y_axis, " (", pca_pct_var[y_num], "% Var)")
      
      angle <- seq(0L, 2L * pi, length = 360L)
      circ  <- scale_axes(data.frame(x = cos(angle), y = sin(angle)),
                                     axes_position, pca_x)
      zero  <- scale_axes(data.frame(x = 0L, y = 0L),
                          axes_position, pca_x)
      x_range <- max(pca_x[, 1L], circ[, 1L]) - min(pca_x[, 1L], circ[, 1L])
      y_range <- max(pca_x[, 2L], circ[, 2L]) - min(pca_x[, 2L], circ[, 2L])
      
      ### ggplot2
      gg <- ggplot() +
        ## Themes and aesthetics
        theme_minimal() +
        scale_color_brewer(palette = pal) +
        theme(panel.grid.major = element_blank(), ## no grid lines
              panel.grid.minor = element_blank(), ## no grid lines
              axis.text.x  = element_blank(),     ## no axis marks
              axis.text.y  = element_blank(),     ## no axis marks
              axis.title.x = element_text(size = 22L, face = "bold"),
              axis.title.y = element_text(size = 22L, face = "bold"),
              aspect.ratio = y_range / x_range,
              legend.box.background = element_rect(),
              legend.title = element_text(size = 18L, face = "bold"),
              legend.text  = element_text(size = 18L, face = "bold")
        ) +
        labs(x = x_lab, y = y_lab)
      ## Data points
      gg <- gg +
        geom_point(pca_x,
                   mapping = aes(x = get(x_axis), y = get(y_axis),
                                 color = cluster,
                                 fill  = cluster,
                                 shape = cluster),
                   size = 3L)
      ## Axis segments
      gg <- gg +
        geom_segment(pca_rot,
                     mapping = aes(x = get(x_pc), xend = zero[, 1L],
                                   y = get(y_axis), yend = zero[, 2L]),
                     size = .3, colour = "red") +
        ## Axis label text
        geom_text(pca_rot,
                  mapping = aes(x = get(x_axis),
                                y = get(y_axis),
                                label = colnames(dat_std)),
                  size = 6L, colour = "red", fontface = "bold",
                  vjust = "outward", hjust = "outward") +
        ## Circle path
        geom_path(circ, mapping = aes(x = x, y = y),
                  color = "grey80", size = .3, inherit.aes = F)
      
      return(gg)
    }
  })
  
  ### _Grand tour plotly reactive -----
  grand_height <- function(){
    if(grand_active() == TRUE){
      return(height_px)
    }else return(1L)
  }
  grand_width <- function(){
    if(grand_active() == TRUE){
      return(width_px)
    }else return(1L)
  }
  grand_plot_ls <- reactive({
    if(grand_active() == TRUE){
      ## Initialize
      dat_std <- dat()
      cluster <- cl()
      tpath   <- task_tpath()
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
                     identity_args = list(size = 3L))
      }
      
      return(gg_ls)
    }
  })
  
  
  ### _Radial tour plotly reactive -----
  radial_height <- function(){
    if(radial_active()){
      return(height_px)
    }else return(1L)
  }
  radial_width <- function(){
    if(radial_active() == TRUE){
      return(width_px)
    }else return(1L)
  }
  radial_plot_ls <- reactive({
    if(radial_active()){
      req(manip_var())
      ## Data init
      dat_std <- dat()
      bas <- basis_pca(dat_std)
      mvar <- manip_var()
      cluster <- cl()
      fps <- 6L
      axes_position <- "left"
      ## Needed only for seting scales and aspect ratio
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
                     identity_args = list(size = 3L))
      }
      
      return(gg_ls)
    } ## End of radial_active()
  })
  
  
  
  ##### Observers ----
  observeEvent({
    dat()
  }, {
    choices <- paste0("V", 1:p())
    updateCheckboxGroupInput(session, "task_response", 
                             choices = choices, selected = "", inline = TRUE)
  })
  ### _Obs update axis/task choices -----
  observeEvent({
    dat()
    input$factor
  }, {
    ## Init axis choices when data changes
    if(pca_active() == TRUE | radial_active() == TRUE){
      choices <- paste0("PC", 1:PC_cap)
      updateRadioButtons(session, "x_axis", choices = choices, selected = "PC1", inline = TRUE)
      updateRadioButtons(session, "y_axis", choices = choices, selected = "PC2", inline = TRUE)
      loggit("INFO", "Task data changed while axes active; updated PC axes choices.")
    }
    choices <- paste0("V", 1:p())
    updateCheckboxGroupInput(session, "task_resp_very_ab",
                             choices = choices, inline  = TRUE)
    updateCheckboxGroupInput(session, "task_resp_some_ab",
                             choices = choices, inline  = TRUE)
    updateCheckboxGroupInput(session, "task_resp",
                             choices = choices, inline  = TRUE)
    updateCheckboxGroupInput(session, "task_resp_some_bc",
                             choices = choices, inline  = TRUE)
    loggit("INFO", "Task data changed; updated task 2 responce choices.")
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
    if(radial_active() == TRUE){
      rv$radial_ls <- list()
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
    if(radial_active() == TRUE){
      these_colnames <- colnames(dat())
      updateRadioButtons(session, "manip_var_nm", choices = these_colnames,
                         selected = these_colnames[1], inline = TRUE)
      loggit("INFO", paste0("Task data or training factor changed; input$manip_var_nm choices updated."))
    }
  })
  
  ### _Obs grand slider value -----
  ## CURRENTLY RESTART BUTTON
  # observeEvent(
  #   {
  #     dat()
  #     factor_nm()
  #     input$factor
  #   }, {
  #     if(grand_active() == TRUE
  #        #& time_elapsed() > 1
  #     ){
  #       rv$granf_ls <- list()
  #       
  #       browser()
  #       ## WOULD NEED NUMBER OF SLIDES
  #       updateSliderInput(session, "grand_restart", value = .val)
  #       loggit("INFO",
  #              paste0("New manip slider value (from dat/axes/manip_var)."),
  #              paste0("manip_slider: ", .val,
  #                     pfs()
  #              )
  #       )
  #     }
  #   }
  # )
  
  ##### _Obs task responses -----
  ### task responses & ttr
  observeEvent(input$task_response, {
    if(time_elapsed() > 1){
      rv$task_ttr[1] <- time_elapsed()
      rv$task_response[1] <- paste(input$task_response, collapse = ", ")
      loggit("INFO", "Task response changed.",
             paste0("Response: ", rv$task_response[1],
                    ". ttr: ", rv$task_ttr[1], ".")
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
      input$task_resp
    }, {
      rv$resp_inter <- rv$resp_inter + 1L
    }
  )
  
  ##### _Obs survey answers -----
  observeEvent(input$survey1, {
    if(time_elapsed() > 1){
      i <- 1
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey1
      loggit("INFO", "Survey 1 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey2, {
    if(time_elapsed() > 1){
      i <- 2
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey2
      loggit("INFO", "Survey 2 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey3, {
    if(time_elapsed() > 1){
      i <- 3
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey3
      loggit("INFO", "Survey 3 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey4, {
    if(time_elapsed() > 1){
      i <- 4
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey4
      loggit("INFO", "Survey 4 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey5, {
    if(time_elapsed() > 1){
      i <- 5
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey5
      loggit("INFO", "Survey 5 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey6, {
    if(time_elapsed() > 1){
      i <- 6
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey6
      loggit("INFO", "Survey 6 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey7, {
    if(time_elapsed() > 1){
      i <- 7
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey7
      loggit("INFO", "Survey 7 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey8, {
    if(time_elapsed() > 1){
      i <- 8
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey8
      loggit("INFO", "Survey 8 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey9, {
    if(time_elapsed() > 1){
      i <- 9
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey9
      loggit("INFO", "Survey 9 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey10, {
    if(time_elapsed() > 1){
      i <- 10
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey10
      loggit("INFO", "Survey 10 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey11, {
    if(time_elapsed() > 1){
      i <- 11
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey11
      loggit("INFO", "Survey 11 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey12, {
    if(time_elapsed() > 1){
      i <- 12
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey12
      loggit("INFO", "Survey 12 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey13, {
    if(time_elapsed() > 1){
      i <- 13
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey13
      loggit("INFO", "Survey 13 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey14, {
    if(time_elapsed() > 1){
      i <- 14
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey14
      loggit("INFO", "Survey 14 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey15, {
    if(time_elapsed() > 1){
      i <- 15L
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey15
      loggit("INFO", "Survey 15 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey16, {
    if(time_elapsed() > 1L){
      i <- 16L
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey16
      loggit("INFO", "Survey 16 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  observeEvent(input$survey17, {
    if(time_elapsed() > 1L){
      i <- 17L
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey17
      loggit("INFO", "Survey 17 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".")
      )
    }
  })
  
  ### _Obs next page button -----
  observeEvent(input$next_pg_button, {
    if((rv$stopwatch > 1L & do_log == TRUE) | do_log == FALSE){
      cat(paste0("in loop, top --", rv$pg))
      task_resp  <- task_resp()
      task_ans   <- task_ans()
      task_score <- task_score()
      
      ### __Training evaluation -----
      ## if training section, evaluate response
      if(section_nm() == "training"){
        this_char <- ""
        ## Evaluation of the training for task
        if(rv$training_aes == FALSE){
          rv$training_aes <- TRUE
          bar   <- -.5
          if(task_score < bar){ ## score not passing
            this_char <-
              "That seems a little off. Remember that the importance of a
              variable for distinguishing a group is related to variables in a
              separating direction with large magnitudes in the projection, but
              variables with small contributions cannot be ruled out, multiple
              projections most be looked at."
          }
          if(task_score >= bar){ ## score is passing
            this_char <-
              "Very good! As a reminder, the importance of a variable for
              distinguishing a group is related to variables in a separating
              direction with large magnidutes in the projection, but variables
              with small contributions cannot be ruled out, multiple
              projections most be looked at."
          }
          output$plot_msg <- renderText(
            app_html_red(this_char)
          )
          return()
        }
      } ## end of training section evaluation
      
      ##### __rv$resp_tbl -----
      ## Write responses and ttr to resp_tbl
      if(section_nm() %in% c("task", "training")){
        ins_row <- which(rv$resp_tbl$sim_nm  == sim_nm())[1L]
        
        ## Is response concerning?
        task_concern <- "no" ## Init
        if(NA %in% rv$task_ttr){
          task_concern <- "YES, ttr contains NA. REMOVE."}
        if(min(rv$task_ttr) == "(default)" & !is.na(time_elapsed())){
          task_concern <- "Some, ttr defaulted, answer not changed,."}
        if(max(rv$task_ttr) > task_time() + 45L &
            !is.na(time_elapsed()) & factor_nm() == "task"){
          task_concern <- "YES, time elaspsed + >45 sec. answer may not be reliable."}
        
        ## Did task time run out
        plot_elapsed <- NA
        if(section_nm() == "task"){
          if(time_elapsed() >  task_time()) plot_elapsed <- 1L
          if(time_elapsed() <= task_time()) plot_elapsed <- 0L
        }
        
        # if(rv$pg == 3) browser()
        # rv$resp_tbl$pca_inter[ins_row]    <- rv$pca_inter
        # rv$resp_tbl$radial_inter[ins_row] <- rv$radial_inter
        # rv$resp_tbl$resp_inter[ins_row]   <- rv$resp_inter
        # rv$resp_tbl$plot_elapsed[ins_row] <- plot_elapsed
        # rv$resp_tbl$ttr[ins_row]          <- rv$task_ttr
        # rv$resp_tbl$response[ins_row]     <- task_resp
        # rv$resp_tbl$answer[ins_row]       <- task_ans
        # rv$resp_tbl$score[ins_row]        <- task_score
        # rv$resp_tbl$concern[ins_row]      <- task_concern
      } ## End of writing to resp_tbl
      cat("!!! ITERATED PG!!!")
      ### __New page ----
      ## Advance to the next page, reset variables
      rv$pg <- rv$pg + 1L
      ## Reset responses, ttr, and timer for next task
      output$plot_msg     <- renderText("")
      rv$pca_inter        <- 0L
      rv$radial_inter     <- 0L
      rv$grand_inter      <- 0L
      rv$resp_inter       <- 0L
      rv$task_response    <- NULL
      rv$task_ttr         <- NULL
      rv$task_answer      <- NULL
      rv$task_score       <- NULL
      rv$timer            <- task_time()
      rv$stopwatch        <- 0L
      rv$timer_active     <- TRUE
      rv$training_aes     <- FALSE
      if(rv$pg == survey_start_pg) shinyjs::hide("next_pg_button")
      
      ## Set structure for writeing to resp_tbl
      ## cluster seperation task:
      ##TODO: responce table writing.
      n_rows <- 1
      def <- "none (default)"
      if(section_nm() == "survey"){
        def <- c(rep("decline to answer (default)", 3),
                 rep("5 (default)", n_survey_questions - 3))
      }
      rv$task_response <- def
      rv$task_ttr      <- "(default)"
      loggit("INFO", paste0("Next page:"), pfs())
      cat("bottom of loop -- ")
    }
    cat("after loop \n")
  })
  
  
  ### _Obs save responses button -----
  observeEvent(input$save_resp, {
    filebase = paste("responses", this_group, Sys.info()[4], sep = "_")
    prefix = ""
    
    ## Write survey responses to rv$resp_tbl
    ins_row_start <- nrow(rv$resp_tbl) - n_survey_questions + 1
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
  observeEvent(input$browser, if(do_disp_dev_tools == TRUE){browser()})
  
  ### _Obs timer -----
  observe({
    invalidateLater(1000, session)
    isolate({
      rv$timer     <- rv$timer - 1
      rv$stopwatch <- rv$stopwatch + 1
    })
    if(rv$timer < 0 & section_nm() == "task" & rv$timer_active == TRUE){
      rv$timer_active <- FALSE
      loggit("INFO", "Timer elapsed.",
             pfs())
    }
  })
  
  
  ##### Outputs -----
  output$timer_disp <- renderText({
    if(section_nm() == "task"){ ## Disp timer counting down if on a task.
      if(rv$timer < 1){return("Time has expired, please enter your best guess and proceed.")
      }else{
        return(
          paste0("Time left: ", lubridate::seconds_to_period(rv$timer),
                 " of ", lubridate::seconds_to_period(task_time()))
        )
      }
    }
    if(section_nm() == "training" & section_pg() != 6){ ## Disp timer Counting up if in training.
      return(paste0("Time elapsed this task: ", lubridate::seconds_to_period(rv$stopwatch)))
    }
  })
  
  ### Condition handling for ui coditionalPanels
  output$is_saved    <- reactive(if(is.null(rv$save_file)){0}else{1}) ## Control save_msg.
  output$pg          <- reactive(rv$pg)        ## For hiding ui next_task button
  output$section_nm  <- reactive(section_nm()) ## For ui between sections
  output$factor_nm   <- reactive(factor_nm())  ## For sidebar inputs
  output$section_pg  <- reactive(section_pg()) ## For navigating training
  output$any_active  <- reactive(any_active()) ## For display of the task response.
  output$dev_tools   <- reactive({ ## For JS eval of R boolean...
    if(do_disp_dev_tools == TRUE){
      return(TRUE)
    }else return(FALSE)
  }) 
  #input$task_response
  
  outputOptions(output, "is_saved",    suspendWhenHidden = FALSE) ## Eager evaluation for ui conditionalPanel
  outputOptions(output, "pg",          suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "section_nm",  suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "factor_nm",   suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "section_pg",  suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "any_active",  suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "dev_tools",   suspendWhenHidden = FALSE) ## Eager evaluation for ui conditionalPanel

  ### General task outputs
  ## height: ggplot applies on renderPlot(), plotly applies to a plotly option.
  output$task_header <- renderText(task_header())
  output$pca_plot <- renderPlot({pca_plot()}, height = pca_height)
  output$grand_ui <- renderUI({
    if(grand_active()){
      ## Initialize
      p <- p()
      output$grand_plot <- 
        renderPlot({grand_plot_ls()[[input$grand_frame]]}, height = grand_height)
      
      ## UI
      fluidRow(
        plotOutput("grand_plot", height = "100%"),
        sliderInput("grand_frame", "Grand tour frame:",
                    min = 1, max = 90,
                    value = 1, step = 1,
                    animate =
                      animationOptions(interval = 167L, loop = FALSE))
      ) ## End fluidRow()
    } ## End if(active)
  }) ## End renderUI(), assigning output$grand_ui
  output$radial_frame <- renderUI({
    if(radial_active()){
      ## Initialize
      req(radial_plot_ls())
      n_frames <- length(radial_plot_ls())
      
      sliderInput("radial_frame", "Radial tour frame:",
                  min = 1, max = n_frames,
                  value = 1, step = 1,
                  animate =
                    animationOptions(interval = 167L, loop = FALSE))
    } ## End if(active)
  }) ## End renderUI(), assigning output$radial_input
  
  output$radial_plot <-
    renderPlot({radial_plot_ls()[[input$radial_frame]]}, height = radial_height)
  
  dummy_txt <- reactive({
    paste0("dummy slider value: ", input$dummy)
  })
  output$dummy          <- renderPrint(dummy_txt())
  output$resp_tbl       <- renderTable({rv$resp_tbl})
  
  ### dev_tools display -----
  output$resp_tbl <- renderTable({
    if(do_disp_dev_tools == TRUE){
      rv$resp_tbl
    }
  })
  output$dev_msg  <- renderPrint({
    if(do_disp_dev_tools == TRUE){
      cat("dev msg -- \n",
          page_info(),
          task_header(),
          timer_info(),
          pfs()
      )
    }
  })
} ## End server function

### Combine as shiny app.
shinyApp(ui = ui, server = server)

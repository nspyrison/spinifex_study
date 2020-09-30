source('global.r', local = TRUE)


####### Server function, for shiny app
server <- function(input, output, session){
  output$TMP <- renderText(input$TMP)
  
  ##### Reavtive value initialization -----
  rv                  <- reactiveValues()
  rv$pg               <- 1L
  rv$timer            <- 999L
  rv$stopwatch        <- 0L
  rv$timer_active     <- TRUE
  rv$pca_inter        <- 0L
  rv$radial_inter     <- 0L
  rv$resp_inter       <- 0L
  rv$task_ttr         <- NULL
  rv$task_response    <- NULL
  rv$save_file        <- NULL
  rv$training_aes     <- FALSE
  rv$second_training  <- FALSE
  rv$curr_basis       <- NULL
  rv$radial_ls        <- list()
  rv$basis_ls         <- list()
  rv$resp_tbl         <- data.frame(
    user_uid     = substr(log_name, 5L, nchar(log_name)),
    group        = this_group,
    factor       = c(rep(this_factor_nm_order[1L], 4L),
                     rep(this_factor_nm_order[2L], 4L),
                     rep(this_factor_nm_order[3L], 4L)),
    block        = rep(c(rep(s_blocks[1L], 2L), rep(s_blocks[2L], 2L)), 3L),
    sim_id       = 301L:312L,
    pca_inter    = NA,
    radial_inter = NA,
    grand_inter  = NA, 
    resp_inter   = NA,
    plot_elapsed = NA,
    ttr          = NA,
    response     = NA,
    answer       = NA,
    marks        = NA
  )
    
  ##### Reactives -----
  p <- reactive({ ncol(dat()) })
  n_cl <- reactive({ length(unique(attributes(s_dat[[block()]])$cl_lvl)) })
  
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
  period <- reactive({1L + ((section_pg() - 1L) %/% (n_blocks))})
  factor_nm <- reactive({ ## ~ for group 1: pca, grand, radial
    if(section_nm() == "training") return(input$factor)
    if(section_nm() == "task") return(this_factor_nm_order[period()])
    return("NONE / NA")
  })
  block <- reactive({ ## in 0, "t", 1,2,3
    if(section_nm() == "training") return(c(0L, "t", "t", "t", "t", 0L)[section_pg()])
    return((section_pg() - n_blocks) %% (n_blocks))
  })
  sim <- reactive({
    if(section_nm() == "training"){
      req(input$simFactor, input$simModel)
      return(paste0(input$simFactor,"_", input$simModel))
    }
    return("baseLn_EEE")
    # if(section_nm() == "training") return("NA - training")
    # return((period() - 1L) * 6L + 3L + block())
  })
  task_time <- reactive({
    req(factor_nm())
    if(factor_nm() == "grand"){adj <- 1L
    } else adj <- 0L
    return(180L + adj)
  })
  time_elapsed <- reactive({ as.integer(task_time() - rv$timer) })
  dat <- reactive({ ## Simulation df with attachments.
    req(section_nm())
    if(section_nm() == "training"){
      ret <- get(sim())[, -1]
      # req(section_pg())
      # ret <- s_t_dat[[section_pg()]]
    } else { ## evaluation section.
      ret <- get(sim())[, -1]
      # req(sim())
      # ret <- s_dat[[sim()]]
    }
    ## Return
    # as.data.frame(tourr::rescale(ret))
    ret
  })
  cl <- reactive({ ## Simulation df with attachments.
    req(section_nm())
    if(section_nm() == "training"){
      ret <- get(sim())[, 1]
    }else{ ## Evaluation section.
      ret <- get(sim())[, 1]
    }
    ## Return
    ret
  })
  
  task_tpath <- reactive({
    if(section_nm() == "training"){
      return(s_t_tpath[[section_pg()]])
    } else {
      return(s_tpath[[sim()]])
    }
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
    # req(rv$timer_active, input$factor)
    if(factor_nm() == "radial"){
      return(TRUE)
    } else return(FALSE)
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
  pfbs <- reactive({
    paste("period(), factor_nm(), block(), sim(): ", period(), factor_nm(), block(), sim())
  })
  
  #### Task answer
  task_ans <- reactive({
    dat_std <- dat()
    cl <- dat_std$cl
    #TODO need to bring in latest clSep function
    rep(0, p())
  })
  ### Response
  task_resp <- reactive({
    if(section_nm() %in% c("training", "task")) 
      ## Vector of the numbers without 'V'
      return(as.integer(gsub(' |V', '', input$task_response)))
    return(0)
  })
  task_score <- reactive({
    if(section_nm() %in% c("training", "task")){
      ans  <- task_ans()
      resp <- task_resp()
      #TODO need to look at clSep .rmd
      "<stuff here>"
    }
    return(0)
  })
  
  ### PCA plot reactive -----
  pca_height <- function(){
    if(pca_active() == TRUE){
      return(600)
    } else return(1)
  }
  pca_plot <- reactive({
    if(pca_active() == TRUE){
      dat_std <- dat()
      clas <- cl()
      
      axes_position <- "left"
      USE_AXES <- TRUE
      USE_AES  <- TRUE
      
      x_axis <- input$x_axis
      y_axis <- input$y_axis
      x_num  <- as.integer(substr(x_axis, 3L, 3L))
      y_num  <- as.integer(substr(y_axis, 3L, 3L))
      
      pca     <- prcomp(dat_std)
      pca_x   <- 2 * (data.frame(tourr::rescale(pca$x[ , c(x_num, y_num)])) - .5)
      pca_rot <- data.frame(pca$rotation[ , c(x_num, y_num)])
      pca_rot <- app_set_axes_position(pca_rot, axes_position)
      pca_pct_var <- round(100L * pca$sdev^2 / sum(pca$sdev^2), 1)
      x_lab <- paste0(x_axis, " (", pca_pct_var[x_num], "% Var)")
      y_lab <- paste0(y_axis, " (", pca_pct_var[y_num], "% Var)")
      
      angle <- seq(0L, 2L * pi, length = 360L)
      circ  <- app_set_axes_position(data.frame(x = cos(angle), y = sin(angle)),
                                     axes_position)
      zero  <- app_set_axes_position(0L, axes_position)
      
      ### ggplot2
      gg <- ggplot()
      if(USE_AES == FALSE){
        # data points
        gg <- gg +
          geom_point(pca_x,
                     mapping = aes(x = get(x_axis), y = get(y_axis)),
                     size = 3L)
      } else { ## if USE_AES == TRUE then apply more aes.
        gg <- gg +
          geom_point(pca_x,
                     mapping = aes(x = get(x_axis), y = get(y_axis),
                                   color = clas,
                                   fill  = clas,
                                   shape = clas),
                     size = 3L)
      }
      if(USE_AXES == TRUE){ ## then draw axes
        # axis segments
        gg <- gg +
          geom_segment(pca_rot,
                       mapping = aes(x = get(x_axis), xend = zero[, 1L],
                                     y = get(y_axis), yend = zero[, 2L]),
                       size = .3, colour = "red") +
          # axis label text
          geom_text(pca_rot,
                    mapping = aes(x = get(x_axis),
                                  y = get(y_axis),
                                  label = colnames(dat_std)),
                    size = 6, colour = "red", fontface = "bold",
                    vjust = "outward", hjust = "outward") +
          # Cirle path
          geom_path(circ, mapping = aes(x = x, y = y),
                    color = "grey80", size = .3, inherit.aes = F)
      }
      
      x_range <- max(pca_x[, 1L], circ[, 1L]) - min(pca_x[, 1L], circ[, 1L])
      y_range <- max(pca_x[, 2L], circ[, 2L]) - min(pca_x[, 2L], circ[, 2L])
      # Options
      gg <- gg + theme_minimal() +
        scale_color_brewer(palette = pal) +
        theme(panel.grid.major = element_blank(), # no grid lines
              panel.grid.minor = element_blank(), # no grid lines
              axis.text.x  = element_blank(),     # no axis marks
              axis.text.y  = element_blank(),     # no axis marks
              axis.title.x = element_text(size = 22L, face = "bold"),
              axis.title.y = element_text(size = 22L, face = "bold"),
              aspect.ratio = y_range / x_range,
              legend.box.background = element_rect(),
              legend.title = element_text(size = 18L, face = "bold"),
              legend.text  = element_text(size = 18L, face = "bold")
        ) +
        labs(x = x_lab, y = y_lab)
      
      return(gg)
    }
  })
  
  ### Grand tour plot reactive -----
  grand_height <- function(){
    if(grand_active() == T){
      return(600)
    } else return(1)
  }
  grand_plot <- reactive({
    if(grand_active() == T){
      ## data init
      dat_std <- dat()
      clas <- cl()
      
      ## tour init
      angle <- .1
      fps   <- 6
      max_frames <- 90 ## 90 frame for 15 sec @ fps = 6
      
      tpath <- task_tpath()
      
      full_path <- tourr::interpolate(basis_set = tpath, angle = angle)
      attr(full_path, "class") <- "array"
      max_frames <- min(c(max_frames, dim(full_path)[3]))
      full_path <- full_path[,, 1:max_frames]
      
      tour_df <- array2df(array = full_path, data = dat_std)
      
      ## render init
      axes_position <- "left"
      angle <- seq(0, 2 * pi, length = 360)
      circ  <- app_set_axes_position(data.frame(x = cos(angle), y = sin(angle)),
                                     axes_position)
      zero  <- app_set_axes_position(0, axes_position)
      
      ### ggplot2
      basis_df <- tour_df$basis_slides
      basis_df[, 1:2] <- app_set_axes_position(tour_df$basis_slides[, 1:2], axes_position)
      colnames(basis_df) <- c("x", "y", "frame", "lab")
      data_df  <- tour_df$data_frames
      data_df[, 1:2] <- 2 * (tourr::rescale(data_df[, 1:2]) - .5)
      clas <- rep(clas, max_frames)
      
      gg <- ggplot()
      ## Projected data points with cluster aesthetics
      gg <- gg +
        geom_point(data_df,
                   mapping = aes(x = x, y = y, frame = frame,
                                 color = clas,
                                 fill  = clas,
                                 shape = clas),
                   size = 1.7) + ## smaller size for plotly
        ## Axis segments
        geom_segment(basis_df,
                     mapping = aes(x = x, xend = zero[, 1],
                                   y = y, yend = zero[, 2],
                                   frame = frame),
                     size = .3, colour = "red") +
        ## Axis label text
        geom_text(basis_df,
                  mapping = aes(x = x,
                                y = y,
                                frame = frame,
                                label = lab),
                  size = 6, colour = "red", fontface = "bold",
                  vjust = "outward", hjust = "outward") +
        ## Cirle path
        geom_path(circ,
                  mapping = aes(x = x, y = y),
                  color = "grey80", size = .3, inherit.aes = F)
      
      x_range <- max(data_df[, 1], circ[, 1]) - min(data_df[, 1], circ[, 1])
      y_range <- max(data_df[, 2], circ[, 2]) - min(data_df[, 2], circ[, 2]) +
        ## Themes and aesthetic settings
        theme_minimal() +
        scale_color_brewer(palette = pal) +
        scale_fill_brewer(palette  = pal) +
        theme(panel.grid.major = element_blank(), ## no grid lines
              panel.grid.minor = element_blank(), ## no grid lines
              axis.text.x  = element_blank(),     ## no axis marks
              axis.text.y  = element_blank(),     ## no axis marks
              axis.title.x = element_blank(),     ## no axis titles for grand
              axis.title.y = element_blank(),     ## no axis titles for grand
              aspect.ratio = y_range / x_range,
              legend.title = element_text(size = 18, face = "bold"),
              legend.text  = element_text(size = 18, face = "bold"),
              legend.box.background = element_rect()
        ) ## end of ggplot2 work
      
      ### plotly
      ggp <- plotly::ggplotly(p = gg, tooltip = "none") %>% 
        plotly::animation_opts(p = ggp, frame = 1 / fps * 1000,
                               transition = 0, redraw = FALSE) %>% 
        plotly::layout(
          ggp, showlegend = T, yaxis = list(showgrid = F, showline = F),
          xaxis = list(scaleanchor = "y", scaleratio = 1, showgrid = F,
                       showline = F, autorange = TRUE, fixedrange = FALSE),
          ## added for APP:
          height = grand_height(),
          yaxis = list(autorange = TRUE, fixedrange = FALSE), ## suppose to rescale, I don't think it does.
          legend = list(x = 0.75, y = 0.7) ## postition the title better
        )
      
      return(ggp)
    }
  })
  
  
  ### radial tour plot reactive -----
  radial_height <- function(){
    if(radial_active()){
      return(600)
    } else return(1)
  }
  radial_plot <- reactive({
    if(radial_active()){
      ### Make rv$radial_ls
      if(length(rv$radial_ls) == 0){
        ## data init
        dat_std <- dat()
        clas <- cl()
        m_var <- manip_var()
        
        ## slider to phi/theta
        theta <- phi <- NULL
        mv_sp <- create_manip_space(rv$curr_basis, m_var)[m_var, ]
        theta <- atan(mv_sp[2] / mv_sp[1]) ## Radial angle
        phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
        phi_pts <- acos((0:10) / 10) ## Possible angles to select with manip_slider
        phi_vect <- (phi_pts - phi_start) * - sign(mv_sp[1])
        
        browser()
        for (i in 1:length(phi_vect)){
          rv$basis_ls[[i]] <- view_frame(
            basis = rv$curr_basis, manip_var = manip_var(),
            theta = theta, phi = phi_vect[i])
          row.names(rv$basis_ls[[i]]) <- colnames(dat)
        }
        
        ## Render init
        axes_position <- "left"
        
        
        for (i in 1:length(rv$basis_ls)){
            rv$radial_ls[[i]] <- app_oblique_frame(data      = dat_std,
                                                   basis     = rv$basis_ls[[i]],
                                                   manip_var = m_var,
                                                   theta     = 0,
                                                   phi       = 0,
                                                   cluster   = clas,
                                                   axes      = axes_position
            )
        }
      } ## End of assigning rv$radial_ls
      

    } ## Non-display conditions return nothing.
  })
  
  
 
  
  
  ##### Start observes
  ### Obs update axis/task choices -----
  observeEvent({
    dat()
    input$factor
  }, {
    ## Init axis choices when data changes
    if(pca_active() == TRUE | radial_active() == TRUE){
      choices <- paste0("PC", 1:PC_cap)
      updateRadioButtons(session, "x_axis", choices = choices, selected = "PC1")
      updateRadioButtons(session, "y_axis", choices = choices, selected = "PC2")
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
      } else {x_axis_out <- paste0("PC", x_axis_num - 1)}
      
      updateRadioButtons(session, "x_axis", choices = choices, selected = x_axis_out)
      loggit("INFO", paste0("x_axis set to ", input$x_axis,
                            ", same as y_axis; x_axis bumped to ", x_axis_out, "."),
             pfbs()
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
      } else {y_axis_out <- paste0("PC", y_axis_num - 1)}
      
      updateRadioButtons(session, "y_axis", choices = choices, selected = y_axis_out)
      loggit("INFO", paste0("y_axis set to ", input$y_axis,
                            ", same as x_axis; y_axis bumped to ", y_axis_out, "."),
             pfbs()
      )
      
      output$plot_msg <- renderText(
        app_html_red(paste0("Must select different principal components."))
      )
    }
  })
  
  ### Obs radial basis -----
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
                    pfbs()
             )
      )
    }
  })
  
 
  
  ### Obs radial update manip_var_nm choices -----
  observeEvent({
    dat()
    input$factor
  }, {
    ## Init manip_var_nm choices on data change.
    if(radial_active() == TRUE){
      these_colnames <- colnames(dat())
      updateRadioButtons(session, "manip_var_nm", choices = these_colnames,
                         selected = these_colnames[1])
      loggit("INFO", paste0("Task data or training factor changed; input$manip_var_nm choices updated."))
    }
  })
  
  ### Obs grand slider value -----
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
  #                     pfbs()
  #              )
  #       )
  #     }
  #   }
  # )
  
  ##### Obs task responses -----
  ### task responses & ttr
  observeEvent(input$task_resp_very_ab, {
    if(time_elapsed() > 1){
      rv$task_ttr[1] <- time_elapsed()
      rv$task_response[1] <- paste(input$task_resp_very_ab, collapse = ", ")
      loggit("INFO", "Task 2, very important, clusters ab response entered.",
             paste0("Response: ", rv$task_response[1],
                    ". ttr: ", rv$task_ttr[1], ".",
                    pfbs()
             )
      )
    }
  })
  observeEvent(input$task_resp_some_ab, {
    if(time_elapsed() > 1){
      rv$task_ttr[2] <- time_elapsed()
      rv$task_response[2] <- paste(input$task_resp_some_ab, collapse = ", ")
      loggit("INFO", "Task 2, somewhat important clusters ab response entered.",
             paste0("Response: ", rv$task_response[2],
                    ". ttr: ", rv$task_ttr[2], ".",
                    pfbs()
             )
      )
    }
  })
  observeEvent(input$task_response, {
    if(time_elapsed() > 1){
      rv$task_ttr[3] <- time_elapsed()
      rv$task_response[3] <- paste(input$task_response, collapse = ", ")
      loggit("INFO", "Task response interacted with",
             paste0("Response: ", rv$task_response,
                    ". ttr: ", rv$task_ttr, ".",
                    pfbs()
             )
      )
    }
  })

  
  ### Obs interaction count -----
  observeEvent(
    {
      input$x_axis
      input$y_axis
      input$manip_var_nm
    }, {
      rv$pca_inter <- rv$pca_inter + 1L
    }
  )
  observeEvent(
    {
      input$manip_slider
      input$manip_var_nm
    }, {
      rv$radial_inter <- rv$radial_inter + 1L
    }
  )
  observeEvent(
    {
      input$task_resp
    }, {
      rv$resp_inter <- rv$resp_inter + 1L
    }
  )
  
  ##### Obs survey answers -----
  observeEvent(input$survey1, {
    if(time_elapsed() > 1){
      i <- 1
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey1
      loggit("INFO", "Survey 1 entered.",
             paste0("Response: ", rv$task_response[i],
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
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
                    ". ttr: ", rv$task_ttr[i], ".",
                    pfbs()
             )
      )
    }
  })
  
  ### Obs next page button -----
  observeEvent(input$next_pg_button, {
    if((rv$stopwatch > 1L & do_log == TRUE) | do_log == FALSE){
      cat(paste0("in loop, top --", rv$pg) )
      task_resp  <- task_resp()
      task_ans   <- task_ans()
      task_score <- task_score()
      
      ### _Training evaluation -----
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
      
      ##### _rv$resp_tbl -----
      ## Write reponses and ttr to resp_tbl
      if(section_nm() %in% c("task", "training")){
        ins_row <- which(rv$resp_tbl$factor  == factor_nm() &
                           rv$resp_tbl$block == block())[1L]
        
        ## Is response concerning?
        task_concern <- "no"
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
      ### _New page ----
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
      
      #if(rv$pg == survey_start_pg) shinyjs::hide("next_pg_button")
      
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
      loggit("INFO", paste0("Next page:"), pfbs())
      cat("bottom of loop -- ")
    }
    cat("after loop \n")
  })
  
  
  ### Obs save reponses button -----
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
  
  ### Obs browser -----
  observeEvent(input$browser, if(do_disp_dev_tools == TRUE){browser()})
  
  ### Obs timer -----
  observe({
    invalidateLater(1000, session)
    isolate({
      rv$timer     <- rv$timer - 1
      rv$stopwatch <- rv$stopwatch + 1
    })
    if(rv$timer < 0 & section_nm() == "task" & rv$timer_active == TRUE){
      rv$timer_active <- FALSE
      loggit("INFO", "Timer elapsed.",
             pfbs())
    }
  })
  
  
  ##### Outputs -----
  output$timer_disp <- renderText({
    if(section_nm() == "task"){ ## Disp timer counting down if on a task.
      if(rv$timer < 1){return("Time has expired, please enter your best guess and proceed.")
      } else {
        return(
          paste0("Time left: ", lubridate::seconds_to_period(rv$timer),
                 " of ", lubridate::seconds_to_period(task_time()))
        )
      }
    }
    if(section_nm() == "training" & block() != 6){ ## Disp timer Counting up if in training.
      return(paste0("Time elapsed this task: ", lubridate::seconds_to_period(rv$stopwatch)))
    }
  })
  
  ### Condition handling for ui coditionalPanels
  output$is_saved   <- reactive(if(is.null(rv$save_file)){0} else {1}) ## Control save_msg.
  output$pg         <- reactive(rv$pg)        ## For hiding ui next_task button
  output$section_nm <- reactive(section_nm()) ## For ui between sections
  output$factor_nm  <- reactive(factor_nm())  ## For sidebar inputs
  output$block      <- reactive(block())      ## For training ui
  output$section_pg <- reactive(section_pg()) ## For navigating training

  outputOptions(output, "is_saved",   suspendWhenHidden = FALSE) ## Eager evaluation for ui conditionalPanel
  outputOptions(output, "pg",         suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "section_nm", suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "factor_nm",  suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "block",      suspendWhenHidden = FALSE) ##  "
  outputOptions(output, "section_pg", suspendWhenHidden = FALSE) ## Eager evaluation for ui conditionalPanel
  
  ### General task outputs
  output$task_header    <- renderText(task_header())
  output$pca_plot       <- renderPlot({pca_plot()}, height = pca_height)
  output$grand_plot     <- renderPlotly({suppressWarnings(grand_plot())})
  output$radial_plot    <- renderPlot({radial_plot()}, height = radial_height)
  output$resp_tbl       <- renderTable({rv$resp_tbl})
  output$task_ans_ptile <- renderPrint({task_ans_ptile()})
  output$task_ans       <- renderPrint({task_ans()})
  output$task_score     <- renderPrint({task_score()})
  
  ### Dev msg -----
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
          pfbs()
      )
    }
  })
} ## End server function

### Combine as shiny app.
shinyApp(ui = ui, server = server)



##### DEPRICATING: -----
### old resp_tbl structure deleted.

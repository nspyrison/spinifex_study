source('global_static.r', local = TRUE)


##### Server function, for shiny app
server <- function(input, output, session) {
  loggit("INFO", "app has started", "spinifex_study")
  
  ### Initialization and reactives -----
  rv                   <- reactiveValues()
  rv$pg_num            <- 1
  rv$timer             <- 120
  rv$timer_active      <- TRUE
  rv$task_responses    <- NULL
  rv$task_durations    <- NULL
  rv$save_file         <- NULL
  rv$ans_tbl           <- NULL
  rv$training_passes   <- FALSE
  rv$training_attempts <- 1
  rv$factor_num        <- 1
  
  ##### Start reactives
  p1 <- reactive({ ncol(s_dat[[1]]) })
  p2 <- reactive({ ncol(s_dat[[2]]) })
  p3 <- reactive({ ncol(s_dat[[3]]) })
  k1 <- reactive({
    n_cls <- length(unique(attributes(s_dat[[1]])$cluster)) 
    2 * (n_cls - 1)
  })
  k2 <- reactive({
    n_cls <- length(unique(attributes(s_dat[[2]])$cluster))
    2 * (n_cls - 1)
  })
  k3 <- reactive({
    n_cls <- length(unique(attributes(s_dat[[3]])$cluster))
    2 * (n_cls - 1)
  })
  k_sims <- reactive({ k1() + k2() + k3() })
  this_p <- reactive({ ncol(task_dat()) })
  this_k <- reactive({ length(unique(attributes(task_dat())$cluster)) })
  ui_section <- reactive({ # text name of section
    if (rv$pg_num == 1) {return("intro")}
    if (rv$pg_num %in% training_start:(task_start - 1) ) {return("training")}
    if (rv$pg_num %in% task_start:(survey_start - 1) ) {return("task")}
    if (rv$pg_num == survey_start) {return("survey")} 
    return("passed survey, need to cap pg_num")
  })
  section_pg_num <- reactive({ # ~page num of this section.
    if (ui_section() == "training"){
      return(rv$pg_num - (training_start - 1))
    }
    if (ui_section() == "task"){
      return(rv$pg_num - (task_start - 1))
    }
    return(1) # dummy 1, NA and 999 cause other issues.
  })
  factor <- reactive({ # ~ PCA, gtour, mtour
    if (ui_section() != "task") {return(NA)
      } else {# is task
        factor_num <- 1 + (section_pg_num() %/% (n_blocks * n_reps))
        return(this_factor_order[factor_num])
      }
  })
  block_num <- reactive({ # 1:2
    if (ui_section() == "training") {return(section_pg_num())
    } else {
      return(1 + (section_pg_num() - 1) %/% n_reps)
    }
  })
  rep_num <- reactive({ # 1:3
    if (ui_section() == "training") {return(section_pg_num())}
    if (section_pg_num() - (n_reps * (block_num() - 1)) <= 0) {stop("check rep_num() it's <= 0.")}
    return(section_pg_num() - (n_reps * (block_num() - 1)))
  })
  blockrep <- reactive({ # n1, n2, d1, d2
    paste0(s_block_id[block_num()], rep_num())
  })
  task_dat <- reactive({ # simulation df with attachments.
    if (ui_section() == "training") {sim_intro
    } else {
      ret <- tourr::rescale(s_dat[[rep_num()]])
      colnames(ret) <- paste0("V", 1:ncol(ret))
      return(ret) 
    }
  })

  
  
  ### PCA plot reactive -----
  pca_plot <- reactive({
    if (rv$timer_active | ui_section() == "training") {
      # data init
      dat_std <- task_dat()
      cluster <- attributes(dat_std)$cluster
      
      # render init
      pal <- "Dark2"
      axes_position <- "center"
      USE_AXES <- TRUE
      USE_AES  <- TRUE
      if (block_num() == 1) {
        USE_AXES <- FALSE
        if(rv$training_passes == FALSE) { # During training
          USE_AES  <- FALSE
        } 
      }
      pca <- prcomp(dat_std)
      if (block_num() == 2) { # 2nd block, change sign of the basis.
        pca_x <- as.matrix(dat_std) %*% (-1 * pca$rotation)
        pca_x <- data.frame(2 * (tourr::rescale(pca_x) - .5))
        pca_rotation <- set_axes_position(data.frame(t(-1 * pca$rotation)),
                                          axes_position)
      } else { # not 2nd block, leave the signs of the basis
        pca_x <- data.frame(2 * (tourr::rescale(pca$x) - .5))
        pca_rotation <- set_axes_position(data.frame(t(pca$rotation)),
                                          axes_position)
      }
      
      pca_pct_var <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
      pca_x_axis <- input$x_axis
      pca_y_axis <- input$y_axis
      v_x_axis <- paste0("V", substr(pca_x_axis,3,3))
      v_y_axis <- paste0("V", substr(pca_y_axis,3,3))
      x_lab <- paste0(input$x_axis, " (",
                      pca_pct_var[as.integer(substr(pca_x_axis,3,3))], "% Var)")
      y_lab <- paste0(input$y_axis, " (",
                      pca_pct_var[as.integer(substr(pca_y_axis,3,3))], "% Var)")
      
      angle <- seq(0, 2 * pi, length = 360)
      circ  <- set_axes_position(data.frame(x = cos(angle), y = sin(angle)),
                                axes_position)
      zero  <- set_axes_position(0, axes_position)
      
      
      ### ggplot2
      ret <- ggplot()
      if (USE_AES == FALSE){
        # data points
        ret <- ret + 
          geom_point(pca_x, 
                     mapping = aes(x = get(pca_x_axis), y = get(pca_y_axis)), 
                     size = 3)
      } else { # if USE_AES == TRUE then apply more aes.
        ret <- ret +
          geom_point(pca_x, 
                     mapping = aes(x = get(pca_x_axis), y = get(pca_y_axis),
                                   color = cluster, 
                                   fill = cluster, 
                                   shape = cluster), 
                     size = 3)
      }
      if (USE_AXES == TRUE) { # if USE_AXES == TRUE then draw axes
        # axis segments
        ret <- ret +
          geom_segment(pca_rotation,
                       mapping = aes(x = get(v_x_axis), xend = zero,
                                     y = get(v_y_axis), yend = zero),
                       size = .3, colour = "red") +
          # axis label text
            geom_text(pca_rotation,
                      mapping = aes(x = get(v_x_axis),
                                    y = get(v_y_axis),
                                    label = colnames(pca_rotation)),
                      size = 6, colour = "red", fontface = "bold",
                      vjust = "outward", hjust = "outward") +
            # Cirle path
            geom_path(circ,
                      mapping = aes(x = x, y = y),
                      color = "grey80", size = .3, inherit.aes = F)
        }
      
      # Options 
      ret <- ret + theme_minimal() +
        theme(aspect.ratio = 1) +
        scale_color_brewer(palette = pal) +
        theme(panel.grid.major = element_blank(), # no grid lines
              panel.grid.minor = element_blank(), # no grid lines
              axis.text.x = element_blank(),      # no axis marks
              axis.text.y = element_blank(),      # no axis marks
              axis.title.x = element_text(size = 22, face = "bold"),
              axis.title.y = element_text(size = 22, face = "bold"),
              legend.box.background = element_rect(),
              legend.title = element_text(size = 18, face = "bold"),
              legend.text  = element_text(size = 18, face = "bold")
        ) +
        labs(x = x_lab, y = y_lab)
      
      return(ret)
    } else {return()}
  })
  
  ### Grand tour plot reactive -----
  gtour_plot <- reactive({ 
    if (rv$timer_active | ui_section() == "training") {
      # data init
      dat_std <- task_dat()
      cluster <- attributes(dat_std)$cluster
      
      # tour init
      angle <- .1
      fps   <- 3
      max_frames <- 30 * fps # 90 frame at 30 sec @ fps = 3
      set.seed(123) # if tourr starts using seeds
      # # start basis is pca[, 1:2]
      # basis <- prcomp(dat_std)$rotation[, 1:2]
      # tpath <- save_history(dat_std, tour_path = grand_tour(), max = 8)
      # str(tpath)
      # # prepend the basis matrix, pc1:2, reformat for tourr
      # tpath <- abind::abind(basis, tpath, along = 3)
      # attr(tpath, "data") <- dat_std
      # structure(tpath, class = "history_array")
      # str(tpath)
      ##TODO: pca basis not working (above). grand tour without PCA init below      
      
      tpath <- save_history(dat_std, tour_path = grand_tour(), max = 8)
      
      full_path <- tourr::interpolate(basis_set = tpath, angle = angle)
      attr(full_path, "class") <- "array"
      max_frames <- min(c(max_frames, dim(full_path)[3]))
      full_path <- full_path[, , 1:max_frames]
      
      tour_df <- array2df(array = full_path, data = dat_std)
      # tour_df is tour in long form 
      
      # render init
      pal <- "Dark2"
      axes_position <- "center"
      USE_AXES <- TRUE
      USE_AES  <- TRUE
      if (block_num() == 1) {
        USE_AXES <- FALSE
        if(rv$training_passes == FALSE) { # During training
          USE_AES  <- FALSE
        } 
      }
      if (USE_AXES == FALSE) {axes_position = "off"}
      angle <- seq(0, 2 * pi, length = 360)
      circ  <- set_axes_position(data.frame(x = cos(angle), y = sin(angle)),
                                 axes_position)
      zero  <- set_axes_position(0, axes_position)
      
      ### ggplot2
      basis_df <- tour_df$basis_slides
      basis_df[, 1:2] <- set_axes_position(tour_df$basis_slides[, 1:2], axes_position)
      data_df  <- tour_df$data_slides
      cluster  <- rep(cluster, max_frames)
      
      gg <- ggplot()
      if (USE_AES == FALSE){
        # data points
        gg <- gg + 
          geom_point(data_df, 
                     mapping = aes(x = x, y = y, frame = slide), 
                     size = 1.7) # smaller size for plotly
      } else { # if USE_AES == TRUE then apply more aes.
        gg <- gg +
          geom_point(data_df, 
                     mapping = aes(x = x, y = y, frame = slide, 
                                   color = cluster, 
                                   fill  = cluster, 
                                   shape = cluster), 
                     size = 1.7) # smaller size for plotly
      }
      if (USE_AXES == TRUE) { # iF USE_AXES == TRUE then draw axes.
        # axis segments
        gg <- gg +
          geom_segment(basis_df,
                       mapping = aes(x = x, xend = zero,
                                     y = y, yend = zero,
                                     frame = slide),
                       size = .3, colour = "red") +
          # axis label text
          geom_text(basis_df,
                    mapping = aes(x = x,
                                  y = y,
                                  frame = slide,
                                  label = lab),
                    size = 6, colour = "red", fontface = "bold",
                    vjust = "outward", hjust = "outward") +
          # Cirle path
          geom_path(circ,
                    mapping = aes(x = x, y = y),
                    color = "grey80", size = .3, inherit.aes = F)
      }
      
      # Options 
      gg <- gg + theme_minimal() +
        theme(aspect.ratio = 1) +
        scale_color_brewer(palette = pal) +
        scale_fill_brewer(palette = pal) +
        theme(panel.grid.major = element_blank(), # no grid lines
              panel.grid.minor = element_blank(), # no grid lines
              axis.text.x = element_blank(),      # no axis marks
              axis.text.y = element_blank(),      # no axis marks
              axis.title.x = element_blank(),     # no axis titles for gtour
              axis.title.y = element_blank(),     # no axis titles for gtour
              legend.box.background = element_rect(),
              legend.title = element_text(size = 18, face = "bold"),
              legend.text  = element_text(size = 18, face = "bold")
        ) # end of ggplot2 work 
      
      ### plotly
      ggp <- plotly::ggplotly(p = gg, tooltip = "none") 
      ggp <- plotly::animation_opts(p = ggp, frame = 1 / fps * 1000, 
                                    transition = 0, redraw = FALSE)
      ggp <- plotly::layout(
        ggp, showlegend = T, yaxis = list(showgrid = F, showline = F),
        xaxis = list(scaleanchor = "y", scaleratio = 1, showgrid = F, showline = F),
        legend = list(x = 0.8, y = 0.7, title = "cluster")
      )
      
      return(ggp)
    } else {return()}
  })
  
  
  ### Manual tour plot reactive -----
  mtour_plot <- reactive({
    if (rv$timer_active | ui_section() == "training") {
      if (is.null(rv$curr_basis)) {stop("rv$curr_basis not found while calling mtour_plot()")}
      # data init
      dat_std <- task_dat()
      cluster <- attributes(dat_std)$cluster
      if (input$manip_var == "<none>") {m_var <- 1
      } else {m_var <- which(colnames(dat) == input$manip_var)}
      
      
      ## TODO: expand oblique frame for like aes.
      ret <- oblique_frame(data      = dat_std,
                           basis     = rv$curr_basis,
                           manip_var = m_var,
                           theta     = 0, # perform rotation when setting rv$curr_basis
                           phi       = 0,
                           col       = cluster,
                           pch       = cluster,
                           axes      = "center",
                           alpha     = 1
      )
      
      return(ret)
    } else {return()}
  })
  
  
  ### Response table reactive -----
  ans_tbl <- reactive({
    col_factor <- 
      c(
        rep(this_factor_order[1], n_blocks * n_reps), # across factors
        rep(this_factor_order[2], n_blocks * n_reps),
        rep(this_factor_order[3], n_blocks * n_reps),
        rep(NA, n_survey_questions)                   # survey
      )
    col_blockrep <- 
      c(
        rep(
          c(s_blockrep_id[1:3],          # block 1
            rep(s_blockrep_id[4], k1()), # block 2
            rep(s_blockrep_id[5], k2()),
            rep(s_blockrep_id[6], k3())
          ), 
          n_factors                      # across factors
        ), 
        paste0("survey", 1:10)           # survey
      )                
    q_id1 <- paste0("cl", letters[rep(1:((k1() / 2) + 1), each = 2)], "_", c("very", "some"))
    q_id2 <- paste0("cl", letters[rep(1:((k2() / 2) + 1), each = 2)], "_", c("very", "some"))
    q_id3 <- paste0("cl", letters[rep(1:((k3() / 2) + 1), each = 2)], "_", c("very", "some"))
    col_q_id <- 
      c(
        rep(
          c(rep(NA, 3),         # block 1
            q_id1, q_id2, q_id3 # block 2
          ), 
          n_factors             # across factors
        ), 
        rep(NA, 10)             # survey
      )
    col_sim_id   <- 
      c(
        rep(
          c(sim1_num, sim2_num, sim3_num, # block 1
            rep(sim1_num, k1()),          # block 2
            rep(sim2_num, k2()),
            rep(sim3_num, k3())
          ),
          n_factors                       # across factors
        ),
        rep(NA, 10)                       # survey
      )
    col_question <- 
      c(
        rep(
          c(rep(s_block_questions[1], n_reps),   # block 1
            rep(s_block_questions[2], k_sims()), # block 2
          ),
          n_factors                              # across factors
        ),
        s_survey_questions                       # survey
      ) 
    col_response <- col_duration <- 
      c(
        rep(
          c(rep(NA, n_reps),  # block 1
            rep(NA, k_sims()) # block 2
          ),
          n_factors           # across factors
        ),
        rep(NA, 10)           # survey
      )
    
    data.frame(factor   = col_factor,
               blockrep = col_blockrep,
               q_id     = col_q_id,
               sim_id   = col_sim_id,
               question = col_question,
               response = col_response,
               duration = col_duration)
  })
  ##### End of reactives
  
  
  ##### Start observes
  ### Obs update axis choices -----
  observeEvent(task_dat(), { # Init axis choices when data changes
    if (rv$timer_active | ui_section() == "training") {
      p <- ncol(task_dat())
      choices <- paste0("PC", 1:p)
      updateRadioButtons(session, "x_axis", choices = choices, selected = "PC1")
      updateRadioButtons(session, "y_axis", choices = choices, selected = "PC2")
      loggit("INFO", "task_dat() changed.", "updated axes choices.")
    }
  })
  # Bump x_axis when set to the same as y_axis
  observeEvent(input$x_axis, { 
    if (input$x_axis == input$y_axis) {
      p <- ncol(task_dat())
      choices <- paste0("PC", 1:p)
      opts <- choices[!choices %in% input$x_axis]
      x_axis_out <- sample(opts, 1)
      updateRadioButtons(session, "x_axis", choices = choices, selected = x_axis_out)
      loggit("INFO", 
             paste0("x_axis set to ", input$x_axis , ", same as y_axis."), 
             paste0("x_axis bumped to ", x_axis_out, "."))
    }
  })
  # Bump y_axis when set to the same as x_axis
  observeEvent(input$y_axis, {
    if (input$x_axis == input$y_axis) {
      p <- ncol(task_dat())
      choices <- paste0("PC", 1:p)
      opts <- choices[!choices %in% input$x_axis]
      y_axis_out <- sample(opts, 1)
      updateRadioButtons(session, "y_axis", choices = choices, selected = sample(opts, 1))
      loggit("INFO", 
             paste0("y_axis set to ", input$y_axis , "same as x_axis."), 
             paste0("y_axis bumped to ", y_axis_out, "."))
    }
  })
  
  ### Obs mtour basis ----- 
  observeEvent({ 
    task_dat()
    input$x_axis
    input$y_axis
  },
  {
    dat_std <- task_dat()
    pca <- prcomp(dat_std)
    x <- input$x_axis
    y <- input$y_axis
    x_num <- as.integer(substr(x, nchar(x), nchar(x)))
    y_num <- as.integer(substr(y, nchar(y), nchar(y)))
    rv$curr_basis <- pca$rotation[, c(x_num, y_num)]
    loggit("INFO", 
           paste0("New basis set (from task_dat/axes) "), 
           paste0(print(rv$curr_basis), "x_axis: ", x, ", y_axis: ", y))
  })
  
  ### Obs mtour slider ----
  observeEvent(input$manip_slider, {
    if (input$manip_var != "<none>") {
      theta <- phi <- NULL
      mv_sp <- create_manip_space(rv$curr_basis, manip_var_num())[manip_var_num(), ]
      if ("Radial" == "Radial") { # Fixed to "Radial" # input$manip_type == "Radial"
        theta <- atan(mv_sp[2] / mv_sp[1])
        phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
        phi <- (acos(input$manip_slider) - phi_start) * - sign(mv_sp[1])
      }
      ret <- oblique_basis(basis = rv$curr_basis, manip_var = manip_var_num(),
                           theta = theta, phi = phi)
      row.names(ret) <- colnames(task_dat())
      
      rv$curr_basis <- ret
      loggit("INFO", paste0("Slider value changed: ", input$manip_slider),
             paste0("rv$curr_basis updated: ", rv$curr_basis))
    }
  })
  
  ### Obs mtour update manip_var choices -----
  observeEvent(task_dat(), { # Init manip_var choices on data change.
    if (rv$timer_active | ui_section() == "training") {
      these_colnames <- colnames(task_dat())
      updateSelectInput(session, "manip_var", choices = these_colnames, 
                        selected = these_colnames[1])
      loggit("INFO", paste0("task_dat() changed., input$manip_var updated."),
             paste0("input$manip_var updated."))
    }
  })
  
  ### Obs response and duration -----
  ##### Block 1 response & duration
  observeEvent(input$blk1_ans, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[1] <- input$blk1_ans
      rv$task_durations[1] <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 1 entered.", 
             paste0("Response: ", rv$task_responses[1], 
                    ". Duration: ", rv$task_durations[1], "."))
    }
  })
  ##### Block 2 responses & duration
  observeEvent(input$blk2_ans1, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[1] <- input$blk2_ans1
      rv$task_durations[1] <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 2, input 1 entered.", 
             paste0("Response: ", rv$task_responses[1], 
                    ". Duration: ", rv$task_durations[1], "."))
    }
  })
  observeEvent(input$blk2_ans2, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[2]  <- input$blk2_ans2
      rv$task_durations[2]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 2, input 2 entered.", 
             paste0("Response: ", rv$task_responses[2], 
                    ". Duration: ", rv$task_durations[2], "."))
    }
  })
  observeEvent(input$blk2_ans3, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[3]  <- input$blk2_ans3
      rv$task_durations[3]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 2, input 3 entered.", 
             paste0("Response: ", rv$task_responses[3], 
                    ". Duration: ", rv$task_durations[3], "."))
    }
  })
  observeEvent(input$blk2_ans4, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[4]  <- input$blk2_ans4
      rv$task_durations[4]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 2, input 4 entered.", 
             paste0("Response: ", rv$task_responses[4], 
                    ". Duration: ", rv$task_durations[4], "."))
    }
  })
  observeEvent(input$blk2_ans5, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[5]  <- input$blk2_ans5
      rv$task_durations[5]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 2, input 5 entered.", 
             paste0("Response: ", rv$task_responses[5], 
                    ". Duration: ", rv$task_durations[5], "."))
    }
  })
  observeEvent(input$blk2_ans6, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[6]  <- input$blk2_ans6
      rv$task_durations[6]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 2, input 6 entered.", 
             paste0("Response: ", rv$task_responses[6], 
                    ". Duration: ", rv$task_durations[6], "."))
    }
  })
  observeEvent(input$blk2_ans7, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[7]  <- input$blk2_ans7
      rv$task_durations[7]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 2, input 7 entered.", 
             paste0("Response: ", rv$task_responses[7], 
                    ". Duration: ", rv$task_durations[7], "."))
    }
  })
  observeEvent(input$blk2_ans8, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[8]  <- input$blk2_ans8
      rv$task_durations[8]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 2, input 8 entered.", 
             paste0("Response: ", rv$task_responses[8], 
                    ". Duration: ", rv$task_durations[8], "."))
    }
  })
  observeEvent(input$blk2_ans9, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[9]  <- input$blk2_ans9
      rv$task_durations[9]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 2, input 9 entered.", 
             paste0("Response: ", rv$task_responses[9], 
                    ". Duration: ", rv$task_durations[9], "."))
    }
  })
  observeEvent(input$blk2_ans10, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[10] <- input$blk2_ans10
      rv$task_durations[10] <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 2, input 10 entered.", 
             paste0("Response: ", rv$task_responses[10], 
                    ". Duration: ", rv$task_durations[10], "."))
    }
  })
  observeEvent(input$blk2_ans11, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[11] <- input$blk2_ans11
      rv$task_durations[11] <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 2, input 11 entered.", 
             paste0("Response: ", rv$task_responses[11], 
                    ". Duration: ", rv$task_durations[11], "."))
    }
  })
  observeEvent(input$blk2_ans12, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[12] <- input$blk2_ans12
      rv$task_durations[12] <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 2, input 12 entered.", 
             paste0("Response: ", rv$task_responses[12], 
                    ". Duration: ", rv$task_durations[12], "."))
    }
  })
  
  
  ### Obs next page button -----
  observeEvent(input$next_pg_button, {
    ### ON LAST PAGE:
    # if <on last task> {<do nothing>}
    if (rv$pg_num >= survey_start){ return() }
    # Init rv$ans_tbl <- ans_tbl() on at the task section
    if (rv$pg_num + 1 == task_start){ rv$ans_tbl <- ans_tbl() }
    
    # If training section, evaluate response 
    if (ui_section() == "training" & rv$training_passes == FALSE) {
      # Evaluate training block 1
      if (block_num() == 1) {
        ## TODO: removed too many training atempts blocks.
        # if (rv$training_attempts >= 3) { # Too many attempts msg
        #   output$plot_msg <- renderText("<h3><span style='color:red'>Please see the proctor for additional instruction.</span></h3>") 
        #   rv$training_attempts <- rv$training_attempts + 1
        #   return()
        # }
        response <- input$blk1_ans
        ans <- length(attr(sim_intro, "ncl"))
        
        if (response - ans >= 2){ # >= 2 clusters too high, retry
          output$plot_msg <- renderText("<h3><span style='color:red'>That seems a little high, make sure to check from multiple perspectives and give it another shot.</span></h3>") 
          rv$training_attempts <- rv$training_attempts + 1
          return()
        }
        if (response - ans <= -2){ # <= 2 clusters too low, retry
          output$plot_msg <- renderText("<h3><span style='color:red'>That seems a little low, make sure to check from multiple perspectives and give it another shot.</span></h3>") 
          rv$training_attempts <- rv$training_attempts + 1
          return()
        }
        if (abs(response - ans) == 1){ # within 1 cluster, msg, passes
          output$plot_msg <- renderText(
            paste0("<h3><span style='color:red'>Close, there are actually ", ans, " clusters in the data, but you have the right idea.</span></h3>") 
          )
          rv$training_passes <- TRUE
            return()
        }
        if (response == ans){ # exact, msg, pass
          output$plot_msg <- renderText("<h3><span style='color:red'>That's correct, great job!</span></h3>") 
          rv$training_passes <- TRUE
          return()
        }
      }
      ## TODO: This removed evaluation of the training for blocks 2.
      # if (block_num() == 2) {
      #   output$plot_msg <- renderText("<h3><span style='color:red'>Training block 2 answer TBD.</span></h3>") 
      #   rv$training_passes <- TRUE
      #   return()
      # }
    }
    
    # If task section, write reponses and duration to ans_tbl
    if (ui_section() == "task") {
      ins_row <- which(rv$ans_tbl$blockrep == blockrep())[1] # first row of this blockrep.
      ins_nrows <- length(rv$task_responses) - 1
      rv$ans_tbl[ins_row:(ins_row + ins_nrows), 6] <- rv$task_responses
      rv$ans_tbl[ins_row:(ins_row + ins_nrows), 7] <- rv$task_durations
    }
    
    ### NEW PAGE:
    rv$pg_num <- rv$pg_num + 1 
    # Reset responses, duration, and timer for next task
    output$plot_msg <- renderText("")
    rv$task_responses <- NULL
    rv$task_durations <- NULL
    rv$timer <- 120
    rv$timer_active <- TRUE
    rv$training_passes <- FALSE
    rv$training_attempts <- 1
    
    cat("new page:", "pg_num:", rv$pg_num, "ui_section:", ui_section(), 
        "section_pg_num:", section_pg_num(), "factor:", factor(), 
        "block_num:", block_num(), "rep_num:", rep_num(), "\n")
    
    # Clear task response
    if (ui_section() == "task") {
      if (block_num() == 1) { # reset to same settings.
        updateNumericInput(session, "blk1_ans", "",
                           value = 0, min = 0, max = 10)
      }
    }
    
    # Set structure for responses and durations
    n_rows <- 0 
    if (ui_section() == "task") {
      if (block_num() == 1){n_rows <- 1} 
      if (block_num() == 2){n_rows <- 2 * (this_k() - 1)}
    }
    if (ui_section() == "survey") {n_rows <- n_survey_questions}
    
    rv$task_responses <- rep("default", n_rows)
    rv$task_durations <- rep("default", n_rows)
    loggit("INFO", "Next page: ", 
           paste0("rv$pg_num: ", rv$pg_num, 
                  ". ui_section(): ", ui_section(),
                  ". blockrep(): ", blockrep(), 
                  "."))
  })
  
  ### Obs save reponses button -----
  observeEvent(input$save_ans, {
    # Write survey responses to rv$ans_tbl
    ins_nrows <- n_survey_questions - 1
    ins_row <- nrow(rv$ans_tbl) - ins_nrows
    rv$ans_tbl[ins_row:(ins_row + ins_nrows), 5] <- rv$task_responses
    rv$ans_tbl[ins_row:(ins_row + ins_nrows), 6] <- rv$task_durations
    
    # Write rv$ans_tbl to .csv file.
    df <- rv$ans_tbl
    if (!is.null(rv$save_file)){ # if save already exists 
      save_msg <- paste0("<h3><span style='color:red'>Reponses already saved as ", 
                         rv$save_file, ".</span></h3>")
      output$save_msg <- renderText(save_msg)
      loggit("INFO", "Save button (Previously saved): ", 
             paste0("save_msg: ", save_msg,  "."))
      return()
    }
    
    # Do the actual saving
    save_base <- paste0("response_table_factorid", this_factor_id, "_")
    save_num  <- 1
    save_name <- sprintf(paste0(save_base, "%03d"), save_num)
    save_file <- paste0(save_name, ".csv")
    while (file.exists(save_file)){ # set the correct file number to use
      save_name <- sprintf(paste0(save_base, "%03d"), save_num)
      save_file <- paste0(save_name, ".csv")
      save_num  <- save_num + 1
    }
    assign(save_name, df)
    write.csv(get(save_name), file = save_file, row.names = FALSE)
    rv$save_file <- save_file
    
    save_msg <- paste0("<h3><span style='color:red'>Reponses saved as ", save_file, 
                       ". Thank you for participating!</span></h3>")
    output$save_msg <- renderText(save_msg)
    
    loggit("INFO", "Save button: ", 
           paste0("rv$save_file: ", rv$save_file, 
                  ". save_msg: ", save_msg,
                  "."))
    return()
  })
  
  ### Obs browser -----
  observeEvent(input$browser, {browser()})
  
  ### Obs timer -----
  observe({
    if (ui_section() == "task") {
      invalidateLater(1000, session)
      isolate({
        rv$timer <- rv$timer - 1
        if(rv$timer < 1 & rv$timer_active == TRUE){
          rv$timer_active <- FALSE
          loggit("INFO", "timer elapsed.", 
                 paste0("On rv$pg_num: ", rv$pg_num, "."))
        }
      })
    }
  })

  
  ##### Outputs -----
  output$timer_disp <- renderText({
    if (ui_section() == "task") { # disp timer if not an intro page.
      if (rv$timer < 1) {return("Time has expired, please enter your best guess and proceed.")
      } else {paste0("Time left: ", lubridate::seconds_to_period(rv$timer))}
    } else {return()}
  })
  
  ### Controls ui coditionalPanels 
  output$is_saved   <- reactive(if (is.null(rv$save_file)) {0} else {1}) # control save_msg.
  output$pg_num     <- reactive(rv$pg_num)    # for hiding ui next_task button
  output$ui_section <- reactive(ui_section()) # for ui between sections
  output$factor     <- reactive(factor())     # for sidebar inputs
  output$block_num  <- reactive(block_num())  # for  titles, and response inputs
  output$rep_num    <- reactive(rep_num())    # for  training ui
  outputOptions(output, "is_saved",   suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  outputOptions(output, "pg_num",     suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  outputOptions(output, "ui_section", suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  outputOptions(output, "factor",     suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  outputOptions(output, "block_num",  suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  outputOptions(output, "rep_num",    suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  ### training outputs
  output$training_header_text <- renderText(training_header_text[block_num()])
  output$training_top_text    <- renderText(training_top_text[block_num()])
  ### general task outputs
  output$pca_plot   <- renderPlot({pca_plot()}, height = 640) 
  output$gtour_plot <- renderPlotly({gtour_plot()})
  output$mtour_plot <- renderPlotly({mtour_plot()})
  output$ans_tbl    <- renderTable({rv$ans_tbl})
  
  # output$blk1_ans defined in global_*.r
  ### Block 2 inputs, importance of var on cl seperation -----
  output$blk2Inputs <- renderUI({
    dat_std <- task_dat()
    p <- ncol(dat_std)                               # num var
    k <- length(unique(attributes(dat_std)$cluster)) # num of clusters
    q <- (2 * (k - 1))                               # num total questions
    
    lapply(1:q, function(this_q){
      this_k_letter <- letters[rep(1:(k - 1), each = 2)][this_q]
      this_q_txt    <- c("Very", "Somewhat")[rep(1:(k - 1),2)[this_q]]
      this_q_id     <- tolower(substr(this_q_txt, 1, 4))
      
      checkboxGroupInput(inputId = paste0("blk2_ans_cl", this_k_letter, "_", this_q_id),
                         label   = paste0(this_q_txt, " important for distinguishing cluster '", this_k_letter, "'"),
                         choices = paste0("V", 1:p),
                         inline  = TRUE)
    })
  }) # close renderUI for blk2inputs
  
  
  ### Dev msg -----
  output$dev_msg <- renderPrint(cat("dev msg -- ", "\n",
                                    "ui_section()", ui_section(), "\n",
                                    "block_num(): ", block_num(), "\n",
                                    "rv$timer: ", rv$timer, "\n",
                                    "rv$pg_num: ", rv$pg_num, "\n",
                                    "rep_num(): ", rep_num(), "\n",
                                    "input$x_axis: ", input$x_axis, "\n",
                                    "rv$task_responses: ", rv$task_responses, "\n",
                                    "rv$task_durations: ", rv$task_durations, "\n",
                                    "eval input$x_axis: ", eval(input$x_axis), "\n",
                                    "eval input$y_axis: ", eval(input$y_axis), "\n",
                                    "input$save_ans: ", input$save_ans, "\n",
                                    "input$save_ans > 5: ", input$save_ans > 5, "\n",
                                    "rv$save_file: ", rv$save_file, "\n",
                                    "is.null(rv$save_file): ", is.null(rv$save_file), "\n",
                                    sep = ""))
  
  
  session$onSessionEnded(function(){
    cat("(onSessionEnded ran) \n")
    loggit("INFO", "app has stopped", "spinifex_study")
  })
}

### Combine as shiny app.
shinyApp(ui = ui, server = server)


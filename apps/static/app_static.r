source('global_static.r', local = TRUE)


##### Server function, for shiny app
server <- function(input, output, session) {
  loggit("INFO", "app has started", "spinifex_study")
  
  ### Initialization and reactives -----
  rv                   <- reactiveValues()
  rv$pg_num            <- 1
  rv$timer             <- 0
  rv$timer_active      <- TRUE
  rv$task_responses    <- NULL
  rv$task_durations    <- NULL
  rv$save_file         <- NULL
  rv$ans_tbl           <- NULL
  rv$training_aes      <- FALSE
  rv$second_training   <- FALSE
  rv$factor_num        <- 1
  rv$curr_basis        <- NULL
  
  ##### Start reactives
  p1 <- reactive({ ncol(s_dat[[1]]) })
  p2 <- reactive({ ncol(s_dat[[7]]) })
  p3 <- reactive({ ncol(s_dat[[13]]) })
  k1 <- reactive({
    n_cls <- length(unique(attributes(s_dat[[1]])$cluster)) 
    return(2 * (n_cls - 1))
  })
  k2 <- reactive({
    n_cls <- length(unique(attributes(s_dat[[7]])$cluster))
    return(2 * (n_cls - 1))
  })
  k3 <- reactive({
    n_cls <- length(unique(attributes(s_dat[[13]])$cluster))
    return(2 * (n_cls - 1))
  })
  k_sims <- reactive({ k1() + k2() + k3() })
  k_t1 <- reactive({
    n_cls1 <- length(unique(attributes(s_train[[1]])$cluster))
    return(2 * (n_cls1 - 1))
  })
  k_t2 <- reactive({
    n_cls2 <- length(unique(attributes(s_train[[2]])$cluster))
    return(2 * (n_cls2 - 1))
  })
  k_trains <- reactive({ k_t1() + k_t2() })
  this_p <- reactive({ ncol(task_dat()) })
  this_k <- reactive({ 
    2 * (length(unique(attributes(s_dat[[rep_num()]])$cluster)) - 1) 
  })
  this_cl <- reactive({ length(unique(attributes(s_dat[[rep_num()]])$cluster)) })
  
  ui_section <- reactive({ # text name of section
    if (rv$pg_num == 1) {return("intro")}
    if (rv$pg_num %in% training_start:(task_start - 1) ) {return("training")}
    if (rv$pg_num %in% task_start:(survey_start - 1) ) {return("task")}
    if (rv$pg_num == survey_start) {return("survey")} 
    return("passed survey, need to cap pg_num")
  })
  section_pg_num <- reactive({ # ~page num of this section.
    if (ui_section() == "training"){ # Training: 1=ui, 2&3=blk1, 4&5=blk2, 6=splash
      return(rv$pg_num - (training_start - 1))
    }
    if (ui_section() == "task"){
      return(rv$pg_num - (task_start -1))
    }
    return(1) # dummy 1, NA and 999 cause other issues.
  })
  period_num <- reactive({1 + (section_pg_num() %/% (n_blocks * n_reps))})
  factor <- reactive({ # ~ PCA, gtour, mtour
    if (ui_section() != "task") {return("NONE")
      } else {# is task
        return(this_factor_order[period_num()])
      }
  })
  block_num <- reactive({ # 1:2
    if (ui_section() == "training") {
      return(c(0, 1, 1, 2, 2, 0)[section_pg_num()])
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
    if (ui_section() == "training") {
      ls <- paste0("training_", 
                   c(paste0(s_block_id[1], 1:2),
                     paste0(s_block_id[2], 1),
                     paste0(s_block_id[2], 2)
                   )
      )
      return(ls[section_pg_num() - 1])
    }
    return(paste0(s_block_id[block_num()], rep_num()))
  })
  task_dat <- reactive({ # simulation df with attachments.
    if (ui_section() == "training") {
      if (rep_num() %in% c(3, 5)) {return(s_train[[2]])
        } else return(s_train[[1]])
    } else {
      return(s_dat[[rep_num()]]) 
    }
  })
  manip_var_num <- reactive({ 
    if (input$manip_var == "<none>") {return(1)}
    return(which(colnames(task_dat()) == input$manip_var))
  }) 
  
  
  ### PCA plot reactive -----
  pca_height <- function(){
    if ((rv$timer_active & factor() == "pca") |
        (ui_section() == "training" & input$factor == "pca")) {
      return(640)
    } else return(1) 
  }
  pca_plot <- reactive({
    if ((rv$timer_active & factor() == "pca") |
        (ui_section() == "training" & input$factor == "pca")) {
      # data init
      dat <- task_dat()
      dat_std <- tourr::rescale(dat)
      cluster <- attributes(dat)$cluster
      
      # render init
      pal <- "Dark2"
      axes_position <- "center"
      USE_AXES <- TRUE
      USE_AES  <- TRUE
      if (block_num() == 1) {
        USE_AXES <- FALSE
        if(rv$training_aes == FALSE) { # During training
          USE_AES  <- FALSE
        } 
      }
      
      pca <- prcomp(dat_std)
      pca_x <- data.frame(2 * (tourr::rescale(pca$x) - .5))
      pca_rotation <- set_axes_position(data.frame(t(pca$rotation)),
                                        axes_position)
      
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
      gg <- ggplot()
      if (USE_AES == FALSE){
        # data points
        gg <- gg + 
          geom_point(pca_x, 
                     mapping = aes(x = get(pca_x_axis), y = get(pca_y_axis)), 
                     size = 3)
      } else { # if USE_AES == TRUE then apply more aes.
        gg <- gg +
          geom_point(pca_x, 
                     mapping = aes(x = get(pca_x_axis), y = get(pca_y_axis),
                                   color = cluster, 
                                   fill  = cluster, 
                                   shape = cluster), 
                     size = 3)
      }
      if (USE_AXES == TRUE) { # if USE_AXES == TRUE then draw axes
        # axis segments
        gg <- gg +
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
      gg <- gg + theme_minimal() +
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
      
      return(gg)
    } else {return()}
  })
  
  ### Grand tour plot reactive -----
  gtour_height <- function(){
    if ((rv$timer_active & factor() == "grand") | 
        (ui_section() == "training" & input$factor == "grand")) {
      return(640)
    } else return(1) 
  }
  gtour_plot <- reactive({ 
    if ((rv$timer_active & factor() == "grand") | 
        (ui_section() == "training" & input$factor == "grand")) {
      # data init
      dat <- task_dat()
      dat_std <- tourr::rescale(dat)
      cluster <- attributes(dat)$cluster
      
      # tour init
      angle <- .1
      fps   <- 6
      max_frames <- 90 # 90 frame for 15 sec @ fps = 6
      set.seed(123) # if tourr starts using seeds
      ##TODO: pca basis not working (below). perform grand tour without pca basis.
      # # start basis is pca[, 1:2]
      # basis <- prcomp(dat_std)$rotation[, 1:2]
      # tpath <- save_history(dat_std, tour_path = grand_tour(), max = 8)
      # str(tpath)
      # # prepend the basis matrix, pc1:2, reformat for tourr
      # tpath <- abind::abind(basis, tpath, along = 3)
      # attr(tpath, "data") <- dat_std
      # structure(tpath, class = "history_array")
      # str(tpath)
      ##TODO: pca basis not working (above). perform grand tour without pca basis.
      
      tpath <- save_history(dat_std, tour_path = grand_tour(), max = 8)
      
      full_path <- tourr::interpolate(basis_set = tpath, angle = angle)
      attr(full_path, "class") <- "array"
      max_frames <- min(c(max_frames, dim(full_path)[3]))
      full_path <- full_path[, , 1:max_frames]
      
      tour_df <- array2df(array = full_path, data = dat_std) # to long form df
      
      # render init
      pal <- "Dark2"
      axes_position <- "center"
      USE_AXES <- TRUE
      USE_AES  <- TRUE
      if (block_num() == 1) {
        USE_AXES <- FALSE
        if(rv$training_aes == FALSE) { # During training
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
      ## scaling in array2df not applying here...
      data_df[, 1:2] <- 2 * (tourr::rescale(data_df[, 1:2]) - .5)
      cluster <- rep(cluster, max_frames)
      
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
      ggp <- plotly::ggplotly(p = gg) #, tooltip = "none") 
      ggp <- plotly::animation_opts(p = ggp, frame = 1 / fps * 1000, 
                                    transition = 0, redraw = FALSE)
      ggp <- plotly::layout(
        ggp, showlegend = T, yaxis = list(showgrid = F, showline = F),
        xaxis = list(scaleanchor = "y", scaleratio = 1, showgrid = F, 
                     showline = F, autorange = TRUE, fixedrange = FALSE),
        #added for APP
        height = gtour_height(),
        yaxis = list(autorange = TRUE, fixedrange = FALSE), # suppose to rescale, I don't think it does.
        legend = list(x = 0.8, y = 0.7) # postition the title better
      )
      
      return(ggp)
    } else {return()}
  })
  
  
  ### Manual tour plot reactive -----
  mtour_height <- function() {
    if ((rv$timer_active  & factor() == "manual") | 
        (ui_section() == "training" & input$factor == "manual")) {
      return(800)
    } else return(1) 
  }
  mtour_plot <- reactive({
    if ((rv$timer_active  & factor() == "manual") | 
        (ui_section() == "training" & input$factor == "manual")) {
      if (is.null(rv$curr_basis)) {stop("rv$curr_basis not found while calling mtour_plot()")}
      # data init
      dat <- task_dat()
      dat_std <- tourr::rescale(dat)
      cluster <- attributes(dat)$cluster
      m_var <- NULL
      if (input$manip_var == "<none>") {m_var <- 1
      } else {m_var <- which(colnames(dat) == input$manip_var)}
      
      # render init
      pal <- "Dark2"
      axes_position <- "center"
      USE_AXES <- TRUE
      USE_AES  <- TRUE
      if (block_num() == 1) {
        USE_AXES <- FALSE
        axes_position <- "off"
        if(rv$training_aes == FALSE) { # During training
          USE_AES  <- FALSE
        } 
      }
      
      if (USE_AES == TRUE) {
        ret <- app_oblique_frame(data      = dat_std,
                                 basis     = rv$curr_basis,
                                 manip_var = m_var,
                                 theta     = 0, 
                                 phi       = 0,
                                 cluster   = cluster,
                                 axes      = axes_position
        )
      } else { # USE_AES == FALSE
        ret <- app_oblique_frame(data      = dat_std,
                                 basis     = rv$curr_basis,
                                 manip_var = m_var,
                                 theta     = 0, 
                                 phi       = 0,
                                 axes      = axes_position
        )
      }
      
      return(ret)
    } else {return()} # non-display conditions return nothing.
  })
  
  
  ### Response table reactive -----
  ans_tbl <- reactive({
    # init columns
    col_factor <- 
      c(
        rep("training", n_trainings + k_trains()),    # training
        rep(this_factor_order[1], n_reps + k_sims()), # across factors
        rep(this_factor_order[2], n_reps + k_sims()),
        rep(this_factor_order[3], n_reps + k_sims()),
        paste0("survey", 1:n_survey_questions)        # survey
      )
    col_blockrep <- 
      c(
        paste0("training_", 
               c(paste0(s_block_id[1], 1:2),             # training
                 rep(paste0(s_block_id[2], 1), k_t1()),
                 rep(paste0(s_block_id[2], 2), k_t2())
               )
        ),
        rep(
          c(s_blockrep_id[1:3],                  # block 1
            rep(s_blockrep_id[4], k1()),         # block 2
            rep(s_blockrep_id[5], k2()),
            rep(s_blockrep_id[6], k3())
          ), 
          n_factors                              # across factors
        ), 
        paste0("survey", 1:n_survey_questions)   # survey
      )
    q_id_t1 <- paste0("cl", letters[rep(1:((k_t1() / 2)), each = 2)], "_", c("very", "some"))
    q_id_t2 <- paste0("cl", letters[rep(1:((k_t2() / 2)), each = 2)], "_", c("very", "some"))
    q_id1 <- paste0("cl", letters[rep(1:((k1() / 2)), each = 2)], "_", c("very", "some"))
    q_id2 <- paste0("cl", letters[rep(1:((k2() / 2)), each = 2)], "_", c("very", "some"))
    q_id3 <- paste0("cl", letters[rep(1:((k3() / 2)), each = 2)], "_", c("very", "some"))
    col_q_id <- 
      c(
        rep(NA, n_trainings),              # training
        q_id_t1, q_id_t2,
        rep(
          c(rep(NA, n_reps),               # block 1
            q_id1, q_id2, q_id3            # block 2
          ), 
          n_factors                        # across factors
        ), 
        rep(NA, n_survey_questions)        # survey
      )
    sim_set <- c(101, 107, 113,       # block 1
                 rep(102, k1()),      # block 2
                 rep(108, k2()),
                 rep(114, k3())
    )
    col_sim_id <- 
      as.character(
        c(
          119:120,                    # training 1 
          rep(119, k_t1()),           # training 2
          rep(120, k_t2()),           
          sim_set,                    # across factors
          sim_set + 2,
          sim_set + 4,
          rep(NA, n_survey_questions) # survey
        )
      )
    col_question <- 
      c(
        rep(s_block_questions[1], n_trainings),
        rep(s_block_questions[2], k_trains()),  # training
        rep(
          c(rep(s_block_questions[1], n_reps),  # block 1
            rep(s_block_questions[2], k_sims()) # block 2
          ),
          n_factors                             # across factors
        ),
        s_survey_questions                      # survey
      ) 
    col_response <- col_duration <-
      c(
        rep(NA, n_trainings + k_trains()), # training
        rep(
          c(rep(NA, n_reps),               # block 1
            rep(NA, k_sims())              # block 2
          ),
          n_factors                        # across factors
        ),
        rep(NA, n_survey_questions)        # survey
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
    dat <- task_dat()
    dat_std <- tourr::rescale(dat)
    pca <- prcomp(dat_std)
    x <- input$x_axis
    y <- input$y_axis
    x_num <- as.integer(substr(x, nchar(x), nchar(x)))
    y_num <- as.integer(substr(y, nchar(y), nchar(y)))
    rv$curr_basis <- pca$rotation[, c(x_num, y_num)]
    loggit("INFO", 
           paste0("New basis set (from task_dat/axes) "), 
           paste0(rv$curr_basis, "x_axis: ", x, ", y_axis: ", y))
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
      loggit("INFO", paste0("task_dat() changed. input$manip_var choices updated."))
    }
  })
  
  ### Obs mtour update slider value -----
  observeEvent(
    {manip_var_num()
      task_dat()
      input$x_axis
      input$y_axis
    },
    {
      mv_sp <- create_manip_space(rv$curr_basis, manip_var_num())[manip_var_num(), ]
      phi_i <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
      this_val <- round(cos(phi_i), 1) # Rad
      updateSliderInput(session, "manip_slider", value = this_val)
      loggit("INFO", 
             paste0("New manip slider value (from task_dat/axes) "), 
             paste0("manip_slider: ", this_val))
    })
  
  ##### Obs block answers -----
  ### Block 1 response & duration
  observeEvent(input$blk1_ans, {
    if(rv$timer > 1) {
      rv$task_responses[1] <- input$blk1_ans
      rv$task_durations[1] <- as.integer(rv$timer)
      loggit("INFO", "Block 1 entered.", 
             paste0("Response: ", rv$task_responses[1], 
                    ". Duration: ", rv$task_durations[1], "."))
    }
  })
  ### Block 2 responses & duration
  observeEvent(input$blk2_ans_cla_very, {
    if(rv$timer > 1) {
      rv$task_responses[1] <- paste(input$blk2_ans_cla_very, collapse = ",")
      rv$task_durations[1] <- as.integer(rv$timer)
      loggit("INFO", "Block 2, cluster 'a' very important response entered.", 
             paste0("Response: ", rv$task_responses[1], 
                    ". Duration: ", rv$task_durations[1], "."))
    }
  })
  observeEvent(input$blk2_ans_cla_some, {
    if(rv$timer > 1) {
      rv$task_responses[2] <- paste(input$blk2_ans_cla_some, collapse = ",")
      rv$task_durations[2] <- as.integer(rv$timer)
      loggit("INFO", "Block 2, cluster 'a' somewhat important response entered.", 
             paste0("Response: ", rv$task_responses[2], 
                    ". Duration: ", rv$task_durations[2], "."))
    }
  })
  observeEvent(input$blk2_ans_clb_very, {
    if(rv$timer > 1) {
      rv$task_responses[3] <- paste(input$blk2_ans_clb_very, collapse = ",")
      rv$task_durations[3] <- as.integer(rv$timer)
      loggit("INFO", "Block 2, cluster 'b' very important response entered.", 
             paste0("Response: ", rv$task_responses[3], 
                    ". Duration: ", rv$task_durations[3], "."))
    }
  })
  observeEvent(input$blk2_ans_clb_some, {
    if(rv$timer > 1) {
      rv$task_responses[4] <- paste(input$blk2_ans_clb_some, collapse = ",")
      rv$task_durations[4] <- as.integer(rv$timer)
      loggit("INFO", "Block 2, cluster 'b' somewhat important response entered.", 
             paste0("Response: ", rv$task_responses[4], 
                    ". Duration: ", rv$task_durations[4], "."))
    }
  })
  observeEvent(input$blk2_ans_clc_very, {
    if(rv$timer > 1) {
      rv$task_responses[5] <- paste(input$blk2_ans_clc_very, collapse = ",")
      rv$task_durations[5] <- as.integer(rv$timer)
      loggit("INFO", "Block 2, cluster 'c' very important response entered.", 
             paste0("Response: ", rv$task_responses[5], 
                    ". Duration: ", rv$task_durations[5], "."))
    }
  })
  observeEvent(input$blk2_ans_clc_some, {
    if(rv$timer > 1) {
      rv$task_responses[6] <- paste(input$blk2_ans_clc_some, collapse = ",")
      rv$task_durations[6] <- as.integer(rv$timer)
      loggit("INFO", "Block 2, cluster 'c' somewhat important response entered.", 
             paste0("Response: ", rv$task_responses[6], 
                    ". Duration: ", rv$task_durations[6], "."))
    }
  })
  observeEvent(input$blk2_ans_cld_very, {
    if(rv$timer > 1) {
      rv$task_responses[7] <- paste(input$blk2_ans_cld_very, collapse = ",")
      rv$task_durations[7] <- as.integer(rv$timer)
      loggit("INFO", "Block 2, cluster 'd' very important response entered.", 
             paste0("Response: ", rv$task_responses[7], 
                    ". Duration: ", rv$task_durations[7], "."))
    }
  })
  observeEvent(input$blk2_ans_cld_some, {
    if(rv$timer > 1) {
      rv$task_responses[8] <- paste(input$blk2_ans_cld_some, collapse = ",")
      rv$task_durations[8] <- as.integer(rv$timer)
      loggit("INFO", "Block 2, cluster 'd' somewhat important response entered.", 
             paste0("Response: ", rv$task_responses[8], 
                    ". Duration: ", rv$task_durations[8], "."))
    }
  })
  
  ##### Obs survey answers -----
  observeEvent(input$survey1, {
    if(rv$timer > 1) {
      i <- 1
      rv$task_responses[i] <- input$survey1
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 1 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey2, {
    if(rv$timer > 1) {
      i <- 2
      rv$task_responses[i] <- input$survey2
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 2 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey3, {
    if(rv$timer > 1) {
      i <- 3
      rv$task_responses[i] <- input$survey3
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 3 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey4, {
    if(rv$timer > 1) {
      i <- 4
      rv$task_responses[i] <- input$survey4
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 4 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey5, {
    if(rv$timer > 1) {
      i <- 5
      rv$task_responses[i] <- input$survey5
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 5 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey6, {
    if(rv$timer > 1) {
      i <- 6
      rv$task_responses[i] <- input$survey6
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 6 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey7, {
    if(rv$timer > 1) {
      i <- 7
      rv$task_responses[i] <- input$survey7
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 7 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey8, {
    if(rv$timer > 1) {
      i <- 8
      rv$task_responses[i] <- input$survey8
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 8 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey9, {
    if(rv$timer > 1) {
      i <- 9
      rv$task_responses[i] <- input$survey9
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 9 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey10, {
    if(rv$timer > 1) {
      i <- 10
      rv$task_responses[i] <- input$survey10
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 10 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey11, {
    if(rv$timer > 1) {
      i <- 11
      rv$task_responses[i] <- input$survey11
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 11 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey12, {
    if(rv$timer > 1) {
      i <- 12
      rv$task_responses[i] <- input$survey12
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 12 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey13, {
    if(rv$timer > 1) {
      i <- 13
      rv$task_responses[i] <- input$survey13
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 13 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey14, {
    if(rv$timer > 1) {
      i <- 14
      rv$task_responses[i] <- input$survey14
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 14 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey15, {
    if(rv$timer > 1) {
      i <- 15
      rv$task_responses[i] <- input$survey15
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 15 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey16, {
    if(rv$timer > 1) {
      i <- 16
      rv$task_responses[i] <- input$survey16
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 16 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey17, {
    if(rv$timer > 1) {
      i <- 17
      rv$task_responses[i] <- input$survey17
      rv$task_durations[i] <- as.integer(rv$timer)
      loggit("INFO", "Survey 17 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })

  
  

  
  ### Obs next page button -----
  observeEvent(input$next_pg_button, {
    # Init rv$ans_tbl <- ans_tbl() first press
    if (rv$pg_num == 1){ rv$ans_tbl <- ans_tbl() }
    # if <on last task> {<do nothing>}
    if (rv$pg_num >= survey_start){ return() }
    
    
    ### _Training evaluation -----
    # If training section, evaluate response
    if (ui_section() == "training") {
      # Evaluate training block 1 
      if (block_num() == 1 & rv$training_aes == FALSE) {
        response <- input$blk1_ans
        ans <- length(attr(task_dat(), "ncl"))
        rv$training_aes <- TRUE
        if (response - ans >= 2){ # >= 2 clusters too high, retry
          rv$second_training <- TRUE
          output$plot_msg <- renderText(paste0(
          "<h3><span style='color:red'>
          That is little high, this data has ", ans, " clusters. 
          For PCA make sure to check a few combinations of components. 
          Using the grand tour look for clusters moving together. 
          In the manual tour rapidly check a few variables 
          while identifying clusters. Try again on another set. 
          </span></h3>")) 
          return()
        }
        if (response - ans <= -2){ # <= 2 clusters too low, retry
          rv$second_training <- TRUE
          output$plot_msg <- renderText(paste0(
            "<h3><span style='color:red'>
          That is little low, this data has ", ans, " clusters. 
          For PCA make sure to check a few combinations of components. 
          Using the grand tour look for clusters moving together. 
          In the manual tour rapidly check a few variables 
          while identifying clusters. Try again on another set. 
          </span></h3>")) 
          return()
        }
        if (abs(response - ans) == 1){ # within 1 cluster, msg, passes
          rv$second_training <- "ask"
          output$plot_msg <- renderText(paste0(
          "<h3><span style='color:red'>
          Close, this data has ", ans, " clusters. You have the right idea. 
          As a reminder,
          for PCA make sure to check a few combinations of components. 
          Using the grand tour look for clusters moving together. 
          In the manual tour rapidly check a few variables 
          while identifying clusters.
          </span></h3>"))
          return()
        }
        if (response == ans){ # exact answer
          rv$second_training <- "ask"
          output$plot_msg <- renderText(
          "<h3><span style='color:red'>
          That's correct, great job! 
          As a reminder,
          for PCA make sure to check a few combinations of components. 
          Using the grand tour look for clusters moving together. 
          In the manual tour rapidly check a few variables 
          while identifying clusters.
          </span></h3>") 
          return()
        }
      }
      # Evaluation of the training for blocks 2.

      if (block_num() == 2 & rv$training_aes == FALSE) {
        rv$training_aes <- TRUE
        # block 2 response
        resp_cla_very <- input$blk2_ans_cla_very # ~ c("V1", "V3") # row 1, col 1, 3 = 2
        resp_cla_some <- input$blk2_ans_cla_some # ~ c("V2", "V4", "V5") # row 1, col 2, 4, 5 = 1
        resp_clb_very <- input$blk2_ans_clb_very
        resp_clb_some <- input$blk2_ans_clb_some
        resp_clc_very <- input$blk2_ans_clc_very
        resp_clc_some <- input$blk2_ans_clc_some
        resp_cld_very <- input$blk2_ans_cld_very
        resp_cld_some <- input$blk2_ans_cld_some
        response_list <- list(resp_cla_very, resp_cla_some,
                              resp_clb_very, resp_clb_some,
                              resp_clc_very, resp_clc_some,
                              resp_cld_very, resp_cld_some)
        
        p    <- this_p()
        n_cl <- this_cl()
        response <- matrix(0, nrow = n_cl, ncol = p) # cannot distinguish between 0 and NA
        for (i in 1:n_cl){
          resp_i <- 2 * i - 1 
          this_col_very <- as.integer(substr(response_list[[resp_i]], 2, 2))
          this_col_some <- as.integer(substr(response_list[[resp_i + 1]], 2, 2))
          if (length(this_col_very) >= 1) response[i, this_col_very] <- 2
          if (length(this_col_some) >= 1) response[i, this_col_some] <- 1
        }
        
        # block 2 answer
        dat <- task_dat()
        supervied_dat <- data.frame(dat, cluster = attr(dat, "cluster"))
        this_lda <- MASS::lda(cluster ~ ., data = supervied_dat)
        abs_lda_means <- abs(this_lda$means)
        
        abs_lda_means_rowptile <- matrix(NA, nrow = nrow(this_lda$means),
                                         ncol = ncol(this_lda$means))
        for (i in 1:nrow(abs_lda_means)){
          abs_lda_means_rowptile[i, ] <- 
            abs_lda_means[i, ] / max(abs_lda_means[i,])
        }
        ans <- dplyr::case_when(
          abs_lda_means_rowptile >= .75 ~ 2, # very important is 2, > 75% ptile
          abs_lda_means_rowptile >= .25 ~ 1, # some important is 2, > 25% ptile
          abs_lda_means_rowptile >= 0 ~ 0
        )
        
        score <- -1 * sum((response - ans)^2) # i got -3 for 1 cl,
        bar <- -6 * (n_cl - 1)
        
        if (score < bar){ # score not passing
          rv$second_training <- TRUE
          output$plot_msg <- renderText(paste0(
            "<h3><span style='color:red'>
          That seems a little off. 
          Remember that the importance of a variable for
          distinguishing a group is related to variables in a separating 
          direction with large magnidutes in the projection, but variables with
          small contributions cannot be ruled out, multiple projections most be 
          looked at.
          </span></h3>"))
          return()
        }
        if (score >= bar){ # score is passing
          rv$second_training <- "ask"
          output$plot_msg <- renderText(paste0(
            "<h3><span style='color:red'>
            Very good! 
            As a reminder, the importance of a variable for
            distinguishing a group is related to variables in a separating 
            direction with large magnidutes in the projection, but variables with
            small contributions cannot be ruled out, multiple projections most be 
            looked at.
            </span></h3>"))
          return()
        }
      }
      
    } # end of training section evaluation
    
    # If task section, write reponses and duration to ans_tbl
    # _rv$ans_tbl -----
    if (ui_section() == "task" |
        ui_section() == "training" & block_num() %in% 1:2) {
      # if (ui_section() == "task") {browser()}
      #browser()
      ins_row <- which(rv$ans_tbl$blockrep == blockrep())[1] # first row of this blockrep.
      ins_nrows <- length(rv$task_responses) - 1
      rv$ans_tbl[ins_row:(ins_row + ins_nrows), 6] <- rv$task_responses
      rv$ans_tbl[ins_row:(ins_row + ins_nrows), 7] <- rv$task_durations
    }
    
    ### _New page ----
    # if second training not needed, skip a page.
    if (ui_section() == "training" & rep_num() %in% c(2, 4) & # rep 1 of block 1 & 2
        !(rv$second_training == TRUE | input$second_training == TRUE)) {
      rv$pg_num <- rv$pg_num + 1
    }
    rv$pg_num <- rv$pg_num + 1
    # Reset responses, duration, and timer for next task
    output$plot_msg <- renderText("")
    rv$task_responses <- NULL
    rv$task_durations <- NULL
    rv$timer <- 0
    rv$timer_active <- TRUE
    rv$training_aes <- FALSE
    rv$second_training <- FALSE
    updateCheckboxInput(session, "second_training", value = FALSE)
    
    # Clear task response
    if (ui_section() == "task") {
      if (block_num() == 1) { # reset to same settings.
        updateNumericInput(session, "blk1_ans", "",
                           value = 0, min = 0, max = 10)
      }
    }
    
    # Set structure for responses and durations
    n_rows <- 1
    if (block_num() == 1){n_rows <- 1} 
    if (block_num() == 2){n_rows <- this_k()}
    if (ui_section() == "survey") {n_rows <- n_survey_questions}
    
    rv$task_responses <- rep("default", n_rows)
    rv$task_durations <- rep("default", n_rows)
    loggit("INFO", "Next page: ", 
           paste0("rv$pg_num: ", rv$pg_num, 
                  ". ui_section(): ", ui_section(),
                  ". block_num(): ", block_num(),
                  ". rep_num(): ", rep_num(), 
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
    invalidateLater(1000, session)
    isolate({rv$timer <- rv$timer + 1})
    if(ui_section() == "task" & rv$timer_active == TRUE &
       ((block_num() == 1 & rv$timer > 60 ) |
        (block_num() == 2 & rv$timer > 180)) ){
      rv$timer_active <- FALSE
      loggit("INFO", "timer elapsed.", 
             paste0("On rv$pg_num: ", rv$pg_num, "."))
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
  output$second_training <- reactive(rv$second_training) # for more training button
  outputOptions(output, "is_saved",   suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  outputOptions(output, "pg_num",     suspendWhenHidden = FALSE) 
  outputOptions(output, "ui_section", suspendWhenHidden = FALSE) 
  outputOptions(output, "factor",     suspendWhenHidden = FALSE) 
  outputOptions(output, "block_num",  suspendWhenHidden = FALSE) 
  outputOptions(output, "rep_num",    suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  outputOptions(output, "second_training", suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  
  ### training outputs
  output$training_header_text <- renderText(training_header_text[block_num()])
  output$training_top_text    <- renderText(training_top_text[block_num()])
  ### general task outputs
  output$pca_plot   <- renderPlot({pca_plot()}, height = pca_height) 
  output$gtour_plot <- renderPlotly({suppressWarnings(gtour_plot())})
  output$mtour_plot <- renderPlot({mtour_plot()}, height = mtour_height)
  output$ans_tbl    <- renderTable({rv$ans_tbl})
  
  # output$blk1_ans defined in global_*.r
  ### Block 2 inputs, importance of var on cl seperation -----
  output$blk2Inputs <- renderUI({
    dat <- task_dat()
    p <- ncol(dat)                                # num var
    cl <- length(unique(attributes(dat)$cluster)) # num of clusters
    q <- (2 * (cl - 1))                           # num total questions
    
    lapply(1:q, function(this_q){
      this_clletter <- letters[rep(1:(cl - 1), each = 2)][this_q]
      this_q_txt    <- c("Very", "Somewhat")[rep(1:(cl - 1),2)[this_q]]
      this_q_id     <- tolower(substr(this_q_txt, 1, 4))
      
      checkboxGroupInput(
        inputId = paste0("blk2_ans_cl", this_clletter, "_", this_q_id), #blk2_ans_cla_very
        label   = paste0(this_q_txt, " important for distinguishing cluster '", 
                         this_clletter, "'"),
        choices = paste0("V", 1:p),
        inline  = TRUE
      )
      
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


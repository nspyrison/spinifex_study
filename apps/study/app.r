source('global.r', local = TRUE)


##### Server function, for shiny app
server <- function(input, output, session) {
  loggit("INFO", "Spinifex study app has started.")
  loggit("INFO", paste0("Computer name (Sys.info nodename):", Sys.info()[4]) )
  loggit("INFO", paste0("Factor id is ", as.character(this_factor_id), 
                        "; factor order: ", paste0(this_factor_order, collapse = ", "), ".") )
  
  ### Initialization and reactives -----
  rv                 <- reactiveValues()
  rv$pg_num          <- 1
  rv$timer           <- 999
  rv$stopwatch       <- 0
  rv$timer_active    <- TRUE
  rv$task_responses  <- NULL
  rv$task_durations  <- NULL
  rv$save_file       <- NULL
  rv$ans_tbl         <- NULL
  rv$training_aes    <- FALSE
  rv$second_training <- FALSE
  rv$curr_basis      <- NULL
  rv$this_theta      <- NULL
  
  ##### Start reactives
  p <- reactive({ ncol(task_dat()) })
  n_cl <- reactive({ length(unique(attributes(s_dat[[block_num()]])$cluster)) })
  
  ui_section <- reactive({ # text name of section
    if (rv$pg_num %in% 1:(training_start - 1)) {return("intro")}
    if (rv$pg_num %in% training_start:(task_start - 1) ) {return("training")}
    if (rv$pg_num %in% task_start:(survey_start - 1) ) {return("task")}
    if (rv$pg_num >= survey_start - 1) {return("survey")} 
  })
  section_pg_num <- reactive({ # ~page num of this section.
    if (ui_section() == "intro") {return(rv$pg_num)}
    if (ui_section() == "training"){
      return(rv$pg_num - (training_start - 1))
    }
    if (ui_section() == "task"){
      return(rv$pg_num - (task_start - 1))
    }
    if (ui_section() == "survey") {return(1)}
  })
  period_num <- reactive({1 + ((section_pg_num() - 1) %/% (n_tasks * n_blocks))})
  factor <- reactive({ # ~ PCA, gtour, mtour
    if (ui_section() == "training") return("training")
    if (ui_section() == "task") return(this_factor_order[period_num()])
    return("NONE / NA")
  })
  task_num <- reactive({ # 1:2
    if (ui_section() == "training") return(c(0, 1, 1, 2, 2, 0)[section_pg_num()])
    return(1 + ((section_pg_num() - 1) %/% n_blocks) - 2 * (period_num() - 1))
  })
  block_num <- reactive({ # 1:3
    if (ui_section() == "training") return(c(0, 1, 2, 1, 2, 0)[section_pg_num()])
    return((section_pg_num() - (n_blocks * (task_num() - 1))) %% (n_tasks * n_blocks))
  })
  taskblock <- reactive({ return(paste0(s_task_id[task_num()], block_num())) })
  sim_num <- reactive({
    return( (period_num() - 1) * 6 + (task_num() - 1) * 3 + block_num() )
  })
  task_time <- reactive({
    if (factor() == "grand") {adj <- 1
    } else adj <- 0
    if (task_num() == 1) return(60  + adj)
    if (task_num() == 2) return(180 + adj)
    return(999)
  })
  time_elapsed <- reactive({ as.integer(task_time() - rv$timer) })
  task_dat <- reactive({ # simulation df with attachments.
    if (ui_section() == "training") {
      if (section_pg_num() %in% c(3, 5)) {return(s_train[[2]])
      } else return(s_train[[1]])
    } else {
      return(s_dat[[sim_num()]]) 
    }
  })
  task_tpath <- reactive({
    if (ui_section() == "training") {
      if (section_pg_num() %in% c(3, 5)) {return(s_tpath_train[[2]])
      } else return(s_tpath_train[[1]])
    } else {
      return(s_tpath[[sim_num()]]) 
    }
  })
  manip_var_num <- reactive({ 
    if (input$manip_var == "<none>") {return(1)}
    return(which(colnames(task_dat()) == input$manip_var))
  }) 
  pca_active <- reactive({
    if ((rv$timer_active == TRUE & factor() == "pca") |
        (ui_section() == "training" & input$factor == "pca")) {
      return(TRUE)
    } else return(FALSE)
  })
  grand_active <- reactive({
    if ((rv$timer_active == TRUE & factor() == "grand") | 
        (ui_section() == "training" & input$factor == "grand")) {
      return(TRUE)
    } else return(FALSE)
  })
  manual_active <- reactive({
    if ((rv$timer_active == TRUE & factor() == "manual") | 
        (ui_section() == "training" & input$factor == "manual")) {
      return(TRUE)
    } else return(FALSE)
  })
  task_header <- reactive({
    paste0("Evaluation -- factor: ", factor(), ", task ", task_num() 
           # , ", difficulty: ", s_difficulty[block_num()]
    )
  })
  
  ### PCA plot reactive -----
  pca_height <- function(){
    if (pca_active() == TRUE) {
      return(640)
    } else return(1) 
  }
  pca_plot <- reactive({
    if (pca_active() == TRUE) {
      # data init
      dat <- task_dat()
      dat_std <- tourr::rescale(dat)
      cluster <- attributes(dat)$cluster
      
      # render init
      pal <- "Dark2"
      axes_position <- "center"
      USE_AXES <- TRUE
      USE_AES  <- TRUE
      if (task_num() == 1) {
        # USE_AXES <- FALSE # comments as manual may want this and consistency
        axes_position <- "bottomleft"
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
    }
  })
  
  ### Grand tour plot reactive -----
  gtour_height <- function(){
    if (grand_active() == TRUE) {
      return(640)
    } else return(1) 
  }
  gtour_plot <- reactive({ 
    if (grand_active() == TRUE) {
      # data init
      dat <- task_dat()
      dat_std <- tourr::rescale(dat)
      cluster <- attributes(dat)$cluster
      
      # tour init
      angle <- .1
      fps   <- 6
      max_frames <- 90 # 90 frame for 15 sec @ fps = 6
      set.seed(123) # if tourr starts using seeds
      
      # tpath <- save_history(dat_std, tour_path = grand_tour(), max = 8) # create 
      tpath <- task_tpath()
      
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
      if (task_num() == 1) {
        # USE_AXES <- FALSE # comments as manual may want this and consistency
        axes_position <- "bottomleft"
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
    }
  })
  
  
  ### Manual tour plot reactive -----
  mtour_height <- function() {
    if (manual_active()) {
      return(800)
    } else return(1) 
  }
  mtour_plot <- reactive({
    if (manual_active()) {
      # data init
      dat <- task_dat()
      dat_std <- tourr::rescale(dat)
      cluster <- attributes(dat)$cluster
      m_var <- NULL
      if (input$manip_var == "<none>") {m_var <- 1
      } else {m_var <- which(colnames(dat) == input$manip_var)}
      if (is.null(rv$curr_basis)) {
        dat <- task_dat()
        dat_std <- tourr::rescale(dat)
        pca <- prcomp(dat_std)
        rv$curr_basis <- pca$rotation[, c(1, 2)]
        # updateRadioButtons(session, "x_axis", selected = "PC1")
        # updateRadioButtons(session, "y_axis", selected = "PC2")
      }
      # render init
      pal <- "Dark2"
      axes_position <- "center"
      USE_AXES <- TRUE
      USE_AES  <- TRUE
      if (task_num() == 1) {
        # USE_AXES <- FALSE # comments as manual may want this and consistency
        axes_position <- "bottomleft"
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
      } else { # when USE_AES == FALSE
        ret <- app_oblique_frame(data      = dat_std,
                                 basis     = rv$curr_basis,
                                 manip_var = m_var,
                                 theta     = 0, 
                                 phi       = 0,
                                 axes      = axes_position
        )
      }
      
      return(ret)
    } # non-display conditions return nothing.
  })
  
  
  ### ans_tbl() reactive -----
  ans_tbl <- reactive({
    # init columns
    col_factor <- 
      c(
        rep("training", n_trainings + n_task2_questions * n_trainings),     # training
        rep(this_factor_order[1], n_blocks + n_task2_questions * n_blocks), # tasks across factor
        rep(this_factor_order[2], n_blocks + n_task2_questions * n_blocks),
        rep(this_factor_order[3], n_blocks + n_task2_questions * n_blocks),
        rep("survey", 5),                                                   # survey
        rep(paste0("survey_", this_factor_order[1]), 4),
        rep(paste0("survey_", this_factor_order[2]), 4),
        rep(paste0("survey_", this_factor_order[3]), 4)
      )
    col_taskblock <- 
      c(paste0(s_task_id[1], 1:2),                       # training
        rep(paste0(s_task_id[2], 1), n_task2_questions),
        rep(paste0(s_task_id[2], 2), n_task2_questions),
        rep(
          c(s_taskblock_id[1:3],                         # task 1
            rep(s_taskblock_id[4], n_task2_questions),   # task 2
            rep(s_taskblock_id[5], n_task2_questions),
            rep(s_taskblock_id[6], n_task2_questions)
          ),
          n_factors                                      # across factors
        ),      
        paste0("survey", 1:5),                           # survey
        paste0("survey_", this_factor_order[1], 1:4),
        paste0("survey_", this_factor_order[2], 1:4),
        paste0("survey_", this_factor_order[3], 1:4)
      )
    sim_set <- c(101, 107, 113,                 # task 1
                 rep(102, n_task2_questions),   # task 2
                 rep(108, n_task2_questions),
                 rep(114, n_task2_questions)
    )
    col_sim_id <- 
      as.character(
        c(
          119:120,                     # training 1 
          rep(119, n_task2_questions), # training 2
          rep(120, n_task2_questions),
          sim_set,                     # tasks across factors
          sim_set + 2,
          sim_set + n_task2_questions,
          rep(NA, n_survey_questions)  # survey
        )
      )
    col_question <- 
      c(
        rep(s_task_prompts[1], n_trainings),
        rep(s_task2_questions, n_trainings),  # training
        rep(
          c(rep(s_task_prompts[1], n_blocks), # task 1
            rep(s_task2_questions, n_blocks)  # task 2
          ),
          n_factors                           # across factors
        ),
        s_survey_questions                    # survey
      ) 
    col_response <- col_duration <-
      c(
        rep(NA, n_trainings + n_task2_questions * n_trainings), # training
        rep(
          c(rep(NA, n_blocks),                                  # task 1
            rep(NA, n_task2_questions * n_blocks)               # task 2
          ),
          n_factors                                             # across factors
        ),
        rep(NA, n_survey_questions)                             # survey
      )
    
    data.frame(factor    = col_factor,
               taskblock = col_taskblock,
               sim_id    = col_sim_id,
               question  = col_question,
               response  = col_response,
               duration  = col_duration)
  })
  ##### End of reactives
  
  
  ##### Start observes
  ### Obs update axis/task2 choices -----
  observeEvent(task_dat(), { # Init axis choices when data changes
    rv$this_theta <- NULL
    p <- p()
    if (pca_active() == TRUE | manual_active() == TRUE) {
      choices <- paste0("PC", 1:p)
      updateRadioButtons(session, "x_axis", choices = choices, selected = "PC1")
      updateRadioButtons(session, "y_axis", choices = choices, selected = "PC2")
      loggit("INFO", "Task data changed while axes active; updated axes choices.")
    }
    if (task_num() == 2) {
      choices <- paste0("V", 1:p)
      updateCheckboxGroupInput(session, "tsk2_ans_very_ab",
                               choices = choices, inline  = TRUE)
      updateCheckboxGroupInput(session, "tsk2_ans_some_ab",
                               choices = choices, inline  = TRUE)
      updateCheckboxGroupInput(session, "tsk2_ans_very_bc",
                               choices = choices, inline  = TRUE)
      updateCheckboxGroupInput(session, "tsk2_ans_some_bc",
                               choices = choices, inline  = TRUE)
      loggit("INFO", "Task data changed on task 2; updated responce choices.")
    }
  })
  # Bump x_axis when set to the same as y_axis
  observeEvent(input$x_axis, { 
    if (input$x_axis == input$y_axis) {
      rv$this_theta <- NULL
      p <- ncol(task_dat())
      choices <- paste0("PC", 1:p)
      opts <- choices[!choices %in% input$x_axis]
      x_axis_out <- sample(opts, 1)
      updateRadioButtons(session, "x_axis", choices = choices, selected = x_axis_out)
      loggit("INFO", paste0("x_axis set to ", input$x_axis, 
                            ", same as y_axis; x_axis bumped to ", x_axis_out, "."))
    }
  })
  # Bump y_axis when set to the same as x_axis
  observeEvent(input$y_axis, {
    if (input$x_axis == input$y_axis) {
      rv$this_theta <- NULL
      p <- ncol(task_dat())
      choices <- paste0("PC", 1:p)
      opts <- choices[!choices %in% input$x_axis]
      y_axis_out <- sample(opts, 1)
      updateRadioButtons(session, "y_axis", choices = choices, selected = sample(opts, 1))
      loggit("INFO", paste0("y_axis set to ", input$y_axis, 
                            ", same as x_axis; y_axis bumped to ", y_axis_out, "."))
    }
  })
  
  ### Obs mtour basis ----- 
  observeEvent({
    task_dat()
    input$x_axis
    input$y_axis
  },
  {
    if (manual_active() == TRUE){
      rv$this_theta <- NULL
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
             paste0("x_axis: ", x, ", y_axis: ", y, ". rv$curr_basis: ",
                    paste0(round(rv$curr_basis, 2), collapse = ", ")))
    }
  })
  
  ### Obs mtour slider ----
  observeEvent(input$manip_slider, {
    if (manual_active() == TRUE & input$manip_var != "<none>") {
      theta <- phi <- NULL
      mv_sp <- create_manip_space(rv$curr_basis, manip_var_num())[manip_var_num(), ]
      if ("Radial" == "Radial" & !is.null(input$manip_slider)) { # Fixed to "Radial" # input$manip_type == "Radial"
        if (is.null(rv$this_theta)) {theta <- atan(mv_sp[2] / mv_sp[1])
        } else {theta <- rv$this_theta}
        if (input$manip_slider == 0) rv$this_theta <- theta
        phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
        phi <- (acos(input$manip_slider) - phi_start) * - sign(mv_sp[1])
      }
      ret <- oblique_basis(basis = rv$curr_basis, manip_var = manip_var_num(),
                           theta = theta, phi = phi)
      row.names(ret) <- colnames(task_dat())
      
      rv$curr_basis <- ret
      loggit("INFO", paste0("Slider value changed: ", input$manip_slider),
             paste0("rv$curr_basis updated: ",
                    paste0(round(rv$curr_basis, 2), collapse = ", ")))
    }
  })
  
  ### Obs mtour update manip_var choices -----
  observeEvent({
    task_dat()
    input$factor
  }, 
  { # Init manip_var choices on data change.
    if (manual_active() == TRUE) {
      rv$this_theta <- NULL
      these_colnames <- colnames(task_dat())
      updateSelectInput(session, "manip_var", choices = these_colnames, 
                        selected = these_colnames[1])
      loggit("INFO", paste0("Task data changed; input$manip_var choices updated."))
    }
  })
  
  ### Obs mtour update slider value -----
  observeEvent(
    {
      manip_var_num()
      task_dat()
      input$factor
      input$x_axis
      input$y_axis
    }, {
      if (manual_active() == TRUE) {
        mv_sp <- create_manip_space(rv$curr_basis, manip_var_num())[manip_var_num(), ]
        phi_i <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
        this_val <- round(cos(phi_i), 1) # Rad
        updateSliderInput(session, "manip_slider", value = this_val)
        loggit("INFO", 
               paste0("New manip slider value (from task_dat/axes/manip_var)."), 
               paste0("manip_slider: ", this_val))
      }
    })
  
  ##### Obs task answers -----
  ### task 1 response & duration
  observeEvent(input$tsk1_ans, {
    if(time_elapsed() > 1) {
      rv$task_responses[1] <- input$tsk1_ans
      rv$task_durations[1] <- time_elapsed()
      loggit("INFO", "Task 1 entered.", 
             paste0("Response: ", rv$task_responses[1], 
                    ". Duration: ", rv$task_durations[1], "."))
    }
  })
  ### task 2 responses & duration
  observeEvent(input$tsk2_ans_very_ab, {
    if(time_elapsed() > 1) {
      rv$task_responses[1] <- paste(input$tsk2_ans_cla_very, collapse = ",")
      rv$task_durations[1] <- time_elapsed()
      loggit("INFO", "Task 2, very important, clusters ab response entered.", 
             paste0("Response: ", rv$task_responses[1], 
                    ". Duration: ", rv$task_durations[1], "."))
    }
  })
  observeEvent(input$tsk2_ans_some_ab, {
    if(time_elapsed() > 1) {
      rv$task_responses[2] <- paste(input$tsk2_ans_cla_some, collapse = ",")
      rv$task_durations[2] <- time_elapsed()
      loggit("INFO", "Task 2, somewhat important clusters ab response entered.", 
             paste0("Response: ", rv$task_responses[2], 
                    ". Duration: ", rv$task_durations[2], "."))
    }
  })
  observeEvent(input$tsk2_ans_very_bc, {
    if(time_elapsed() > 1) {
      rv$task_responses[3] <- paste(input$tsk2_ans_clb_very, collapse = ",")
      rv$task_durations[3] <- time_elapsed()
      loggit("INFO", "Task 2, very important, clusters bc response entered.", 
             paste0("Response: ", rv$task_responses[3], 
                    ". Duration: ", rv$task_durations[3], "."))
    }
  })
  observeEvent(input$tsk2_ans_some_bc, {
    if(time_elapsed() > 1) {
      rv$task_responses[4] <- paste(input$tsk2_ans_clb_some, collapse = ",")
      rv$task_durations[4] <- time_elapsed()
      loggit("INFO", "Task 2, somewhat important, clusters bc response entered.", 
             paste0("Response: ", rv$task_responses[4], 
                    ". Duration: ", rv$task_durations[4], "."))
    }
  })
  
  ##### Obs survey answers -----
  observeEvent(input$survey1, {
    if(time_elapsed() > 1) {
      i <- 1
      rv$task_responses[i] <- input$survey1
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 1 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey2, {
    if(time_elapsed() > 1) {
      i <- 2
      rv$task_responses[i] <- input$survey2
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 2 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey3, {
    if(time_elapsed() > 1) {
      i <- 3
      rv$task_responses[i] <- input$survey3
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 3 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey4, {
    if(time_elapsed() > 1) {
      i <- 4
      rv$task_responses[i] <- input$survey4
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 4 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey5, {
    if(time_elapsed() > 1) {
      i <- 5
      rv$task_responses[i] <- input$survey5
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 5 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey6, {
    if(time_elapsed() > 1) {
      i <- 6
      rv$task_responses[i] <- input$survey6
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 6 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey7, {
    if(time_elapsed() > 1) {
      i <- 7
      rv$task_responses[i] <- input$survey7
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 7 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey8, {
    if(time_elapsed() > 1) {
      i <- 8
      rv$task_responses[i] <- input$survey8
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 8 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey9, {
    if(time_elapsed() > 1) {
      i <- 9
      rv$task_responses[i] <- input$survey9
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 9 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey10, {
    if(time_elapsed() > 1) {
      i <- 10
      rv$task_responses[i] <- input$survey10
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 10 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey11, {
    if(time_elapsed() > 1) {
      i <- 11
      rv$task_responses[i] <- input$survey11
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 11 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey12, {
    if(time_elapsed() > 1) {
      i <- 12
      rv$task_responses[i] <- input$survey12
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 12 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey13, {
    if(time_elapsed() > 1) {
      i <- 13
      rv$task_responses[i] <- input$survey13
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 13 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey14, {
    if(time_elapsed() > 1) {
      i <- 14
      rv$task_responses[i] <- input$survey14
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 14 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey15, {
    if(time_elapsed() > 1) {
      i <- 15
      rv$task_responses[i] <- input$survey15
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 15 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey16, {
    if(time_elapsed() > 1) {
      i <- 16
      rv$task_responses[i] <- input$survey16
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 16 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  observeEvent(input$survey17, {
    if(time_elapsed() > 1) {
      i <- 17
      rv$task_responses[i] <- input$survey17
      rv$task_durations[i] <- time_elapsed()
      loggit("INFO", "Survey 17 entered.", 
             paste0("Response: ", rv$task_responses[i], 
                    ". Duration: ", rv$task_durations[i], "."))
    }
  })
  
  ### Obs next page button -----
  observeEvent(input$next_pg_button, {
    if ((rv$stopwatch > 2 & is_logging == TRUE) |
        is_logging == FALSE){
      # Init rv$ans_tbl <- ans_tbl() first press
      if (is.null(rv$ans_tbl)){ rv$ans_tbl <- ans_tbl() }
      # if <on last task> {<do nothing>}. Also shouldn't be visible
      if (rv$pg_num >= survey_start){ return() }
      
      
      ### _Training evaluation -----
      # If training section, evaluate response
      if (ui_section() == "training") {
        # Evaluate training task 1 
        if (task_num() == 1 & rv$training_aes == FALSE) {
          rv$training_aes <- TRUE
          response <- input$tsk1_ans
          ans <- length(attr(task_dat(), "ncl"))
          delta <- response - ans
          
          theme_start <- "<h3><span style='color:red'>"
          this_msg <- ""
          main_msg <- 
            "For PCA make sure to plot several components. 
          Using the grand tour look for groups of points moving together. 
          In the manual tour choose the variables with the largest axes 
          sequentially to manipulate their contribution to the projection."
          theme_end <- "</span></h3>"
          
          if (delta >= 2){ # >= 2 clusters too high, retry
            rv$second_training <- TRUE
            this_msg <- paste0("That is little high, this data has ", ans, " clusters. 
            Try again on another training set. ")
          }
          if (delta <= -2){ # <= 2 clusters too low, retry
            rv$second_training <- TRUE
            this_msg <- paste0("That is little low, this data has ", ans, " clusters. 
            Try again on another training set. ")
          }
          if (abs(delta) == 1){ # within 1 cluster, passes
            rv$second_training <- "ask"
            this_msg <- paste0("Close, this data has ", ans, " clusters. 
            You have the right idea. As a reminder, ")
          }
          if (delta == 0){ # exact answer, passes
            rv$second_training <- "ask"
            this_msg <- paste0("That's correct, this data has ", ans, " clusters.
            As a reminder, ")
          }
          output$plot_msg <- renderText(paste0(
            theme_start, this_msg, main_msg, theme_end
          ))
          return()
        }
        
        # Evaluation of the training for tasks 2.
        if (task_num() == 2 & rv$training_aes == FALSE) {
          rv$training_aes <- TRUE
          # task 2 response
          resp_cla_very <- input$tsk2_ans_cla_very # ~ c("V1", "V3") # row 1, col 1, 3 = 2
          resp_cla_some <- input$tsk2_ans_cla_some # ~ c("V2", "V4", "V5") # row 1, col 2, 4, 5 = 1
          resp_clb_very <- input$tsk2_ans_clb_very
          resp_clb_some <- input$tsk2_ans_clb_some
          
          response_list <- list(resp_cla_very, resp_cla_some,
                                resp_clb_very, resp_clb_some)
          
          p    <- p()
          n_cl <- n_cl()
          response <- matrix(0, nrow = n_cl, ncol = p) # cannot distinguish between 0 and NA
          for (i in 1:2){
            resp_i <- 2 * i - 1 
            this_col_very <- as.integer(substr(response_list[[resp_i]], 2, 2))
            this_col_some <- as.integer(substr(response_list[[resp_i + 1]], 2, 2))
            if (length(this_col_very) >= 1) response[i, this_col_very] <- 2
            if (length(this_col_some) >= 1) response[i, this_col_some] <- 1
          }
          
          # task 2 answer
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
          direction with large magnitudes in the projection, but variables with
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
          (ui_section() == "training" & task_num() %in% 1:2)) {
        ins_row_start <- which(rv$ans_tbl$factor == factor() &
                                 rv$ans_tbl$taskblock == taskblock())[1] # first row of this taskblock.
        ins_row_end <- ins_row_start + length(rv$task_responses) - 1
        rv$ans_tbl$response[ins_row_start:ins_row_end] <- rv$task_responses
        rv$ans_tbl$duration[ins_row_start:ins_row_end] <- rv$task_durations
      }
      
      ### _New page ----
      # if second training not needed, skip a page.
      if (ui_section() == "training" & block_num() %in% c(2, 4) & # rep 1 of task 1 & 2
          !(rv$second_training == TRUE | input$second_training == TRUE)) {
        rv$pg_num <- rv$pg_num + 1
      }
      rv$pg_num <- rv$pg_num + 1
      
      # Reset responses, duration, and timer for next task
      output$plot_msg <- renderText("")
      rv$task_responses <- NULL
      rv$task_durations <- NULL
      rv$timer <- task_time()
      rv$stopwatch <- 0
      rv$timer_active <- TRUE
      rv$training_aes <- FALSE
      rv$second_training <- FALSE
      updateCheckboxInput(session, "second_training", value = FALSE)
      
      # Clear task response
      if (ui_section() == "task") {
        if (task_num() == 1) { # reset to same settings.
          updateNumericInput(session, "tsk1_ans", "",
                             value = 0, min = 0, max = 10)
        }
      }
      
      # Set structure for responses and durations
      n_rows <- 0
      if (task_num() == 1){n_rows <- 1} 
      if (task_num() == 2){n_rows <- 4}
      if (ui_section() == "survey") {n_rows <- n_survey_questions}
      
      rv$task_responses <- rep("default", n_rows)
      rv$task_durations <- rep("default", n_rows)
      loggit("INFO", paste0("Next page: section ", ui_section(), 
                            ", section page ", section_pg_num()),
             paste0("rv$pg_num: ", rv$pg_num, 
                    ". ui_section(): ", ui_section(),
                    ". period_num(): ", period_num(),
                    ". factor(): ", factor(),
                    ". task_num(): ", task_num(),
                    ". block_num(): ", block_num(), 
                    ". Wrote responses to rv$ans_tbl."))
    }
  })
  
  
  ### Obs save reponses button -----
  observeEvent(input$save_ans, {
    filebase = paste("responses_", this_factor_id, Sys.info()[4], sep = "_")
    prefix = ""
    
    # Write survey responses to rv$ans_tbl
    ins_row_start <- nrow(rv$ans_tbl) - n_survey_questions + 1
    ins_row_end   <- nrow(rv$ans_tbl)
    rv$ans_tbl$response[ins_row_start:ins_row_end] <- rv$task_responses
    rv$ans_tbl$duration[ins_row_start:ins_row_end] <- rv$task_durations
    
    # Write rv$ans_tbl to .csv file.
    df <- rv$ans_tbl
    if (!is.null(rv$save_file)){ # if save already exists 
      save_msg <- paste0("<h3><span style='color:red'>Reponses already saved as ", 
                         rv$save_file, ".</span></h3>")
      output$save_msg <- renderText(save_msg)
      loggit("INFO", "Save button pressed (Previously saved).", 
             paste0("save_msg: ", save_msg,  "."))
      return()
    }
    
    # Do the actual saving
    save_base <- paste0(prefix, filebase, "_")
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
    
    if (prefix == "") {
      loggit("INFO", "Save button pressed.", 
             paste0("rv$save_file: ", rv$save_file, 
                    ". save_msg: ", save_msg,
                    "."))
    }
    if (prefix != "") {
      loggit("INFO", paste0("NOTE: Prefixed save script run. Prefix was '", prefix, "'."), 
             paste0("Save may not have been user initiated",
                    ". PREFIX USED: ", prefix,
                    ". rv$save_file: ", rv$save_file, 
                    ". save_msg: ", save_msg, "."))
    }
  })
  
  ### Obs browser -----
  observeEvent(input$browser, {browser()})
  
  ### Obs timer -----
  observe({
    task_time <- task_time()
    invalidateLater(1000, session)
    isolate({
      rv$timer <- rv$timer - 1
      rv$stopwatch <- rv$stopwatch + 1
    })
    if(rv$timer < 0 & ui_section() == "task" & rv$timer_active == TRUE){
      rv$timer_active <- FALSE
      loggit("INFO", "Timer elapsed.", 
             paste0("On factor: ", factor(), ", task-block:", taskblock(), "."))
    }
  })
  
  
  ##### Outputs -----
  output$timer_disp <- renderText({
    if (ui_section() == "task") { # disp timer if not an intro page.
      if (rv$timer < 1) {return("Time has expired, please enter your best guess and proceed.")
      } else {return(paste0("Time left: ", lubridate::seconds_to_period(rv$timer)))}
    }
  })
  output$stopwatch_disp <- renderText({
    if (ui_section() == "training" & block_num() != 6) { # disp timer if not an intro page.
      return(paste0("Time elapsed this task: ", lubridate::seconds_to_period(rv$stopwatch)))
    }
  })
  
  ### Controls ui coditionalPanels 
  output$is_saved   <- reactive(if (is.null(rv$save_file)) {0} else {1}) # control save_msg.
  output$pg_num     <- reactive(rv$pg_num)    # for hiding ui next_task button
  output$ui_section <- reactive(ui_section()) # for ui between sections
  output$factor     <- reactive(factor())     # for sidebar inputs
  output$task_num   <- reactive(task_num())   # for titles, and response inputs
  output$block_num  <- reactive(block_num())  # for training ui
  output$section_pg_num <- reactive(section_pg_num()) # For navigating training
  output$second_training <- reactive(rv$second_training) # for more training button
  
  outputOptions(output, "is_saved",        suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  outputOptions(output, "pg_num",          suspendWhenHidden = FALSE)
  outputOptions(output, "ui_section",      suspendWhenHidden = FALSE)
  outputOptions(output, "factor",          suspendWhenHidden = FALSE)
  outputOptions(output, "task_num",        suspendWhenHidden = FALSE)
  outputOptions(output, "block_num",       suspendWhenHidden = FALSE)
  outputOptions(output, "section_pg_num",  suspendWhenHidden = FALSE)
  outputOptions(output, "second_training", suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  
  ### general task outputs
  output$task_header <- renderText(task_header())
  output$pca_plot    <- renderPlot({pca_plot()}, height = pca_height)
  output$gtour_plot  <- renderPlotly({suppressWarnings(gtour_plot())})
  output$mtour_plot  <- renderPlot({mtour_plot()}, height = mtour_height)
  output$ans_tbl     <- renderTable({rv$ans_tbl})
  
  ### Dev msg -----
  output$dev_msg <- renderPrint(cat("dev msg -- ", "\n",
                                    "this ans_tbl row: ", factor(), "|", taskblock(), "\n",
                                    "rv$pg_num: ", rv$pg_num, "\n",
                                    "ui_section() ", ui_section(), "\n",
                                    "section_pg_num() ", section_pg_num(), "\n",
                                    "factor(): ", factor(), "\n",
                                    "task_num(): ", task_num(), "\n",
                                    "block_num(): ", block_num(), "\n",
                                    "rv$timer: ", rv$timer, "\n",
                                    sep = ""))
  
}

### Combine as shiny app.
shinyApp(ui = ui, server = server)


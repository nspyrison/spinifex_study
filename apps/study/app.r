source('global.r', local = TRUE)


##### Server function, for shiny app
server <- function(input, output, session) {
  loggit("INFO", "Spinifex study app has started.",
         paste0("Computer name (Sys.info nodename):", Sys.info()[4],
                ", group is ", as.character(this_group), 
                "; factor order: ", paste0(this_factor_order, e = ", "), "."
         )
  )
  
  ### Reavtive value initialization -----
  rv                 <- reactiveValues()
  rv$pg              <- 1L
  rv$timer           <- 999
  rv$stopwatch       <- 0
  rv$timer_active    <- TRUE
  rv$pca_inter       <- 1L
  rv$manual_inter    <- 1L
  rv$resp_inter      <- 1L
  rv$task_ttr        <- NULL
  rv$task_response   <- NULL
  rv$save_file       <- NULL
  rv$ans_tbl         <- NULL
  rv$training_aes    <- FALSE
  rv$second_training <- FALSE
  rv$curr_basis      <- NULL
  rv$manual_ls       <- list()
  rv$basis_ls        <- list()
  
  ##### Reactives -----
  p <- reactive({ ncol(dat()) })
  n_cl <- reactive({ length(unique(attributes(s_dat[[block()]])$cluster)) })
  
  section <- reactive({ # text name of section
    if (rv$pg %in% 1:(training_start - 1)) {return("intro")}
    if (rv$pg %in% training_start:(task_start - 1) ) {return("training")}
    if (rv$pg %in% task_start:(survey_start - 1) ) {return("task")}
    if (rv$pg >= survey_start - 1) {return("survey")} 
  })
  section_pg <- reactive({ # ~page num of this section.
    if (section() == "intro") {return(rv$pg)}
    if (section() == "training"){
      return(rv$pg - (training_start - 1))
    }
    if (section() == "task"){
      return(rv$pg - (task_start - 1))
    }
    if (section() == "survey") {return(1)}
  })
  period <- reactive({1 + ((section_pg() - 1) %/% (n_tasks * n_blocks))})
  factor <- reactive({ # ~ PCA, gtour, mtour
    if (section() == "training") return("training")
    if (section() == "task") return(this_factor_nm_order[period()])
    return("NONE / NA")
  })
  task <- reactive({ # 1:2
    if (section() == "training") return(c(0, 1, 1, 2, 2, 0)[section_pg()])
    return(1 + ((section_pg() - 1) %/% n_blocks) - 2 * (period() - 1))
  })
  block <- reactive({ # 1:3
    if (section() == "training") return(c(0, "t", "t", "t", "t", 0)[section_pg()])
    return((section_pg() - (n_blocks * (task() - 1))) %% (n_tasks * n_blocks))
  })
  sim <- reactive({
    return( (period() - 1) * 6 + (task() - 1) * 3 + block() )
  })
  task_time <- reactive({
    if (factor() == "grand") {adj <- 1
    } else adj <- 0
    if (task() == 1) return(60  + adj)
    if (task() == 2) return(180 + adj)
    return(999)
  })
  time_elapsed <- reactive({ as.integer(task_time() - rv$timer) })
  dat <- reactive({ # simulation df with attachments.
    if (section() == "training") {
      if (section_pg() %in% c(3, 5)) {return(s_train[[2]])
      } else return(s_train[[1]])
    } else {
      return(s_dat[[sim()]]) 
    }
  })
  task_tpath <- reactive({
    if (section() == "training") {
      if (section_pg() %in% c(3, 5)) {return(s_tpath_train[[2]])
      } else return(s_tpath_train[[1]])
    } else {
      return(s_tpath[[sim()]]) 
    }
  })
  manip_var <- reactive({ 
    if (input$manip_var_nm == "<none>") {return(1)}
    return(which(colnames(dat()) == input$manip_var_nm))
  }) 
  pca_active <- reactive({
    if ((rv$timer_active == TRUE & factor() == "pca") |
        (section() == "training" & input$factor == "pca")) {
      return(TRUE)
    } else return(FALSE)
  })
  grand_active <- reactive({
    if ((rv$timer_active == TRUE & factor() == "grand") | 
        (section() == "training" & input$factor == "grand")) {
      return(TRUE)
    } else return(FALSE)
  })
  manual_active <- reactive({
    if ((rv$timer_active == TRUE & factor() == "manual") | 
        (section() == "training" & input$factor == "manual")) {
      return(TRUE)
    } else return(FALSE)
  })
  task_header <- reactive({
    paste0("Evaluation -- factor: ", factor(), ", task ", task() 
           # , ", difficulty: ", s_difficulty[block()] # don't show difficulty
    )
  })
  ### Throttle manip_slider
  manip_slider   <- reactive(input$manip_slider)
  manip_slider_t <- throttle(manip_slider, 10)
  
  ### Task 1 answer -----
  task1_ans <- reactive({
    if (task() == 1){
      response <- input$tsk1_ans
      length(attr(dat(), "ncl"))
    }
  })
  task1_score <- reactive({
    if (task() == 1){
      abs(input$tsk1_ans - task1_ans())
    }
  })  
  
  ### Task 2 answer 
  task2_ans_ptile <- reactive({
    if (task() == 2){
      .dat <- dat()
      .supervied_sim <- data.frame(.dat, cluster = attr(.dat, "cluster"))
      .lda <- MASS::lda(cluster~., data = .supervied_sim)
      
      .abs_mean_diff_ab <- abs(.lda$means[1, ] - .lda$means[2, ])
      .abs_mean_diff_bc <- abs(.lda$means[2, ] - .lda$means[3, ])
      .ab_ptile <- .abs_mean_diff_ab / max(.abs_mean_diff_ab)
      .bc_ptile <- .abs_mean_diff_bc / max(.abs_mean_diff_bc)
      matrix(c(.ab_ptile, .bc_ptile), 
             nrow = 2, ncol = p(), byrow = T)
    }
  })
  task2_ans <- reactive({
    if (task() == 2){
      .ptile_mat <- task2_ans_ptile()
      .ptile <- c(.ptile_mat[1, ], .ptile_mat[2, ])
      .ans_vect <- dplyr::case_when(
        .ptile >= .75 ~ 2, # very
        .ptile >= .25 ~ 1, # somewhat
        .ptile >= 0 ~ 0
      )
      matrix(.ans_vect, nrow = 2, ncol = p(), byrow = T)
    }
  })
  ### Score individual inputs for ans_tbl rows
  task2_vars_very_ab <- reactive({
    if (task() == 2){
      which(task2_ans()[1, ] == 2)
    }
  })
  task2_vars_some_ab <- reactive({
    if (task() == 2){
      which(task2_ans()[1, ] == 1)
    }
  })
  task2_vars_very_bc <- reactive({
    if (task() == 2){
      which(task2_ans()[2, ] == 2)
    }
  })
  task2_vars_some_bc <- reactive({
    if (task() == 2){
      which(task2_ans()[2, ] == 1)
    }
  })
  task2_score_very_ab <- reactive({
    if (task() == 2){
      resp <- as.integer(gsub(' |V', '', input$tsk2_ans_very_ab))
      ans  <- task2_vars_very_ab()
      -(length(union(resp, ans)) - length(intersect(resp, ans))) * 2^2
    }
  })
  task2_score_some_ab <- reactive({
    if (task() == 2){
      resp <- as.integer(gsub(' |V', '', input$tsk2_ans_some_ab))
      ans  <- task2_vars_some_ab()
      -(length(union(resp, ans)) - length(intersect(resp, ans))) * 1^2
    }
  })
  task2_score_very_bc <- reactive({
    if (task() == 2){
      resp <- as.integer(gsub(' |V', '', input$tsk2_ans_very_bc))
      ans  <- task2_vars_very_bc()
      -(length(union(resp, ans)) - length(intersect(resp, ans))) * 2^2
    }
  })
  task2_score_some_bc <- reactive({
    if (task() == 2){
      resp <- as.integer(gsub(' |V', '', input$tsk2_ans_some_bc))
      ans  <- task2_vars_some_bc()
      -(length(union(resp, ans)) - length(intersect(resp, ans))) * 1^2
    }
  })
  ### Collated response and scores
  task2_resp <- reactive({
    if (task() == 2){
      resp <- matrix(0, nrow = 2, ncol = p())
      row.names(resp) <- c("ab", "bc")
      task2_very_ab <- as.integer(gsub(' |V', '', input$tsk2_ans_very_ab))
      task2_some_ab <- as.integer(gsub(' |V', '', input$tsk2_ans_some_ab))
      task2_very_bc <- as.integer(gsub(' |V', '', input$tsk2_ans_very_bc))
      task2_some_bc <- as.integer(gsub(' |V', '', input$tsk2_ans_some_bc))
      resp[1, task2_very_ab] <- 2
      resp[1, task2_some_ab] <- 1
      resp[2, task2_very_bc] <- 2
      resp[2, task2_some_bc] <- 1
      resp
    }
  })
  task2_score <- reactive({
    if (task() == 2){
      ans  <- task2_ans()
      resp <- task2_resp()
      -sum((ans - resp)^2)
    }
  })
  
  ### PCA plot reactive -----
  pca_height <- function(){
    if (pca_active() == TRUE) {
      return(600)
    } else return(1) 
  }
  pca_plot <- reactive({
    if (pca_active() == TRUE) {
      # data init
      dat <- dat()
      dat_std <- tourr::rescale(dat)
      cluster <- attributes(dat)$cluster
      # dat_std <- tourr::rescale(flea[,2:7]); cluster <- flea$species; colnames(dat_std) <- paste("V", 1:6)
      
      # render init
      pal <- "Dark2"
      axes_position <- "left"
      USE_AXES <- TRUE
      USE_AES  <- TRUE
      if (task() == 1) {
        if(rv$training_aes == FALSE) { # During training
          USE_AES  <- FALSE
        } 
      }
      
      # x_axis <- "PC1"; y_axis <- "PC2"
      x_axis <- input$x_axis
      y_axis <- input$y_axis
      x_num  <- as.integer(substr(x_axis, 3, 3))
      y_num  <- as.integer(substr(y_axis, 3, 3))
      
      pca     <- prcomp(dat_std)
      pca_x   <- 2 * (data.frame(tourr::rescale(pca$x[ , c(x_num, y_num)])) - .5)
      pca_rot <- data.frame(pca$rotation[ , c(x_num, y_num)])
      pca_rot <- app_set_axes_position(pca_rot, axes_position)
      pca_pct_var <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
      x_lab <- paste0(x_axis, " (", pca_pct_var[x_num], "% Var)")
      y_lab <- paste0(y_axis, " (", pca_pct_var[y_num], "% Var)")
      
      angle <- seq(0, 2 * pi, length = 360)
      circ  <- app_set_axes_position(data.frame(x = cos(angle), y = sin(angle)),
                                     axes_position)
      zero  <- app_set_axes_position(0, axes_position)
      
      ### ggplot2
      gg <- ggplot()
      if (USE_AES == FALSE){
        # data points
        gg <- gg + 
          geom_point(pca_x, 
                     mapping = aes(x = get(x_axis), y = get(y_axis)), 
                     size = 3)
      } else { # if USE_AES == TRUE then apply more aes.
        gg <- gg +
          geom_point(pca_x, 
                     mapping = aes(x = get(x_axis), y = get(y_axis),
                                   color = cluster, 
                                   fill  = cluster, 
                                   shape = cluster), 
                     size = 3)
      }
      if (USE_AXES == TRUE) { # if USE_AXES == TRUE then draw axes
        # axis segments
        gg <- gg +
          geom_segment(pca_rot,
                       mapping = aes(x = get(x_axis), xend = zero[, 1],
                                     y = get(y_axis), yend = zero[, 2]),
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
      
      x_range <- max(pca_x[, 1], circ[, 1]) - min(pca_x[, 1], circ[, 1])
      y_range <- max(pca_x[, 2], circ[, 2]) - min(pca_x[, 2], circ[, 2])
      # Options 
      gg <- gg + theme_minimal() +
        scale_color_brewer(palette = pal) +
        theme(panel.grid.major = element_blank(), # no grid lines
              panel.grid.minor = element_blank(), # no grid lines
              axis.text.x  = element_blank(),     # no axis marks
              axis.text.y  = element_blank(),     # no axis marks
              axis.title.x = element_text(size = 22, face = "bold"),
              axis.title.y = element_text(size = 22, face = "bold"),
              aspect.ratio = y_range / x_range,
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
      return(600)
    } else return(1) 
  }
  gtour_plot <- reactive({ 
    if (grand_active() == TRUE) {
      # data init
      dat <- dat()
      dat_std <- tourr::rescale(dat)
      cluster <- attributes(dat)$cluster
      
      # tour init
      angle <- .1
      fps   <- 6
      max_frames <- 90 # 90 frame for 15 sec @ fps = 6
      set.seed(123) # if tourr starts using seeds
      
      tpath <- task_tpath()
      
      full_path <- tourr::interpolate(basis_set = tpath, angle = angle)
      attr(full_path, "class") <- "array"
      max_frames <- min(c(max_frames, dim(full_path)[3]))
      full_path <- full_path[, , 1:max_frames]
      
      tour_df <- array2df(array = full_path, data = dat_std) # to long form df
      
      # render init
      pal <- "Dark2"
      axes_position <- "left"
      USE_AXES <- TRUE
      USE_AES  <- TRUE
      if (task() == 1) {
        if(rv$training_aes == FALSE) { # During training
          USE_AES <- FALSE
        } 
      }
      if (USE_AXES == FALSE) {axes_position = "off"}
      angle <- seq(0, 2 * pi, length = 360)
      circ  <- app_set_axes_position(data.frame(x = cos(angle), y = sin(angle)),
                                     axes_position)
      zero  <- app_set_axes_position(0, axes_position)
      
      ### ggplot2
      basis_df <- tour_df$basis_slides
      basis_df[, 1:2] <- app_set_axes_position(tour_df$basis_slides[, 1:2], axes_position)
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
                       mapping = aes(x = x, xend = zero[, 1],
                                     y = y, yend = zero[, 2],
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
      
      x_range <- max(data_df[, 1], circ[, 1]) - min(data_df[, 1], circ[, 1])
      y_range <- max(data_df[, 2], circ[, 2]) - min(data_df[, 2], circ[, 2])
      # Options 
      gg <- gg + theme_minimal() +
        scale_color_brewer(palette = pal) +
        scale_fill_brewer(palette = pal) +
        theme(panel.grid.major = element_blank(), # no grid lines
              panel.grid.minor = element_blank(), # no grid lines
              axis.text.x = element_blank(),      # no axis marks
              axis.text.y = element_blank(),      # no axis marks
              axis.title.x = element_blank(),     # no axis titles for gtour
              axis.title.y = element_blank(),     # no axis titles for gtour
              aspect.ratio = y_range / x_range, 
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
      return(600)
    } else return(1) 
  }
  mtour_plot <- reactive({
    if (manual_active()) {
      ### Make rv$manual_ls
      if(length(rv$manual_ls) == 0){
        # data init
        dat <- dat()
        dat_std <- tourr::rescale(dat)
        cluster <- attributes(dat)$cluster
        m_var   <- manip_var()
        
        # slider to phi/theta
        theta <- phi <- NULL
        mv_sp <- create_manip_space(rv$curr_basis, m_var)[m_var, ]
        theta <- atan(mv_sp[2] / mv_sp[1]) # Radial
        phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
        phi_pts <- acos((0:10)/10) # possible angles changes from manip_slider.
        phi_vect <- (phi_pts - phi_start) * - sign(mv_sp[1]) 
        
        for (i in 1:length(phi_vect)){
          rv$basis_ls[[i]] <- oblique_basis(basis = rv$curr_basis, manip_var = manip_var(),
                                            theta = theta, phi = phi_vect[i])
          row.names(rv$basis_ls[[i]]) <- colnames(dat)
        }
        
        # render init
        pal <- "Dark2"
        axes_position <- "left"
        USE_AXES <- TRUE
        USE_AES  <- TRUE
        if (task() == 1) {
          if(rv$training_aes == FALSE) { # During training
            USE_AES  <- FALSE
          } 
        }
        
        for (i in 1:length(rv$basis_ls)){
          if (USE_AES == TRUE) {
            rv$manual_ls[[i]] <- app_oblique_frame(data      = dat_std,
                                                   basis     = rv$basis_ls[[i]],
                                                   manip_var = m_var,
                                                   theta     = 0, 
                                                   phi       = 0,
                                                   cluster   = cluster,
                                                   axes      = axes_position
            )
          } else { # when USE_AES == FALSE
            rv$manual_ls[[i]] <- app_oblique_frame(data      = dat_std,
                                                   basis     = rv$basis_ls[[i]],
                                                   manip_var = m_var,
                                                   theta     = 0, 
                                                   phi       = 0,
                                                   axes      = axes_position
            )
          }
        } 
      } # end of for creating rv$manual_ls
      
      j <- manip_slider_t() * 10 + 1
      if (time_elapsed() > 1)
        rv$curr_basis <- rv$basis_ls[[j]]
      
      rv$manual_ls[[j]]
    } # non-display conditions return nothing.
  })
  
  
  ### ans_tbl() reactive -----
  ans_tbl <- reactive({
    # init columns
    col_factor <- 
      c(rep("training", n_trainings + n_task2_questions * n_trainings),        # training
        rep(this_factor_nm_order[1], n_blocks + n_task2_questions * n_blocks), # tasks across factor
        rep(this_factor_nm_order[2], n_blocks + n_task2_questions * n_blocks),
        rep(this_factor_nm_order[3], n_blocks + n_task2_questions * n_blocks),
        rep("survey", n_survey_questions)                                      # survey
      )
    col_task <- 
      c(rep(1, n_tasks),                                   # training
        rep(2, n_tasks * n_task2_questions),
        rep(c(rep(1, n_blocks ),                           # task 1
              rep(2, n_blocks * n_task2_questions)),       # task 2
            n_factors                                      # across factors
        ),
        paste0("survey", 1:6),                             # survey
        paste0("survey", 7:10, "_", this_factor_nm_order[1]),
        paste0("survey", 7:10, "_", this_factor_nm_order[2]),
        paste0("survey", 7:10, "_", this_factor_nm_order[3])
      )
    col_block <- 
      c("t", "t",                                  # training
        rep("t", n_tasks * n_task2_questions),
        rep(c(1:n_blocks,                          # task 1
              rep(1:n_blocks, n_task2_questions)), # task 2
            n_factors                              # across factors
        ),
        rep(NA, n_survey_questions)                # survey
      )
    sim_set <- c(201, 207, 213,               # task 1
                 rep(202, n_task2_questions), # task 2
                 rep(208, n_task2_questions),
                 rep(214, n_task2_questions)
    )
    col_sim_id <- 
      as.character(
        c("t1", "t2",
          rep("t1", n_task2_questions), # training 1 
          rep("t2", n_task2_questions), # training 2
          sim_set,                      # tasks across factors
          sim_set + 2,
          sim_set + n_task2_questions,
          rep(NA, n_survey_questions)   # survey
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
    
    data.frame(user_uid        = substr(log_name, 5, nchar(log_name)),
               group           = substr(log_name, 5, 5),
               factor          = col_factor,
               task            = col_task,
               block           = col_block,
               sim_id          = col_sim_id,
               question        = col_question,
               pca_inter       = NA,
               manual_inter    = NA,
               resp_inter      = NA,
               plot_elapsed    = NA,
               ttr             = NA,
               response        = NA,
               answer          = NA,
               task_score      = NA,
               clust_score     = NA,
               intensity_score = NA,
               line_score      = NA,
               concern         = NA
    )
  })
  ##### End of reactives
  
  
  ##### Start observes
  ### Obs update axis/task2 choices -----
  observeEvent({
    dat()
    input$factor
  }, { # Init axis choices when data changes
    if (pca_active() == TRUE | manual_active() == TRUE) {
      choices <- paste0("PC", 1:PC_cap)
      updateRadioButtons(session, "x_axis", choices = choices, selected = "PC1")
      updateRadioButtons(session, "y_axis", choices = choices, selected = "PC2")
      loggit("INFO", "Task data changed while axes active; updated PC axes choices.")
    }
    if (task() == 2) {
      choices <- paste0("V", 1:p())
      updateCheckboxGroupInput(session, "tsk2_ans_very_ab",
                               choices = choices, inline  = TRUE)
      updateCheckboxGroupInput(session, "tsk2_ans_some_ab",
                               choices = choices, inline  = TRUE)
      updateCheckboxGroupInput(session, "tsk2_ans_very_bc",
                               choices = choices, inline  = TRUE)
      updateCheckboxGroupInput(session, "tsk2_ans_some_bc",
                               choices = choices, inline  = TRUE)
      loggit("INFO", "Task data changed; updated task 2 responce choices.")
    }
  })
  # Bump x_axis when set to the same as y_axis
  observeEvent(input$x_axis, {
    output$plot_msg <- renderText("")
    if (input$x_axis == input$y_axis) {
      x_axis_out <- NULL
      choices <- paste0("PC", 1:PC_cap)
      x_axis_num <- as.integer(substr(input$x_axis, 3, 3))
      if(x_axis_num <= 3) {x_axis_out <- paste0("PC", x_axis_num + 1)
      } else {x_axis_out <- paste0("PC", x_axis_num - 1)}
      
      updateRadioButtons(session, "x_axis", choices = choices, selected = x_axis_out)
      loggit("INFO", paste0("x_axis set to ", input$x_axis, 
                            ", same as y_axis; x_axis bumped to ", x_axis_out, "."),
             paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
      )
      
      output$plot_msg <- renderText(
        app_html_red(paste0("Please select different principal components. 
                            X axis randomed selected to ", x_axis_out, "."))
      )
    }
  })
  # Bump y_axis when set to the same as x_axis
  observeEvent(input$y_axis, {
    output$plot_msg <- renderText("")
    if (input$x_axis == input$y_axis) {
      y_axis_out <- NULL
      choices <- paste0("PC", 1:PC_cap)
      y_axis_num <- as.integer(substr(input$x_axis, 3, 3))
      if(y_axis_num <= 3) {y_axis_out <- paste0("PC", y_axis_num + 1)
      } else {y_axis_out <- paste0("PC", y_axis_num - 1)}
      
      updateRadioButtons(session, "y_axis", choices = choices, selected = y_axis_out)
      loggit("INFO", paste0("y_axis set to ", input$y_axis, 
                            ", same as x_axis; y_axis bumped to ", y_axis_out, "."),
             paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
      )
      
      output$plot_msg <- renderText(
        app_html_red(paste0("Must select different principal components."))
      )
    }
  })
  
  ### Obs mtour basis ----- 
  observeEvent({
    dat()
    input$x_axis
    input$y_axis
    factor()
    input$factor
  },
  {
    if (manual_active() == TRUE){
      rv$manual_ls <- list()
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
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  
  ### Obs mtour slider ----
  observeEvent(manip_slider_t(), {
    if (manual_active() == TRUE) {
      loggit("INFO", paste0("Slider value changed: ", manip_slider_t()),
             paste0("rv$curr_basis updated: ",
                    paste0(round(rv$curr_basis, 2), e = ", "), 
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  
  ### Obs mtour update manip_var_nm choices -----
  observeEvent({
    dat()
    input$factor
  }, 
  { # Init manip_var_nm choices on data change.
    if (manual_active() == TRUE) {
      these_colnames <- colnames(dat())
      updateSelectInput(session, "manip_var_nm", choices = these_colnames, 
                        selected = these_colnames[1])
      loggit("INFO", paste0("Task data or training factor changed; input$manip_var_nm choices updated."))
    }
  })
  
  ### Obs mtour update slider value -----
  observeEvent(
    {
      manip_var()
      dat()
      factor()
      input$factor
      input$x_axis
      input$y_axis
    }, {
      if(manual_active() == TRUE 
         #& time_elapsed() > 1
         ) {
        rv$manual_ls <- list()
        cat(rv$curr_basis)
        mv_sp <- create_manip_space(rv$curr_basis, manip_var())[manip_var(), ]
        phi_i <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
        .val <- round(cos(phi_i), 1)
        cat(paste0("slider set to :", .val,". \n"))
        updateSliderInput(session, "manip_slider", value = .val)
        loggit("INFO", 
               paste0("New manip slider value (from dat/axes/manip_var)."), 
               paste0("manip_slider: ", .val, 
                      paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
               )
        )
      }
    }
  )
  
  ##### Obs task responses -----
  ### task 1 response & ttr
  observeEvent(input$tsk1_ans, {
    if(time_elapsed() > 1) {
      rv$task_ttr[1] <- time_elapsed()
      rv$task_response[1] <- input$tsk1_ans
      loggit("INFO", "Task 1 entered.", 
             paste0("Response: ", rv$task_response[1], 
                    ". ttr: ", rv$task_ttr[1], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  ### task 2 responses & ttr
  observeEvent(input$tsk2_ans_very_ab, {
    if(time_elapsed() > 1) {
      rv$task_ttr[1] <- time_elapsed()
      rv$task_response[1] <- paste(input$tsk2_ans_very_ab, collapse = ", ")
      loggit("INFO", "Task 2, very important, clusters ab response entered.", 
             paste0("Response: ", rv$task_response[1], 
                    ". ttr: ", rv$task_ttr[1], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$tsk2_ans_some_ab, {
    if(time_elapsed() > 1) {
      rv$task_ttr[2] <- time_elapsed()
      rv$task_response[2] <- paste(input$tsk2_ans_some_ab, collapse = ", ")
      loggit("INFO", "Task 2, somewhat important clusters ab response entered.", 
             paste0("Response: ", rv$task_response[2], 
                    ". ttr: ", rv$task_ttr[2], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$tsk2_ans_very_bc, {
    if(time_elapsed() > 1) {
      rv$task_ttr[3] <- time_elapsed()
      rv$task_response[3] <- paste(input$tsk2_ans_very_bc, collapse = ", ")
      loggit("INFO", "Task 2, very important, clusters bc response entered.", 
             paste0("Response: ", rv$task_response[3], 
                    ". ttr: ", rv$task_ttr[3], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$tsk2_ans_some_bc, {
    if(time_elapsed() > 1) {
      rv$task_ttr[4] <- time_elapsed()
      rv$task_response[4] <- paste(input$tsk2_ans_some_bc, collapse = ", ")
      loggit("INFO", "Task 2, somewhat important, clusters bc response entered.", 
             paste0("Response: ", rv$task_response[4], 
                    ". ttr: ", rv$task_ttr[4], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
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
      rv$manual_inter <- rv$manual_inter + 1L
    }
  )
  observeEvent(
    {
      input$tsk1_ans
      input$tsk2_ans_very_ab
      input$tsk2_ans_some_ab
      input$tsk2_ans_very_bc
      input$tsk2_ans_some_bc
      
    }, {
      rv$resp_inter <- rv$resp_inter + 1L
    }
  )
  
  ##### Obs survey answers -----
  observeEvent(input$survey1, {
    if(time_elapsed() > 1) {
      i <- 1
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey1
      loggit("INFO", "Survey 1 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey2, {
    if(time_elapsed() > 1) {
      i <- 2
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey2
      loggit("INFO", "Survey 2 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey3, {
    if(time_elapsed() > 1) {
      i <- 3
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey3
      loggit("INFO", "Survey 3 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey4, {
    if(time_elapsed() > 1) {
      i <- 4
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey4
      loggit("INFO", "Survey 4 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey5, {
    if(time_elapsed() > 1) {
      i <- 5
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey5
      loggit("INFO", "Survey 5 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey6, {
    if(time_elapsed() > 1) {
      i <- 6
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey6
      loggit("INFO", "Survey 6 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey7, {
    if(time_elapsed() > 1) {
      i <- 7
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey7
      loggit("INFO", "Survey 7 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey8, {
    if(time_elapsed() > 1) {
      i <- 8
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey8
      loggit("INFO", "Survey 8 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey9, {
    if(time_elapsed() > 1) {
      i <- 9
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey9
      loggit("INFO", "Survey 9 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey10, {
    if(time_elapsed() > 1) {
      i <- 10
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey10
      loggit("INFO", "Survey 10 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey11, {
    if(time_elapsed() > 1) {
      i <- 11
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey11
      loggit("INFO", "Survey 11 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey12, {
    if(time_elapsed() > 1) {
      i <- 12
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey12
      loggit("INFO", "Survey 12 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey13, {
    if(time_elapsed() > 1) {
      i <- 13
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey13
      loggit("INFO", "Survey 13 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey14, {
    if(time_elapsed() > 1) {
      i <- 14
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey14
      loggit("INFO", "Survey 14 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey15, {
    if(time_elapsed() > 1) {
      i <- 15
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey15
      loggit("INFO", "Survey 15 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey16, {
    if(time_elapsed() > 1) {
      i <- 16
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey16
      loggit("INFO", "Survey 16 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  observeEvent(input$survey17, {
    if(time_elapsed() > 1) {
      i <- 17
      rv$task_ttr[i] <- time_elapsed()
      rv$task_response[i] <- input$survey17
      loggit("INFO", "Survey 17 entered.", 
             paste0("Response: ", rv$task_response[i], 
                    ". ttr: ", rv$task_ttr[i], ".",
                    paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", ")
             )
      )
    }
  })
  
  ### Obs next page button -----
  observeEvent(input$next_pg_button, {
    if ((rv$stopwatch > 2 & is_logging == TRUE) | is_logging == FALSE){
      # Init rv$ans_tbl <- ans_tbl() first press
      if (is.null(rv$ans_tbl)){ rv$ans_tbl <- ans_tbl() }
      # if <on last task> {<do nothing>}. Also shouldn't be visible
      if (rv$pg >= survey_start){ return() }
      
      ### _Training evaluation -----
      # If training section, evaluate response
      if (section() == "training") {
        this_msg <- ""
        # Evaluate training task 1 
        if (task() == 1 & rv$training_aes == FALSE) {
          rv$training_aes <- TRUE
          ans   <- task1_ans()
          delta <- input$tsk1_ans - task1_ans()
          main_msg <- 
            "For PCA make sure to plot several components. 
            Using the grand tour look for groups of points moving together. 
            In the manual tour choose the variables with the largest axes 
            sequentially to manipulate their contribution to the projection."
          
          if (delta >= 2){ # >= 2 clusters too high, retry
            rv$second_training <- TRUE
            this_msg <- paste0("That is little high, this data has ", ans, " clusters. ")
            if(section_pg() %in% c(2, 4)) {this_msg <- 
              paste0(this_msg, "Try again on another training set. ")}
          }
          if (delta <= -2){ # <= 2 clusters too low, retry
            rv$second_training <- TRUE
            this_msg <- paste0("That is little low, this data has ", ans, " clusters. ")
            if(section_pg() %in% c(2, 4)) {this_msg <- 
              paste0(this_msg, "Try again on another training set. ")}
          }
          if (abs(delta) == 1){ # within 1 cluster, passes
            if (section_pg() %in% c(2, 4)) rv$second_training <- "ask"
            this_msg <- paste0("Close, this data has ", ans, " clusters. 
            You have the right idea. As a reminder, ")
          }
          if (delta == 0){ # exact answer, passes
            if (section_pg() %in% c(2, 4))  rv$second_training <- "ask"
            this_msg <- paste0("That's correct, this data has ", ans, " clusters.
            As a reminder, ")
          }
          output$plot_msg <- renderText(
            app_html_red(paste0(this_msg, main_msg))
          )
          return()
        }
        
        # Evaluation of the training for tasks 2.
        if (task() == 2 & rv$training_aes == FALSE) {
          rv$training_aes <- TRUE
          score <- task2_score()
          bar   <- -6
          
          if (score < bar){ # score not passing
            rv$second_training <- TRUE
            this_msg <- 
              "That seems a little off. Remember that the importance of a 
              variable for distinguishing a group is related to variables in a 
              separating direction with large magnitudes in the projection, but 
              variables with small contributions cannot be ruled out, multiple 
              projections most be looked at."
          }
          if (score >= bar){ # score is passing
            rv$second_training <- "ask"
            this_msg <- 
              "Very good! As a reminder, the importance of a variable for
              distinguishing a group is related to variables in a separating 
              direction with large magnidutes in the projection, but variables 
              with small contributions cannot be ruled out, multiple 
              projections most be looked at."
          }
          output$plot_msg <- renderText(
            app_html_red(this_msg)
          )
          return()
            
        }
      } # end of training section evaluation
      
      # Write reponses and ttr to ans_tbl
      ### _rv$ans_tbl -----
      if (section() == "task" |
          (section() == "training" & task() %in% 1:2)) {
        .ins_row_start <- which(rv$ans_tbl$factor  == factor() &
                                  rv$ans_tbl$task  == task() &
                                  rv$ans_tbl$block == block())[1]
        .ins_row_end   <- .ins_row_start + length(rv$task_response) - 1
        .rows          <- .ins_row_start:.ins_row_end
        
        # Is response concerning?
        .task_concern <- "no"
        if (NA %in% rv$task_ttr){
          .task_concern <- "YES, ttr contains NA. REMOVE."}
        if (min(rv$task_ttr) == "(default)" & !is.na(time_elapsed())){
          .task_concern <- "Yes, ttr defaulted. Consider removing."}
        if (max(rv$task_ttr) > task_time() + 30 & 
            !is.na(time_elapsed()) & factor() == "task"){
          .task_concern <- "YES, time elaspsed. REMOVE."}
        if (task() == 2 & !is.na(time_elapsed())){
          ab_inter <- intersect(input$tsk2_ans_very_ab, input$tsk2_ans_some_ab)
          ab_inter_len <- length(ab_inter)
          if (ab_inter_len > 0){
            .task_concern <- "Some, task2 ab contains same var. 'somewhat' unselected by default"
            ab_orig <- input$tsk2_ans_some_ab
            ab_update <- ab_orig[!ab_orig %in% ab_inter]
            .choices <- paste0("V", 1:p())
            updateCheckboxGroupInput(session, "tsk2_ans_some_ab", 
                                     choices = .choices, selected = ab_update)
          }
          bc_inter <- intersect(input$tsk2_ans_very_bc, input$tsk2_ans_very_bc)
          bc_inter_len <- length(bc_inter)
          if (bc_inter_len > 0){
            .task_concern <- "Some, task2 bc contains same var. 'somewhat' unselected by default"
            bc_orig <- input$tsk2_ans_some_ab
            bc_update <- bc_orig[!bc_orig %in% bc_inter]
            .choices <- paste0("V", 1:p())
            updateCheckboxGroupInput(session, "tsk2_ans_some_ab", 
                                     choices = .choices, selected = bc_update)
          }
        }
        
        # task responses
        .task_answer <- .task_score <- .line_score <- .clust_score <- 
          .intensity_score<- NA
        if(task() == 1){
          .task_answer[1] <- task1_ans()
          .task_score[1]  <- task1_score()
        } 
        if(task() == 2){
          .vars_ls  <- list(task2_vars_very_ab(), task2_vars_some_ab(),
                            task2_vars_very_bc(), task2_vars_some_bc())
          .score_ls <- list(task2_score_very_ab(), task2_score_some_ab(),
                            task2_score_very_bc(), task2_score_some_bc())
          for (i in 1:4) {
            .task_answer[i] <- app_vect2str(.vars_ls[[i]])
            .line_score[i]  <- .score_ls[[i]]
          }
          for (i in 1:2){ # 1&2, 3&4  # 1&3, 2&4 
            .clust_score[i]      <- .line_score[2 * i - 1] + .line_score[2 * i]
            .intensity_score[i]  <- .line_score[i] + .line_score[i + 2]
          }
            .task_score <- sum(.line_score)
        }
        
        # Did task time run out
        .plot_elapsed <- NA
        if (section() == "task"){
          if (time_elapsed() >  task_time()) .plot_elapsed <- 1
          if (time_elapsed() <= task_time()) .plot_elapsed <- 0
        }
        
        
        rv$ans_tbl$pca_inter[.rows]       <- rv$pca_inter
        rv$ans_tbl$manual_inter[.rows]    <- rv$manual_inter
        rv$ans_tbl$resp_inter[.rows]      <- rv$resp_inter
        rv$ans_tbl$plot_elapsed[.rows]    <- .plot_elapsed
        rv$ans_tbl$ttr[.rows]             <- rv$task_ttr
        rv$ans_tbl$response[.rows]        <- rv$task_response
        rv$ans_tbl$answer[.rows]          <- .task_answer
        rv$ans_tbl$score[.rows]           <- .task_score
        rv$ans_tbl$line_score[.rows]      <- .line_score
        rv$ans_tbl$clust_score[.rows]     <- "dont trust" #.clust_score
        rv$ans_tbl$intensity_score[.rows] <- "dont trust" #.intensity_score
        rv$ans_tbl$concern[.rows]         <- .task_concern
      } # End of writing to ans_tbl
      
      ### _New page ----
      # if second training not needed, skip a page.
      if (section() == "training" & block() %in% c(2, 4) &
          !(rv$second_training == TRUE | input$second_training == TRUE)) {
        rv$pg <- rv$pg + 1
      }
      rv$pg <- rv$pg + 1
      
      # Reset responses, ttr, and timer for next task
      output$plot_msg     <- renderText("")
      rv$pca_inter        <- 1L
      rv$manual_inter     <- 1L
      rv$resp_inter       <- 1L
      rv$task_response    <- NULL
      rv$task_ttr    <- NULL
      rv$task_answer      <- NULL
      rv$task_score       <- NULL
      rv$timer            <- task_time()
      rv$stopwatch        <- 0
      rv$timer_active     <- TRUE
      rv$training_aes     <- FALSE
      rv$second_training  <- FALSE
      updateCheckboxInput(session, "second_training", value = FALSE)
      
      # Clear task 1 response
      if (section() %in% c("task", "training") & task() == 1) {
          updateNumericInput(session, "tsk1_ans", "",
                             value = 0, min = 0, max = 10)
      }
      
      # Set structure for writeing to ans_tbl
      n_rows <- 0
      if (task() == 1){n_rows <- 1} 
      if (task() == 2){n_rows <- 4}
      if (section() == "survey") {n_rows <- 1}
      def <- "default"
      if (task() == 1){def <- "0 (default)"}
      if (task() == 2){def <- "none (default)"}
      if (section() == "survey") {def <- c(rep("decline to answer (default)", 3),
                                              rep("5 (default)", n_survey_questions - 3))}
      rv$task_response <- rep(def, n_rows)
      rv$task_ttr <- rep("(default)", n_rows)
      loggit("INFO", paste0("Next page: section ", section(), 
                            ", section page ", section_pg()),
             paste0("rv$pg: ", rv$pg, 
                    ". section(): ", section(),
                    ". period(): ", period(),
                    ". factor(): ", factor(),
                    ". task(): ", task(),
                    ". block(): ", block(), 
                    ". Wrote previous responses to rv$ans_tbl."))
    }
  })
  
  
  ### Obs save reponses button -----
  observeEvent(input$save_ans, {
    filebase = paste("responses", this_group, Sys.info()[4], sep = "_")
    prefix = ""
    
    # Write survey responses to rv$ans_tbl
    ins_row_start <- nrow(rv$ans_tbl) - n_survey_questions + 1
    ins_row_end   <- nrow(rv$ans_tbl)
    rv$ans_tbl$response[ins_row_start:ins_row_end] <- rv$task_response
    rv$ans_tbl$ttr[ins_row_start:ins_row_end] <- rv$task_ttr
    
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
    assign(save_name, rv$ans_tbl)
    write.csv(get(save_name), file = save_file, row.names = FALSE)
    rv$save_file <- save_file
    
    save_msg <- paste0("Reponses saved as ", save_file, " (log file: ", log_file, "). Thank you for participating!")
    output$save_msg <- renderText(app_html_red(save_msg))
    
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
    invalidateLater(1000, session)
    isolate({
      rv$timer     <- rv$timer - 1
      rv$stopwatch <- rv$stopwatch + 1
    })
    if(rv$timer < 0 & section() == "task" & rv$timer_active == TRUE){
      rv$timer_active <- FALSE
      loggit("INFO", "Timer elapsed.", 
             paste("factor,task,block,period:@", factor(), task(), block(), period(), sep = ", "))
    }
  })
  
  
  ##### Outputs -----
  output$timer_disp <- renderText({
    if (section() == "task") { # disp timer if not an intro page.
      if (rv$timer < 1) {return("Time has expired, please enter your best guess and proceed.")
      } else {return(paste0("Time left: ", lubridate::seconds_to_period(rv$timer)))}
    }
  })
  output$stopwatch_disp <- renderText({
    if (section() == "training" & block() != 6) { # disp timer if not an intro page.
      return(paste0("Time elapsed this task: ", lubridate::seconds_to_period(rv$stopwatch)))
    }
  })
  
  ### Controls ui coditionalPanels 
  output$is_saved        <- reactive(if (is.null(rv$save_file)) {0} else {1}) # control save_msg.
  output$pg              <- reactive(rv$pg)    # for hiding ui next_task button
  output$section         <- reactive(section()) # for ui between sections
  output$factor          <- reactive(factor())     # for sidebar inputs
  output$task            <- reactive(task())   # for titles, and response inputs
  output$block           <- reactive(block())  # for training ui
  output$section_pg      <- reactive(section_pg())   # for navigating training
  output$second_training <- reactive(rv$second_training) # for more training button
  
  outputOptions(output, "is_saved",        suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  outputOptions(output, "pg",              suspendWhenHidden = FALSE)
  outputOptions(output, "section",         suspendWhenHidden = FALSE)
  outputOptions(output, "factor",          suspendWhenHidden = FALSE)
  outputOptions(output, "task",            suspendWhenHidden = FALSE)
  outputOptions(output, "block",           suspendWhenHidden = FALSE)
  outputOptions(output, "section_pg",      suspendWhenHidden = FALSE)
  outputOptions(output, "second_training", suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  
  ### General task outputs
  output$task_header     <- renderText(task_header())
  output$pca_plot        <- renderPlot({pca_plot()}, height = pca_height)
  output$gtour_plot      <- renderPlotly({suppressWarnings(gtour_plot())})
  output$mtour_plot      <- renderPlot({mtour_plot()}, height = mtour_height)
  output$ans_tbl         <- renderTable({rv$ans_tbl})
  output$task2_ans_ptile <- renderPrint({task2_ans_ptile()})
  output$task2_ans       <- renderPrint({task2_ans()})
  output$task2_score     <- renderPrint({task2_score()})
  
  
  ### Dev msg -----
  output$dev_msg <- renderPrint(cat("dev msg -- ", "\n",
                                    "rv$this_sign: ", rv$this_sign, "\n",
                                    "this ans_tbl row: ", factor(), "|", task(), block(), "\n",
                                    "rv$pg: ", rv$pg, "\n",
                                    "section() ", section(), "\n",
                                    "section_pg() ", section_pg(), "\n",
                                    "factor(): ", factor(), "\n",
                                    "task(): ", task(), "\n",
                                    "block(): ", block(), "\n",
                                    "rv$timer: ", rv$timer, "\n",
                                    sep = ""))
  }
  
  ### Combine as shiny app.
  shinyApp(ui = ui, server = server)
  
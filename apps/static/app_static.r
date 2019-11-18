source('global_static.r', local = TRUE)


##### Server function, dynamic outputs ----
server <- function(input, output, session) {  ### INPUT, need to size to number of reps
  loggit("INFO", "app has started", "spinifex_study")
  
  ### Initialization -----
  rv                <- reactiveValues()
  rv$pg_num         <- 1
  rv$timer          <- 120
  rv$timer_active   <- TRUE
  rv$task_responses <- NULL
  rv$task_durations <- NULL
  rv$save_file      <- NULL
  rv$ans_tbl        <- NULL
  rv$training_passes   <- FALSE
  rv$training_attempts <- 1
  
  ##### Start reactives
  p1 <- reactive({ ncol(s_dat[[1]]) })
  p2 <- reactive({ ncol(s_dat[[2]]) })
  p3 <- reactive({ ncol(s_dat[[3]]) })
  p_sims <- reactive({ p1() + p2() + p3() })
  section_num <- reactive({
    if (rv$pg_num >= training_start & rv$pg_num < task_start){ # in training section
      return(rv$pg_num - (training_start - 1))
    }
    if (rv$pg_num >= task_start & rv$pg_num < survey_start){ # in task section
      return(rv$pg_num - (task_start - 1))
    }
    return(1) # dummy 1, NA and 999 cause other issues.
  })
  block_num <- reactive({
    if (ui_section() == "training") {return(section_num())
    } else {
      return(1 + (section_num() - 1) %/% n_reps)
    }
  })
  rep_num <- reactive({
    if (ui_section() == "training") {return(section_num())}
    if (section_num() - (n_reps * (block_num() - 1)) <= 0) {stop("check rep_num() it's <= 0.")}
    return(section_num() - (n_reps * (block_num() - 1)))
  })
  blockrep <- reactive({
    paste0(s_blocks[block_num()], rep_num())
  })
  task_dat <- reactive({
    if (ui_section() == "training") {sim_intro
    } else {
      ret <- s_dat[[rep_num()]]
      colnames(ret) <- paste0("V", 1:ncol(ret))
      return(ret) 
    }
  })
  ui_section <- reactive({
    if (rv$pg_num == 1) {return("intro")}
    if (rv$pg_num %in% training_start:(task_start - 1) ) {return("training")}
    if (rv$pg_num %in% task_start:(survey_start - 1) ) {return("task")}
    if (rv$pg_num == survey_start) {return("survey")} 
    return("passed survey, need to cap pg_num")
  })
  
  
  ### PCA Plot -----
  task_pca <- reactive({
    if (rv$timer_active | ui_section() == "training") {
      dat <- task_dat()
      dat_std <- tourr::rescale(dat)
      pca <- prcomp(dat_std)
      
      if (block_num() == 1) {
        if(rv$training_passes == FALSE) { # During training
          col = "black"
          pch = 16
        } else { # Training already passed
          col <- col_of(attributes(dat)$cluster)
          pch <- pch_of(attributes(dat)$cluster)
        }
        axes_position <- "off"
      }
      if (block_num() == 2){
        col <- col_of(attributes(dat)$cluster)
        pch <- pch_of(attributes(dat)$cluster)
        axes_position <- "center"
      }
      if (block_num() == 3) {
        col <- col_of(attributes(dat)$cluster, pallet_name = "Paired")
        pch <- pch_of(attributes(dat)$cluster)
        axes_position <- "center"
      }
      if (block_num() != 2) { # not 2nd block.
        pca_x <- data.frame(2 * (tourr::rescale(pca$x) - .5))
        pca_rotation <- set_axes_position(data.frame(t(pca$rotation)),
                                          axes_position)
      } else { # is 2nd block, change sign of var map.
        pca_x <- as.matrix(dat) %*% (-1 * pca$rotation)
        pca_x <- data.frame(2 * (tourr::rescale(pca_x) - .5))
        pca_rotation <- set_axes_position(data.frame(t(-1 * pca$rotation)),
                                          axes_position)
      }
      
      pca_pct_var <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
      pca_x_axis <- input$x_axis
      pca_y_axis <- input$y_axis
      rot_x_axis <- paste0("V", substr(pca_x_axis,3,3))
      rot_y_axis <- paste0("V", substr(pca_y_axis,3,3))
      x_lab <- paste0(input$x_axis, " (",
                      pca_pct_var[as.integer(substr(pca_x_axis,3,3))], "% Var)")
      y_lab <- paste0(input$y_axis, " (",
                      pca_pct_var[as.integer(substr(pca_y_axis,3,3))], "% Var)")
      
      angle <- seq(0, 2 * pi, length = 360)
      circ  <- set_axes_position(data.frame(x = cos(angle), y = sin(angle)),
                                axes_position)
      zero  <- set_axes_position(0, axes_position)
      
      ret <- ggplot() +
        # data points
        geom_point(pca_x, mapping = aes(x = get(pca_x_axis),
                                        y = get(pca_y_axis)),
                   color = col, fill = col, shape = pch, size = 3)
      
        if (axes_position != "off") {
          # axis segments
          ret <- ret +
            geom_segment(pca_rotation,
                         mapping = aes(x = get(rot_x_axis), xend = zero,
                                       y = get(rot_y_axis), yend = zero),
                         size = .3, colour = "red") +
            # axis label text
            geom_text(pca_rotation,
                      mapping = aes(x = get(rot_x_axis),
                                    y = get(rot_y_axis),
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
        scale_color_brewer(palette = "Dark2") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_blank(), # marks
              axis.text.y = element_blank(), # marks
              axis.title.x = element_text(size = 22, face = "bold"),
              axis.title.y = element_text(size = 22, face = "bold"),
              legend.position = 'none') +
        labs(x = x_lab, y = y_lab)
      
      return(ret)
    }
  })
  
  
  ### Response table -----
  ans_tbl <- reactive({
    col_blockrep <- c(s_blockrep_id[1:3],          # block 1
                      rep(s_blockrep_id[4], p1()), # block 2
                      rep(s_blockrep_id[5], p2()), 
                      rep(s_blockrep_id[6], p3()), 
                      rep(s_blockrep_id[7], p1()), # block 3
                      rep(s_blockrep_id[8], p2()), 
                      rep(s_blockrep_id[9], p3()), 
                      paste0("survey", 1:10)       # survey
    )
    col_var      <- c(rep(NA, 3),   # block 1
                      rep(c(1:p1(), # block 2 & 3
                            1:p2(), 
                            1:p3()), 2),
                      rep(NA, 10))  # survey
    s_sim_id     <- c("001", "002", "003")
    col_sim_id   <- c(s_sim_id,                     # block 1
                      rep(c(rep(s_sim_id[1], p1()), # block 2 & 3
                            rep(s_sim_id[2], p2()), 
                            rep(s_sim_id[3], p3())), 2),
                      rep(NA, 10))                  # survey
    col_question <- c(rep(s_block_questions[1], n_reps),   # block 1
                      rep(s_block_questions[2], p_sims()), # block 2
                      rep(s_block_questions[3], p_sims()), # block 3
                      s_survey_questions)                  # survey 
    col_response <- c(rep(NA, n_reps),        # block 1
                      rep(NA, p_sims()),      # block 2
                      rep(NA, p_sims()),      # block 3
                      rep(NA, 10)) # survey
    col_duration <- c(rep(NA, n_reps),   # block 1
                      rep(NA, p_sims()), # block 2
                      rep(NA, p_sims()), # block 3
                      rep(NA, 10))       # survey 
    data.frame(blockrep = col_blockrep,
               var      = col_var,
               sim_id   = col_sim_id,
               question = col_question,
               response = col_response,
               duration = col_duration)
  })
  ##### End reactive
  
  
  ##### Start observes
  ### Obs update axis choices -----
  observeEvent(task_dat(), { # Init axis choices when data changes
    p <- ncol(task_dat())
    choices <- paste0("PC", 1:p)
    updateRadioButtons(session, "x_axis", choices = choices, selected = "PC1")
    updateRadioButtons(session, "y_axis", choices = choices, selected = "PC2")
    loggit("INFO", "task_dat() changed.", "updated axes choices.")
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
  
  ### Obs responses and durations -----
  ##### Block 1 responses & duration
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
  ##### Block 3 responses & duration
  observeEvent(input$blk3_ans1, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[1]  <- input$blk3_ans1
      rv$task_durations[1]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 3, input 1 entered.", 
             paste0("Response: ",   rv$task_responses[1], 
                    ". Duration: ", rv$task_durations[1], "."))
    }
  })
  observeEvent(input$blk3_ans2, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[2]  <- input$blk3_ans2
      rv$task_durations[2]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 3, input 2 entered.", 
             paste0("Response: ",   rv$task_responses[2], 
                    ". Duration: ", rv$task_durations[2], "."))
    }
  })
  observeEvent(input$blk3_ans3, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[3]  <- input$blk3_ans3
      rv$task_durations[3]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 3, input 3 entered.", 
             paste0("Response: ",   rv$task_responses[3], 
                    ". Duration: ", rv$task_durations[3], "."))
    }
  })
  observeEvent(input$blk3_ans4, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[4]  <- input$blk3_ans4
      rv$task_durations[4]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 3, input 4 entered.", 
             paste0("Response: ",   rv$task_responses[4], 
                    ". Duration: ", rv$task_durations[4], "."))
    }
  })
  observeEvent(input$blk3_ans5, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[5]  <- input$blk3_ans5
      rv$task_durations[5]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 3, input 5 entered.", 
             paste0("Response: ",   rv$task_responses[5], 
                    ". Duration: ", rv$task_durations[5], "."))
    }
  })
  observeEvent(input$blk3_ans6, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[6]  <- input$blk3_ans6
      rv$task_durations[6]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 3, input 6 entered.", 
             paste0("Response: ",   rv$task_responses[6], 
                    ". Duration: ", rv$task_durations[6], "."))
    }
  })
  observeEvent(input$blk3_ans7, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[7]  <- input$blk3_ans7
      rv$task_durations[7]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 3, input 7 entered.", 
             paste0("Response: ",   rv$task_responses[7], 
                    ". Duration: ", rv$task_durations[7], "."))
    }
  })
  observeEvent(input$blk3_ans8, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[8]  <- input$blk3_ans8
      rv$task_durations[8]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 3, input 8 entered.", 
             paste0("Response: ",   rv$task_responses[8], 
                    ". Duration: ", rv$task_durations[8], "."))
    }
  })
  observeEvent(input$blk3_ans9, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[9]  <- input$blk3_ans9
      rv$task_durations[9]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 3, input 9 entered.", 
             paste0("Response: ",   rv$task_responses[9], 
                    ". Duration: ", rv$task_durations[9], "."))
    }
  })
  observeEvent(input$blk3_ans10, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[10] <- input$blk3_ans10
      rv$task_durations[10] <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 3, input 10 entered.", 
             paste0("Response: ",   rv$task_responses[10], 
                    ". Duration: ", rv$task_durations[10], "."))
    }
  })
  observeEvent(input$blk3_ans11, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[11] <- input$blk3_ans11
      rv$task_durations[11] <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 3, input 11 entered.", 
             paste0("Response: ",   rv$task_responses[11], 
                    ". Duration: ", rv$task_durations[11], "."))
    }
  })
  observeEvent(input$blk3_ans12, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[12] <- input$blk3_ans12
      rv$task_durations[12] <- as.integer(120 - rv$timer)
      loggit("INFO", "Block 3, input 12 entered.", 
             paste0("Response: ",   rv$task_responses[12], 
                    ". Duration: ", rv$task_durations[12], "."))
    }
  })
  ##### Survey responses & duration
  observeEvent(input$ans_gender, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[1]  <- input$ans_gender
      rv$task_durations[1]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Survey, input 1 entered.", 
             paste0("Response: ",   rv$task_responses[1], 
                    ". Duration: ", rv$task_durations[1], "."))
    }
  })
  observeEvent(input$ans_age, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[2]  <- input$ans_age
      rv$task_durations[2]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Survey, input 2 entered.", 
             paste0("Response: ",   rv$task_responses[2], 
                    ". Duration: ", rv$task_durations[2], "."))
    }
  })
  observeEvent(input$ans_edu, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[3]  <- input$ans_edu
      rv$task_durations[3]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Survey, input 3 entered.", 
             paste0("Response: ",   rv$task_responses[3], 
                    ". Duration: ", rv$task_durations[3], "."))
    }
  })
  observeEvent(input$ans_ease, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[4]  <- input$ans_ease
      rv$task_durations[4]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Survey, input 4 entered.", 
             paste0("Response: ",   rv$task_responses[4], 
                    ". Duration: ", rv$task_durations[4], "."))
    }
  })
  observeEvent(input$ans_confidence, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[5]  <- input$ans_confidence
      rv$task_durations[5]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Survey, input 5 entered.", 
             paste0("Response: ",   rv$task_responses[5], 
                    ". Duration: ", rv$task_durations[5], "."))
    }
  })
  observeEvent(input$ans_understand, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[6]  <- input$ans_understand
      rv$task_durations[6]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Survey, input 6 entered.", 
             paste0("Response: ",   rv$task_responses[6], 
                    ". Duration: ", rv$task_durations[6], "."))
    }
  })
  observeEvent(input$ans_use, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[7]  <- input$ans_use
      rv$task_durations[7]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Survey, input 7 entered.", 
             paste0("Response: ",   rv$task_responses[7], 
                    ". Duration: ", rv$task_durations[7], "."))
    }
  })
  observeEvent(input$ans_high_dim, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[8]  <- input$ans_high_dim
      rv$task_durations[8]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Survey, input 8 entered.", 
             paste0("Response: ",   rv$task_responses[8], 
                    ". Duration: ", rv$task_durations[8], "."))
    }
  })
  observeEvent(input$ans_data_vis, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[9]  <- input$ans_data_vis
      rv$task_durations[9]  <- as.integer(120 - rv$timer)
      loggit("INFO", "Survey, input 9 entered.", 
             paste0("Response: ",   rv$task_responses[9], 
                    ". Duration: ", rv$task_durations[9], "."))
    }
  })
  observeEvent(input$ans_previous_knowledge, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[10] <- input$ans_previous_knowledge
      rv$task_durations[10] <- as.integer(120 - rv$timer)
      loggit("INFO", "Survey, input 10 entered.", 
             paste0("Response: ",   rv$task_responses[10], 
                    ". Duration: ", rv$task_durations[10], "."))
    }
  })
  
  
  ### Obs next page button -----
  observeEvent(input$next_pg_button, {
    ### ON LAST PAGE:
    # if <on last task> {<do nothing>}
    if (rv$pg_num >= survey_start){ return() }
    # Init rv$ans_tbl <- ans_tbl() on first press
    if (rv$pg_num == 1){ rv$ans_tbl <- ans_tbl() }
    
    # If training section, evaluate response 
    if (ui_section() == "training" & rv$training_passes == FALSE) {
      # Evaluate training block 1
      if (block_num() == 1) {
        if (rv$training_attempts >= 3) {
          output$plot_msg <- renderText("<b>Please see the proctor for additional instruction.</b>") 
          rv$training_attempts <- rv$training_attempts + 1
          return()
        }
        response <- input$blk1_ans
        ans <- length(attr(sim_intro, "ncl"))
        
        if (response - ans >= 2){ # >= 2 high, retry
          output$plot_msg <- renderText("<b>That seems a little high, make sure to check from multiple perspectives and give it another shot.</b>") 
          rv$training_attempts <- rv$training_attempts + 1
          return()
        }
        if (response - ans <= -2){ # <= 2 low, retry
          output$plot_msg <- renderText("<b>That seems a little low, make sure to check from multiple perspectives and give it another shot.</b>") 
          rv$training_attempts <- rv$training_attempts + 1
          return()
        }
        if (abs(response - ans) == 1){ # within 1, pass
          output$plot_msg <- renderText(
            paste0("<b>Close, there are actually ", ans, " clusters in the data, but you have the right idea.</b>") 
          )
          rv$training_passes <- TRUE
          ##TODO: display supersived graphic.
            return()
        }
        
        if (response == ans){ # exact, pass
          output$plot_msg <- renderText("<b>That's correct, great job!</b>") 
          rv$training_passes <- TRUE
          ##TODO: display supersived graphic.
          return()
        }
      }
      if (block_num() == 2) {
        output$plot_msg <- renderText("<b>Training block 2 answer TBD.</b>") 
        rv$training_passes <- TRUE
        return()
      }
      if (block_num() == 3) {
        output$plot_msg <- renderText("<b>Training block 3 answer TBD.</b>") 
        rv$training_passes <- TRUE
        return()
      }
    }
    
    # If task section, write reponses and duration to ans_tbl
    if (ui_section() == "task") {
      ins_row <- which(rv$ans_tbl$blockrep == blockrep())[1] # first row of this blockrep.
      ins_nrows <- length(rv$task_responses) - 1
      rv$ans_tbl[ins_row:(ins_row + ins_nrows), 5] <- rv$task_responses
      rv$ans_tbl[ins_row:(ins_row + ins_nrows), 6] <- rv$task_durations
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
    
    # Clear task response
    if (ui_section() == "task") {
      if (block_num() == 1) { # reset to same settings.
        updateNumericInput(session, "blk1_ans", "",
                           value = 0, min = 0, max = 10)
      }
    }
    
    # Set structure for responses and durations
    this_p <- 0 
    if (ui_section() == "task") {
      if (block_num() == 1){this_p <- 1
      } else { this_p <- ncol(task_dat()) }
    }
    if (ui_section() == "survey") {this_p <- length(s_survey_questions)}
    
    rv$task_responses <- rep("default", this_p)
    rv$task_durations <- rep("default", this_p)
    loggit("INFO", "Next page: ", 
           paste0("rv$pg_num: ", rv$pg_num, 
                  ". ui_section(): ", ui_section(),
                  ". blockrep(): ", blockrep(), 
                  "."))
  })
  
  ### Obs save reponses button -----
  observeEvent(input$save_ans, {
    # Write survey responses to rv$ans_tbl
    ins_nrows <- length(s_survey_questions) - 1
    ins_row <- nrow(rv$ans_tbl) - ins_nrows
    rv$ans_tbl[ins_row:(ins_row + ins_nrows), 5] <- rv$task_responses
    rv$ans_tbl[ins_row:(ins_row + ins_nrows), 6] <- rv$task_durations
    
    # Write rv$ans_tbl to .csv file.
    df <- rv$ans_tbl
    if (!is.null(rv$save_file)){ # if save already exists 
      save_msg <- paste0("<b>Reponses already saved as ", 
                         rv$save_file, ".</b>")
      output$save_msg <- renderText(save_msg)
      loggit("INFO", "Save button (Previously saved): ", 
             paste0("save_msg: ", save_msg,  "."))
      return()
    }
    
    # Do the actual saving
    save_base <- paste0(".files/response_table_", study_factor)
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
    
    save_msg <- paste0("<b>Reponses saved as ", save_file, 
                       ". Thank you for participating!</b>")
    output$save_msg <- renderText(save_msg)
    
    loggit("INFO", "Save button: ", 
           paste0("rv$save_file: ", rv$save_file, 
                  ". save_msg: ", save_msg,
                  "."))
    return()
  })
  
  ### Obs timer -----
  observe({
    invalidateLater(1000, session)
    isolate({
      if(rv$timer_active)
      {
        rv$timer <- rv$timer - 1
        if(rv$timer < 1 & rv$timer_active == TRUE){
          rv$timer_active <- FALSE
          loggit("INFO", "timer elapsed.", "")
        }
      }
    })
  })
  
  ##### Outputs -----
  output$timer_disp <- renderText({
    if (ui_section() == "task") { # disp timer if not an intro page.
      if (rv$timer < 1) {return("Time has expired, please enter your best guess and proceed.")
      } else {paste0("Time left: ", lubridate::seconds_to_period(rv$timer))}
    } else {return()}
  })
  ### Controls ui coditionalPanels 
  output$ui_section <- reactive(ui_section())
  output$rep_num    <- reactive(rep_num())   # controls ui wording.
  output$block_num  <- reactive(block_num()) # controls ui response layout
  output$pg_num     <- reactive(rv$pg_num)   # controls ui next_task button
  output$is_saved   <- reactive(if (is.null(rv$save_file)) {0} else {1}) # Control thank you.
  outputOptions(output, "ui_section", suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  outputOptions(output, "rep_num",    suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  outputOptions(output, "block_num",  suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  outputOptions(output, "pg_num",     suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  outputOptions(output, "is_saved",     suspendWhenHidden = FALSE) # eager evaluation for ui conditionalPanel
  ### training outputs
  output$training_header_text <- renderText(training_header_text[block_num()])
  output$training_top_text    <- renderText(training_top_text[block_num()])
  ### general task outputs
  #output$TEST_plot  <- renderPlot(plot(1,1))
  output$task_pca   <- renderPlot({task_pca()}, height = 640) 
  output$task_gtour <- renderPlotly({task_gtour()})
  output$ans_tbl    <- renderTable({rv$ans_tbl})
  

  
  # output$blk1_ans defined in global
  ### Block 2 inputs, rate importance -----
  # ie. output$blk2_ans1 is the value for block 2 question about var 1.
  output$blk2Inputs <- renderUI({
    i <- j <- ncol(task_dat())
    lapply(1:i, function(i) {
      radioButtons(inputId = paste0("blk2_ans", i), label = paste("Variable ", i),
                   choices = c(as.character(1:j), "unimportant"), 
                   selected = "unimportant", inline = TRUE)
    })
  })
  
  ### Block 3 inputs, noise -----
  output$blk3Inputs <- renderUI({
    i <- ncol(task_dat())
    lapply(1:i, function(i) {
        radioButtons(inputId = paste0("blk3_ans", i), label = paste("Variable", i),
                           choices = c("group1", "group2", "group3", "group4", "not correlated"), 
                           selected = "not correlated", inline = TRUE)
    })
  })
  
  
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
    if (try_autosave == TRUE & is.null(rv$save_file)) {
      ## try to trip save Observe.
      input$save_ans <- input$save_ans + 1
    }
  })
}

### Combine as shiny app.
shinyApp(ui = ui, server = server)


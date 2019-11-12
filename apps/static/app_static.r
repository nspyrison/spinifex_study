source('global_static.r', local = TRUE)


##### Server function, dynamic outputs ----
server <- function(input, output, session) {  ### INPUT, need to size to number of reps
  ### Initialization -----
  rv                <- reactiveValues()
  rv$pg_num         <- 1
  rv$timer          <- 120
  rv$timer_active   <- TRUE
  rv$task_responses <- NULL
  rv$task_durations <- NULL
  rv$save_file      <- NULL
  rv$ans_tbl        <- NULL
  
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
    if (section_num() - (n_reps * (block_num() - 1)) <= 0) {browser()}
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
        col = "black"
        pch = 16
        axes_position <- "off"
      }
      if (block_num() == 2){
        col <- col_of(attributes(dat)$cluster)
        pch <- pch_of(attributes(dat)$cluster)
        axes_position <- "center"
      }
      if (block_num() == 3) {
        col <- col_of(attributes(dat)$cluster, pallet_name = "Paired")
        pch <- 3 + pch_of(attributes(dat)$cluster)
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
                   color = col, fill = col, shape = pch)
      
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
  ### Obs axis choices -----
  observe({
    p <- ncol(task_dat())
    choices <- paste0("PC", 1:p)
    updateRadioButtons(session, "x_axis", choices = choices, selected = "PC1")
    updateRadioButtons(session, "y_axis", choices = choices, selected = "PC2")
  })
  observeEvent(input$x_axis, { # But, not the same choices, x axis
    if (input$x_axis == input$y_axis) {
      p <- ncol(task_dat())
      choices <- paste0("PC", 1:p)
      opts <- choices[!choices %in% input$x_axis]
      updateRadioButtons(session, "x_axis", choices = choices, selected = sample(opts, 1))
    }
  })
  observeEvent(input$y_axis, { # But, not the same choices, y axis
    if (input$x_axis == input$y_axis) {
      p <- ncol(task_dat())
      choices <- paste0("PC", 1:p)
      opts <- choices[!choices %in% input$x_axis]
      updateRadioButtons(session, "y_axis", choices = choices, selected = sample(opts, 1))
    }
  })
  
  ### Obs responses and durations -----
  ##### Block 1 responses & duration
  observeEvent(input$blk1_ans, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[1] <- input$blk1_ans
      rv$task_durations[1] <- as.integer(120  - rv$timer)
    }
  })
  ##### Block 2 responses & duration
  observeEvent(input$blk2_ans1, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[1]  <- input$blk2_ans1
      rv$task_durations[1]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk2_ans2, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[2]  <- input$blk2_ans2
      rv$task_durations[2]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk2_ans3, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[3]  <- input$blk2_ans3
      rv$task_durations[3]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk2_ans4, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[4]  <- input$blk2_ans4
      rv$task_durations[4]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk2_ans5, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[5]  <- input$blk2_ans5
      rv$task_durations[5]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk2_ans6, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[6]  <- input$blk2_ans6
      rv$task_durations[6]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk2_ans7, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[7]  <- input$blk2_ans7
      rv$task_durations[7]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk2_ans8, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[8]  <- input$blk2_ans8
      rv$task_durations[8]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk2_ans9, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[9]  <- input$blk2_ans9
      rv$task_durations[9]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk2_ans10, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[10] <- input$blk2_ans10
      rv$task_durations[10] <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk2_ans11, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[11] <- input$blk2_ans11
      rv$task_durations[11] <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk2_ans12, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[12] <- input$blk2_ans12
      rv$task_durations[12] <- as.integer(120 - rv$timer)
    }
  })
  ##### Block 3 responses & duration
  observeEvent(input$blk3_ans1, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[1]  <- input$blk3_ans1
      rv$task_durations[1]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk3_ans2, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[2]  <- input$blk3_ans2
      rv$task_durations[2]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk3_ans3, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[3]  <- input$blk3_ans3
      rv$task_durations[3]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk3_ans4, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[4]  <- input$blk3_ans4
      rv$task_durations[4]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk3_ans5, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[5]  <- input$blk3_ans5
      rv$task_durations[5]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk3_ans6, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[6]  <- input$blk3_ans6
      rv$task_durations[6]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk3_ans7, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[7]  <- input$blk3_ans7
      rv$task_durations[7]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk3_ans8, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[8]  <- input$blk3_ans8
      rv$task_durations[8]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk3_ans9, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[9]  <- input$blk3_ans9
      rv$task_durations[9]  <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk3_ans10, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[10] <- input$blk3_ans10
      rv$task_durations[10] <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk3_ans11, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[11] <- input$blk3_ans11
      rv$task_durations[11] <- as.integer(120 - rv$timer)
    }
  })
  observeEvent(input$blk3_ans12, {
    if((120 - rv$timer) > 1) {
      rv$task_responses[12] <- input$blk3_ans12
      rv$task_durations[12] <- as.integer(120 - rv$timer)
    }
  })
  ##### Survey responses & duration
  observeEvent(input$ans_gender, {
    rv$task_responses[1]  <- input$ans_gender
    rv$task_durations[1]  <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_age, {
    rv$task_responses[2]  <- input$ans_age
    rv$task_durations[2]  <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_edu, {
    rv$task_responses[3]  <- input$ans_edu
    rv$task_durations[3]  <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_ease, {
    rv$task_responses[4]  <- input$ans_ease
    rv$task_durations[4]  <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_confidence, {
    rv$task_responses[5]  <- input$ans_confidence
    rv$task_durations[5]  <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_understand, {
    rv$task_responses[6]  <- input$ans_understand
    rv$task_durations[6]  <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_use, {
    rv$task_responses[7]  <- input$ans_use
    rv$task_durations[7]  <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_high_dim, {
    rv$task_responses[8]  <- input$ans_high_dim
    rv$task_durations[8]  <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_data_vis, {
    rv$task_responses[9]  <- input$ans_data_vis
    rv$task_durations[9]  <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_previous_knowledge, {
    rv$task_responses[10] <- input$ans_previous_knowledge
    rv$task_durations[10] <- as.integer(120 - rv$timer)
  })
  ## TODO: propagate to all 3 apps.
  
  
  ### Obs next page button -----
  observeEvent(input$next_pg_button, {
    # if <on last task> {<do nothing>}
    if (rv$pg_num >= survey_start){ return() }
    # Init rv$ans_tbl <- ans_tbl() on first press
    if (rv$pg_num == 1){ rv$ans_tbl <- ans_tbl() }
    
    # Write this task's reponses and duration to ans_tbl
    # if (<not an intro task>) {<write repsonses and durations>}
    if (ui_section() == "task") {
      ins_row <- which(rv$ans_tbl$blockrep == blockrep())[1] # first row of this blockrep.
      ins_nrows <- length(rv$task_responses) - 1
      rv$ans_tbl[ins_row:(ins_row + ins_nrows), 5] <- rv$task_responses
      rv$ans_tbl[ins_row:(ins_row + ins_nrows), 6] <- rv$task_durations
    }
    
    # Reset responses, duration, and timer for next task
    rv$pg_num <- rv$pg_num + 1 # NOW LOOKING AT NEW PAGE
    output$response_msg <- renderText("")
    rv$task_responses <- NULL
    rv$task_durations <- NULL
    rv$timer <- 120
    rv$timer_active <- TRUE
    
    # Reset default values
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
    cat("page: ",rv$pg_num,". length of task_respones: ",length(rv$task_responses), ". this_p: ",this_p, " \n")
    print(rv$task_responses)
  })
  
  ### Obs save reponses button -----
  observeEvent(input$save_ans, {
    if (length(rv$task_responses) != 10) {browser()}
    # Write survey responses to rv$ans_tbl
    ins_nrows <- length(s_survey_questions) - 1
    ins_row <- nrow(rv$ans_tbl) - ins_nrows
    rv$ans_tbl[ins_row:(ins_row + ins_nrows), 5] <- rv$task_responses
    rv$ans_tbl[ins_row:(ins_row + ins_nrows), 6] <- rv$task_durations
    
    # Write rv$ans_tbl to .csv file.
    df <- rv$ans_tbl
    if (!is.null(rv$save_file)){ # if save already exists 
      output$save_msg <- renderPrint(cat("Reponses already saved as ", rv$save_file, 
                                         ".", sep = ""))
      return()
    }
    
    # Do the actual saving
    save_base <- paste0("response_table_", study_factor)
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
    
    output$save_msg <- renderPrint(
      cat("Reponses saved as ", save_file, 
          ". Thank you for participating!", sep = ""))
    
    return()
  })
  
  ### Obs timer -----
  observe({
    invalidateLater(1000, session)
    isolate({
      if(rv$timer_active)
      {
        rv$timer <- rv$timer - 1
        if(rv$timer < 1)
        {
          rv$timer_active <- FALSE
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
  ### Block 2 inputs, importance rank -----
  # ie. output$blk2_ans1 is the value for block 2 question about var 1.
  output$blk2Inputs <- renderUI({
    i <- j <- ncol(task_dat())
    #cnt <- rv$pg_num
    #div(id = letters[(cnt %% length(letters)) + 1],
        lapply(1:i, function(i) {
          radioButtons(inputId = paste0("blk2_ans", i), label = paste("Variable ", i),
                       choices = c(as.character(1:j), "unimportant"), 
                       selected = "unimportant", inline = TRUE)
        })
    #)
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
  # cannot print output$x in output$dev_msg.
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
  
  ## TODO: uncomment when to start logging.
  ## TODO: Copy to the other app_*.r files.
  # ### Create log file.
  # dput(shiny::reactlog(),
  #      file = paste0("reactlog_", study_factor, # + same save_num as rv$save_file
  #                    substr(rv$save_file, nchar(rv$save_file) - 6, nchar(rv$save_file) - 4),
  #                    ".txt")
  # )
}

### Combine as shiny app.
shinyApp(ui = ui, server = server)


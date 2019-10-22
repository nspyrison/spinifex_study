source('global_static.r', local = TRUE)



##### Server function, dynamic outputs ----
server <- function(input, output, session) {  ### INPUT, need to size to number of reps
  ### Initialization -----
  rv                <- reactiveValues()
  rv$task_num       <- 1
  rv$timer          <- 120
  rv$timer_active   <- TRUE
  rv$task_responses <- NULL
  rv$task_durations <- NULL
  rv$save_file      <- NULL
  rv$ans_tbl        <- NULL

  
  ##### Start reactives
  p2 <- reactive({ ncol(s_dat[[2]]) })
  p3 <- reactive({ ncol(s_dat[[3]]) })
  p4 <- reactive({ ncol(s_dat[[4]]) })
  p_sims <- reactive({ p2() + p3() + p4() })
  block_num <- reactive({
    1 + (rv$task_num - 1) %/% (n_reps + 1)
  })
  rep_num <- reactive({
    rv$task_num - (4 * (block_num() - 1))
  })
  blockrep <- reactive({
    if (rep_num() == 1) {return(NULL)}
    paste0(s_blocks[block_num()], rep_num() - 1)
  })
  task_dat <- reactive({
    ret <- s_dat[[rep_num()]]
    colnames(ret) <- paste0("V", 1:ncol(ret))
    return(ret) 
  })
  
  
  ### PCA Plot -----
  ## I disagree with going to base biplot, 
  ##  why not give the same feature map, col, and pch?
  # pca <- princomp(tourr::flea[,1:6])
  # biplot(pca)
  task_pca <- reactive({
    if (rv$timer_active | rep_num() == 1) {
      dat <- task_dat()
      dat_std <- tourr::rescale(dat)
      if (block_num() == 1) {
        col = "black"
        pch = 16
      }
      if (block_num() == 2){
        col <- col_of(attributes(dat)$cluster)
        pch <- pch_of(attributes(dat)$cluster)
      }
      if (block_num() == 3) {
        col <- col_of(attributes(dat)$cluster, pallet_name = "Paired")
        pch <- 3 + pch_of(attributes(dat)$cluster)
      }
      
      pca <- prcomp(dat_std)
      if (block_num() != 2) { # not 2nd block.
        pca_x <- data.frame(2 * (tourr::rescale(pca$x) - .5))
        pca_rotation <- set_axes_position(data.frame(t(pca$rotation)), 
                                          "bottomleft")
      } else { # is 2nd block, change sign of var map.
        pca_x <- as.matrix(dat) %*% (-1 * pca$rotation)
        pca_x <- data.frame(2 * (tourr::rescale(pca_x) - .5))
        pca_rotation <- set_axes_position(data.frame(t(-1 * pca$rotation)), 
                                          "bottomleft")
      }
      pca_pct_var <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
      
      pca_x_axis <- input$x_axis
      pca_y_axis <- input$y_axis
      rot_x_axis <- paste0("V", substr(pca_x_axis,3,3))
      rot_y_axis <- paste0("V", substr(pca_y_axis,3,3))
      x_lab <- paste0(input$x_axis, " - ", 
                      pca_pct_var[as.integer(substr(pca_x_axis,3,3))], "% Var")
      y_lab <- paste0(input$y_axis, " - ", 
                      pca_pct_var[as.integer(substr(pca_y_axis,3,3))], "% Var")
      
      angle <- seq(0, 2 * pi, length = 360)
      circ <- set_axes_position(data.frame(x = cos(angle),
                                           y = sin(angle)),
                                "bottomleft")
      zero  <- set_axes_position(0, "bottomleft")
      
      ggplot() + 
        # data points
        geom_point(pca_x, mapping = aes(x = get(pca_x_axis), 
                                        y = get(pca_y_axis)),
                   color = col, fill = col, shape = pch) +
        # axis segments
        geom_segment(pca_rotation, 
                     mapping = aes(x = get(rot_x_axis), xend = zero,
                                   y = get(rot_y_axis), yend = zero),
                     size = .3, colour = "grey80") +
        # axis label text
        geom_text(pca_rotation, 
                  mapping = aes(x = get(rot_x_axis), 
                                y = get(rot_y_axis), 
                                label = colnames(pca_rotation)), 
                  size = 4, colour = "grey50", 
                  vjust = "outward", hjust = "outward") +
        # Cirle path
        geom_path(circ, 
                  mapping = aes(x = x, y = y),
                  color = "grey80", size = .3, inherit.aes = F) +
        # options
        theme_minimal() + 
        theme(aspect.ratio = 1) +
        scale_color_brewer(palette = "Dark2") +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              legend.position = 'none') +
        labs(x = x_lab, y = y_lab)
    }
  })
  
  
  ### Response table -----
  ans_tbl <- reactive({
    col_blockrep <- c(s_blockrep_id[1:3],          # block 1
                      rep(s_blockrep_id[4], p2()), # block 2
                      rep(s_blockrep_id[5], p3()), 
                      rep(s_blockrep_id[6], p4()), 
                      rep(s_blockrep_id[7], p2()), # block 3
                      rep(s_blockrep_id[8], p3()), 
                      rep(s_blockrep_id[9], p4()), 
                      paste0("survey", 1:7)        # survey
    )
    col_var      <- c(rep(NA, 3),   # block 1
                      rep(c(1:p2(), # block 2 & 3
                            1:p3(), 
                            1:p4()), 2),
                      rep(NA, 7))   # survey
    s_sim_id     <- c("001", "002", "003")
    col_sim_id   <- c(s_sim_id,                     # block 1
                      rep(c(rep(s_sim_id[1], p2()), # block 2 & 3
                            rep(s_sim_id[2], p3()), 
                            rep(s_sim_id[3], p4())), 2),
                      rep(NA, 7))                   # survey
    col_question <- c(rep(s_block_questions[1], n_reps),   # block 1
                      rep(s_block_questions[2], p_sims()), # block 2
                      rep(s_block_questions[3], p_sims()), # block 3
                      s_survey_questions)                  # survey 
    col_response <- c(rep(NA, n_reps),       # block 1
                      rep(NA, p_sims()),     # block 2
                      rep(NA, p_sims()),     # block 3
                      rep("5 (default)", 7)) # survey 
    col_duration <- c(rep(NA, n_reps),   # block 1
                      rep(NA, p_sims()), # block 2
                      rep(NA, p_sims()), # block 3
                      rep(NA, 7))        # survey 
    data.frame(blockrep = col_blockrep,
               var      = col_var,
               sim_id   = col_sim_id,
               question = col_question,
               response = col_response,
               duration = col_duration)
  })
  ##### End reactive
  
  
  ##### Start observes
  ### Update axis choices -----
  observe({
    p <- ncol(task_dat())
    updateRadioButtons(session,
                       "x_axis",
                       choices  = paste0("PC", 1:p),
                       selected = "PC1")
    updateRadioButtons(session,
                       "y_axis",
                       choices  = paste0("PC", 1:p),
                       selected = "PC2")
  })
  
  ### Obs responses and durations -----
  ##### Block 1 responses & duration
  observeEvent(input$blk1_ans, {
    rv$task_responses[1] <- input$blk1_ans
    rv$task_durations[1] <- as.integer(120 - rv$timer)
  })
  ##### Block 2 responses & duration
  observeEvent(input$blk2_ans1, {
    rv$task_responses[1] <- input$blk2_ans1
    rv$task_durations[1] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk2_ans2, {
    rv$task_responses[2] <- input$blk2_ans2
    rv$task_durations[2] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk2_ans3, { 
    rv$task_responses[3] <- input$blk2_ans3
    rv$task_durations[3] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk2_ans4, {
    rv$task_responses[4] <- input$blk2_ans4
    rv$task_durations[4] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk2_ans5, {
    rv$task_responses[5] <- input$blk2_ans5
    rv$task_durations[5] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk2_ans6, {
    rv$task_responses[6] <- input$blk2_ans6
    rv$task_durations[6] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk2_ans7, {
    rv$task_responses[7] <- input$blk2_ans7
    rv$task_durations[7] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk2_ans8, {
    rv$task_responses[8] <- input$blk2_ans8
    rv$task_durations[8] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk2_ans9, {
    rv$task_responses[9] <- input$blk2_ans9
    rv$task_durations[9] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk2_ans10, {
    rv$task_responses[10] <- input$blk2_ans10
    rv$task_durations[10] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk2_ans11, {
    rv$task_responses[11] <- input$blk2_ans11
    rv$task_durations[11] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk2_ans12, {
    rv$task_responses[12] <- input$blk2_ans12
    rv$task_durations[12] <- as.integer(120 - rv$timer)
  })
  ##### Block 3 responses & duration
  observeEvent(input$blk3_ans1, {
    rv$task_responses[1] <- input$blk3_ans1
    rv$task_durations[1] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk3_ans2, {
    rv$task_responses[2] <- input$blk3_ans2
    rv$task_durations[2] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk3_ans3, { 
    rv$task_responses[3] <- input$blk3_ans3
    rv$task_durations[3] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk3_ans4, {
    rv$task_responses[4] <- input$blk3_ans4
    rv$task_durations[4] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk3_ans5, {
    rv$task_responses[5] <- input$blk3_ans5
    rv$task_durations[5] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk3_ans6, {
    rv$task_responses[6] <- input$blk3_ans6
    rv$task_durations[6] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk3_ans7, {
    rv$task_responses[7] <- input$blk3_ans7
    rv$task_durations[7] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk3_ans8, {
    rv$task_responses[8] <- input$blk3_ans8
    rv$task_durations[8] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk3_ans9, {
    rv$task_responses[9] <- input$blk3_ans9
    rv$task_durations[9] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk3_ans10, {
    rv$task_responses[10] <- input$blk3_ans10
    rv$task_durations[10] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk3_ans11, {
    rv$task_responses[11] <- input$blk3_ans11
    rv$task_durations[11] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$blk3_ans12, {
    rv$task_responses[12] <- input$blk3_ans12
    rv$task_durations[12] <- as.integer(120 - rv$timer)
  })
  ##### Survey responses & duration
  observeEvent(input$ans_ease, {
    rv$task_responses[1] <- input$ans_ease
    rv$task_durations[1] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_confidence, {
    rv$task_responses[2] <- input$ans_confidence
    rv$task_durations[2] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_understand, {
    rv$task_responses[3] <- input$ans_understand
    rv$task_durations[3] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_use, {
    rv$task_responses[4] <- input$ans_use
    rv$task_durations[4] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_high_dim, {
    rv$task_responses[5] <- input$ans_high_dim
    rv$task_durations[5] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_data_vis, {
    rv$task_responses[6] <- input$ans_data_vis
    rv$task_durations[6] <- as.integer(120 - rv$timer)
  })
  observeEvent(input$ans_previous_knowledge, {
    rv$task_responses[7] <- input$ans_previous_knowledge
    rv$task_durations[7] <- as.integer(120 - rv$timer)
  })
  
  ### Obs next task button -----
  observeEvent(input$next_task_button, {
    # if <on last task> {<do nothing>}
    if (rv$task_num >= length(s_header_text)){ return() }
    # Init rv$ans_tbl <- snd_tbl() on first press
    if (rv$task_num == 1){ rv$ans_tbl <- ans_tbl() }
    
    #CHECK FOR DEFAULT RESPONSE??
    # if (is.na(input$task_response)){
    #   output$response_msg <- renderText("Please enter a response before continuing.")
    #   return()
    # }
    
    # Write this_ans_df to ans_tbl
    # if (<not an intro task>) {<write repsonses and durations>}
    if (rep_num() != 1) {
      ins_row <- which(rv$ans_tbl$blockrep == blockrep())[1]
      ins_nrows <- length(rv$task_responses) - 1
      rv$ans_tbl[ins_row:(ins_row + ins_nrows), 5] <- rv$task_responses
      rv$ans_tbl[ins_row:(ins_row + ins_nrows), 6] <- rv$task_durations
      print(ins_row)
      print(ins_nrows)
      print(length(rv$task_responses))
      print(rv$task_responses)
    }
    
    # Reset responses, duration, and timer for next task
    rv$task_num <- rv$task_num + 1
    output$response_msg <- renderText("")
    rv$task_responses <- NULL
    rv$task_durations <- NULL
    rv$timer <- 120
    rv$timer_active <- TRUE
    
    # Set structure for responses and durations
    if(block_num() == 1) {this_n <- 1
    } else {
      if(block_num() == 2) {this_n <- p2()}
      if(block_num() == 3) {this_n <- p3()}
      if(block_num() == 4) {this_n <- p4()}
      if(block_num() == 5) {this_n <- length(s_survey_questions)}
    }
    rv$task_responses <- rep("default", this_n)
    rv$task_durations <- rep("default", this_n)
  })
  
  ### Obs save reponses button -----
  # If save button hit 5 times with warning, saves anyway. No double saving.
  observeEvent(input$save_ans, {
    df <- rv$ans_tbl
    save_base <- paste0("response_table_", study_factor)
    if (!is.null(rv$save_file)){
      output$save_msg <- renderPrint(cat("Reponses already saved as ", rv$save_file, 
                                         ". Thank you for participating!", sep = ""))
      return()
    }
    if (input$save_ans > 5){
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
      output$save_msg <- renderPrint(cat("Reponses saved as ", save_file, 
                                         ", dispite warning flag.", sep = ""))
      rv$save_file <- save_file
      return()
    }
    if (max(is.na(df$responses)) == 1) { # Check that all tasks have answers.
      output$save_msg <- renderText("Please verify that all tasks have been answered.")
      return()
    }
    if (min(df[(nrow(df) - 6):nrow(df), 5] == 5) == 1) { # Check that all survey questions not default.
      output$save_msg <- renderText("Please verify that the survey has been answered.")
      return()
    }
    
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
    output$save_msg <- renderPrint(cat("Reponses saved as ", save_file, 
                                       ". Thank you for participating!", sep = ""))
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
  
  ### Outputs -----
  output$timer_disp <- renderText({
    if (rep_num() != 1) { # disp timer if not an intro page.
      if (rv$timer < 1) {return("Time has expired, please enter your best guess and proceed.")
      } else {paste0("Time left: ", lubridate::seconds_to_period(rv$timer))}
    } else {return()}
  })
  output$block_num     <- reactive(block_num())
  output$header_text   <- renderText(s_header_text[rv$task_num])
  output$question_text <- renderText(s_question_text[rv$task_num])
  output$top_text      <- renderText(s_top_text[rv$task_num])
  output$bottom_text   <- renderText(s_bottom_text[rv$task_num])
  output$task_pca      <- renderPlot({task_pca()}, height = 800) 
  output$task_gtour    <- renderPlotly({task_gtour()})
  output$ans_tbl       <- renderTable({rv$ans_tbl})
  
  ### Block 2 inputs, importance rank -----
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
                   choices = c("1", "2", "3", "4", "not correlated"), 
                   selected = "not correlated", inline = TRUE)
    })
  })
  
  
  ### Dev msg -----
  # cannot print output$x in output$dev_msg.
  output$dev_msg <- renderPrint(cat("dev msg -- ", "\n",
                                    "s_header_text[rv$task_num]: ", s_header_text[rv$task_num], "\n",
                                    "rv$timer: ", rv$timer, "\n",
                                    "rv$task_num: ", rv$task_num, "\n",
                                    "block_num(): ", block_num(), "\n",
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
  
  ## TODO: uncomment when finalized. 
  ## TODO: Copy to the other app_*.r files + rv$log_file at top
  # ### Create log file.
  # dput(shiny::reactlog(), 
  #      file = paste0("reactlog_", study_factor, # + same save_num as rv$save_file
  #                    substr(rv$save_file, nchar(rv$save_file) - 6, nchar(rv$save_file) - 4) 
  #                    ".txt")
  # )
}

### Combine as shiny app.
shinyApp(ui = ui, server = server)


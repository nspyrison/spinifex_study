source('global.r', local = TRUE)

library("GGally")
library("lubridate") # For timer

##### Server function, dynamic outputs ----
server <- function(input, output, session) {  ### INPUT, need to size to number of reps
  ### Initialization -----
  rv <- reactiveValues()
  rv$task_num <- 1
  rv$timer <- 120
  rv$timer_active   <- TRUE
  rv$task_responses <- NULL
  rv$task_durations <- NULL
  rv$save_file      <- NULL
  
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
    s_dat[[rep_num()]]
  })
  
  
  ### PCA Plot -----
  task_pca <- reactive({
    if (rv$timer_active | nchar(s_header_text[rv$task_num]) != 10) {
      dat <- task_dat()
      col <- col_of(attributes(dat)$cluster)
      pch <- pch_of(attributes(dat)$cluster)
      dat_std <- tourr::rescale(dat)
      
      pca <- prcomp(dat_std)
      pca_x <- data.frame(2 * (tourr::rescale(pca$x) - .5))
      pca_rotation <- set_axes_position(data.frame(t(pca$rotation)), 
                                        "bottomleft")
      pca_pct_var <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 2)
      
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
  
  ##TODO: not needed for static
  ### gtour Plot -----
  # task_gtour <- reactive({
  #   if (rv$timer_active | nchar(s_header_text[rv$task_num]) != 10) {
  #     dat <- task_dat()
  #     col <- col_of(attributes(dat)$cluster)
  #     pch <- pch_of(attributes(dat)$cluster)
  #     dat_std <- tourr::rescale(dat)
  #     
  #     tpath <- save_history(dat_std, tour_path = grand_tour(), max = 6)
  #     play_tour_path(tour_path = tpath, data = dat_std, col = col, pch = pch,
  #                    axes = "bottomleft")
  #   }
  # })
  
  ### Response table -----
  ans_tbl <- reactive({
    col_blockrep <- c(s_blockrep_id[1:3], # block 1
                      rep(s_blockrep_id[4], p2()), # block 2
                      rep(s_blockrep_id[5], p3()),
                      rep(s_blockrep_id[6], p4()),
                      rep(s_blockrep_id[7], p2()), # block 3
                      rep(s_blockrep_id[8], p3()),
                      rep(s_blockrep_id[9], p4()),
                      paste0("survey", 1:7) # survey
    )
    col_var      <- c(rep(NA, 3),   # block 1
                      rep(c(1:p2(), # block 2&3
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
    col_response <- c(rv$task_responses, # blocks
                      input$ans_ease,    # survey
                      input$ans_confidence,
                      input$ans_understand,
                      input$ans_use,
                      input$ans_high_dim,
                      input$ans_data_vis,
                      input$ans_previous_knowledge)
    col_duration <- c(rv$task_durations, # blocks
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
  ### Obs axis choices -----
  observe({
    d <- ncol(task_dat())
    updateRadioButtons(session,
                       "x_axis",
                       choices  = paste0("PC", 1:d),
                       selected = "PC1")
    updateRadioButtons(session,
                       "y_axis",
                       choices  = paste0("PC", 1:d),
                       selected = "PC2")
  })
  
  ##TODO: finish writing obs events for dynamic inputs.
  ## CANNOT save in response order, needs structure
  ### Obs responses and durations -----
  observeEvent(input$blk2_ans1, {
    rv$responses[1] <- input$blk2_ans1
    rv$durations[1] <- 120 - rv$timer
  })
  observeEvent(input$blk2_ans2, {
    rv$responses[2] <- input$blk2_ans2
    rv$durations[2] <- 120 - rv$timer
  })
  observeEvent(input$blk2_ans3, { 
    rv$responses[3] <- input$blk2_ans3
    rv$durations[3] <- 120 - rv$timer
  })
  observeEvent(input$blk2_ans4, {
    rv$responses[4] <- input$blk2_ans4
    rv$durations[4] <- 120 - rv$timer
  })
  observeEvent(input$blk2_ans5, {
    rv$responses[5] <- input$blk2_ans5
    rv$durations[5] <- 120 - rv$timer
  })
  observeEvent(input$blk2_ans6, {
    rv$responses[6] <- input$blk2_ans6
    rv$durations[6] <- 120 - rv$timer
  })
  observeEvent(input$blk2_ans7, {
    rv$responses[7] <- input$blk2_ans7
    rv$durations[7] <- 120 - rv$timer
  })
  observeEvent(input$blk2_ans8, {
    rv$responses[8] <- input$blk2_ans8
    rv$durations[8] <- 120 - rv$timer
  })
  observeEvent(input$blk2_ans9, {
    rv$responses[9] <- input$blk2_ans9
    rv$durations[9] <- 120 - rv$timer
  })
  observeEvent(input$blk2_ans10, {
    rv$responses[10] <- input$blk2_ans10
    rv$durations[10] <- 120 - rv$timer
  })
  observeEvent(input$blk2_ans11, {
    rv$responses[11] <- input$blk2_ans11
    rv$durations[11] <- 120 - rv$timer
  })
  observeEvent(input$blk2_ans12, {
    rv$responses[12] <- input$blk2_ans12
    rv$durations[12] <- 120 - rv$timer
  })
  
  ### Obs next task button -----
  observeEvent(input$next_task_button, {
    # if <on last task> {<do nothing>}
    if (rv$task_num >= length(s_header_text)){ return() }
    
    #CHECK FOR DEFAULT RESPONSE??
    # if (is.na(input$task_response)){
    #   output$response_msg <- renderText("Please enter a response before continuing.")
    #   return()
    # }
    
    # Write this_ans_df to ans_tbl
    # if (<not an intro task>) {<write repsonses and durations>}
    if (rep_num() != 1) {
      browser()
      ins_row <- which(ans_tbl()$blockrep == blockrep())
      ins_nrows <- length(rv$responses) - 1
      ans_tbl()[ins_row:(ins_row + ins_nrows), 5] <- rv$task_responses
      ans_tbl()[ins_row:(ins_row + ins_nrows), 6] <- rv$task_durations
    }
    
    # Reset responses, duration, and timer for next task
    rv$task_num <- rv$task_num + 1
    output$response_msg <- renderText("")
    rv$task_responses <- "default"
    rv$task_durations <- "default"
    rv$timer <- 120
    rv$timer_active <- TRUE
    
    # Set structure for responses and durations
    if(block_num() == 1) {this_n <- 1
    } else {
      if(rep_num() == 2) {this_n <- p2()}
      if(rep_num() == 3) {this_n <- p3()}
      if(rep_num() == 4) {this_n <- p4()}
    }
    rv$responses <- rep(NA, this_n)
    rv$durations <- rep(NA, this_n)
  })
  

  
  ### Obs save reponses button -----
  # If save button hit 5 times with warning, saves anyway. No double saving.
  observeEvent(input$save_ans, {
    df <- ans_tbl()
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
    if (nchar(s_header_text[rv$task_num]) == 10) { # 10 char for "Task -- xn"
      if (rv$timer < 1) {return("Time has expired, please enter your best guess and proceed.")
      } else {paste0("Time left: ", lubridate::seconds_to_period(rv$timer))}
    } else {return()}
  })
  output$block_num     <- reactive(block_num())
  output$header_text   <- renderText(s_header_text[rv$task_num])
  output$question_text <- renderText(s_question_text[rv$task_num])
  output$top_text      <- renderText(s_top_text[rv$task_num])
  output$bottom_text   <- renderText(s_bottom_text[rv$task_num])
  output$task_pca      <- renderPlot({task_pca()}) 
  output$task_gtour    <- renderPlotly({task_gtour()})
  output$ans_tbl       <- renderTable({ans_tbl()})
  
  ### Block 2 inputs, importance rank -----
  output$blk2Inputs <- renderUI({
    dat <- task_dat()
    i <- j <- ncol(dat)
    lapply(1:i, function(i) {
      radioButtons(inputId = paste0("blk2_ans", i), label = paste("Variable ", i),
                   choices = 1:j, inline = TRUE)
    })
  })
  
  ### Block 3 inputs, noise -----
  output$blk3Inputs <- renderUI({
    dat <- task_dat()
    i <- ncol(dat)
    lapply(1:i, function(i) {
      radioButtons(inputId = paste0("blk3_ans", i), label = paste("Variable", i),
                   choices = c("noise", "signal"), inline = TRUE)
    })
  })
  
  ### Dev msg -----
  # cannot print output$x in output$dev_msg.
  output$dev_msg <- renderPrint(cat("dev msg -- ", "\n",
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
                                    "input$save_ans > 5", input$save_ans > 5, "\n",
                                    "rv$save_file", rv$save_file, "\n",
                                    "is.null(rv$save_file)", is.null(rv$save_file), "\n",
                                    sep = ""))
}

### Combine as shiny app.
shinyApp(ui = ui, server = server)


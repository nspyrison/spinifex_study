source('global.r', local = TRUE)

library("GGally")
library("lubridate") # For timer

##### Server function, dynamic outputs ----
server <- function(input, output, session) {  ### INPUT, need to size to number of reps
  ### Initialization -----
  rv <- reactiveValues()
  rv$task_num <- 1
  rv$timer <- 120
  rv$timer_active <- TRUE
  rv$task_responses <- rep(NA, each = n_blocks * n_reps)
  
  task_dat <- reactive({
    dataset_num <- rv$task_num %% (n_reps + 1)
    s_dat[[dataset_num]]
  })
  
  ### PCA Plot -----
  #TODO: turn this into a full reactive function
  task_plot <- reactive({
    if (rv$timer_active | nchar(s_header_text[rv$task_num]) != 10) {
      dat <- task_dat()
      pca <- data.frame(prcomp(dat)$x)
      col <- col_of(attributes(dat)$cluster)
      pch <- pch_of(attributes(dat)$cluster)
      
      ggplot(pca, mapping = aes(x = get(eval(input$x_axis)), 
                                y = get(eval(input$y_axis))) ) +
        geom_point(color = col, shape = pch) +
        theme_minimal() + theme(aspect.ratio = 1) +
        scale_color_brewer(palette = "Dark2") +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              legend.position = 'none') +
        labs(x = eval(input$x_axis), y = eval(input$y_axis))
    }
  })
  
  ### Update axis selection -----
  observe({
    d <- ncol(s_dat[[rv$task_num]])
    updateRadioButtons(session,
                       "x_axis",
                       choices  = paste0("PC", 1:d),
                       selected = "PC1")
    updateRadioButtons(session,
                       "y_axis",
                       choices  = paste0("PC", 1:d),
                       selected = "PC2")
  })
  
  ### Next task button -----
  observeEvent(input$next_task_button, {
    if (rv$task_num < length(s_header_text)){
      if (is.na(input$task_response)){
        output$response_msg <- renderText("Please enter a response before continuing.")
        return()
      }
      rv$task_responses[rv$task_num] <- input$task_response
      rv$task_num <- rv$task_num + 1
      rv$timer <- 120
      rv$timer_active <- TRUE
      output$response_msg <- renderText("")
      if (nchar(s_header_text[rv$task_num]) == 10){
        updateNumericInput(session, "task_response", value = "")
      } else {
        if (grepl("cluster", s_header_text[rv$task_num])){
          updateNumericInput(session, "task_response", value = 1)}
        if (grepl("important", s_header_text[rv$task_num])){
          updateNumericInput(session, "task_response", value = 2)}
        if (grepl("correlated", s_header_text[rv$task_num])){
          updateNumericInput(session, "task_response", value = 3)}
      }
    }
  })
  
  ### Response table -----
  ans_tbl <- reactive({
    col_responses <- c(rv$task_responses,
                       input$ans_ease,
                       input$ans_confidence,
                       input$ans_understand,
                       input$ans_use,
                       input$ans_high_dim,
                       input$ans_data_vis,
                       input$ans_previous_knowledge)
    data.frame(blockrep  = col_blockrep,
               question  = col_question,
               sim_id    = col_sim_id,
               #sim       = nest((col_sim)), # a list with uneven tibbles 
               responses = col_responses)
  })
  
  ### Save reponse table, data -----
  observeEvent(input$save_ans, {
    df <- ans_tbl()
    if (max(is.na(df)) == 1) { # Check that all tasks have answers.
      output$save_msg <- renderText("Please verify that all tasks have been answered.")
      return()
    }
    if (min(df[(nrow(df) - 6):nrow(df), 3] == 5) == 1) { # Check that all survey questions not default.
      output$save_msg <- renderText("Please verify that the survey has been answered.")
      return()
    }
    
    save_num <- 1
    save_name <- sprintf("reponse_table%03d", save_num)
    save_file <- paste0(save_name, ".csv")
    while (file.exists(save_file)){
      save_num <- save_num + 1
      save_name <- sprintf("reponse_table%03d", save_num)
      save_file <- paste0(save_name, ".csv")
    }
    assign(save_name, ans_tbl())
    write.csv(response_table, file = save_file, row.names = FALSE)
    sim_save_name <- sprintf("simulation_data%03d", save_num)
    sim_save_file <- paste0(sim_save_name, ".csv")
    assign(sprintf("reponse_table%03d", save_num), s_dat)
    write.csv(ndf_simulation, file = sim_save_file, row.names = FALSE)
    output$save_msg <- renderPrint(paste0("Reponses saved as ", save_file, ", 
                         and data saved as ", sim_save_file, "."))
  })
  
  ### Timer -----
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
    if (nchar(s_header_text[rv$task_num]) == 10) { # 10 for "Task -- xn"
      if (rv$timer < 1) {return("Time has expired, please enter your best guess and proceed.")
      } else {paste0("Time left: ", lubridate::seconds_to_period(rv$timer))}
    } else {return()}
  })
  output$header_text <- renderText(s_header_text[rv$task_num])
  output$question_text <- renderText(s_question_text[rv$task_num])
  output$top_text <- renderText(s_top_text[rv$task_num])
  output$bottom_text <- renderText(s_bottom_text[rv$task_num])
  output$task_plot <- renderPlot({task_plot()}) 
  output$ans_tbl <- renderTable({
    ans_tbl()[ , !(names(ans_tbl()) %in% "simulation")] # hide dataset from users
  })
  output$dev_msg <- renderPrint(cat("dev msg -- \n",
                                    "rv$timer: ", rv$timer, "\n",
                                    "rv$task_num: ", rv$task_num, "\n",
                                    "s_header_text[rv$task_num]: ", s_header_text[rv$task_num], "\n",
                                    "input$task_response: ", input$task_response, "\n",
                                    "head(col): ", head(col), "\n",
                                    "input$x_axis: ", input$x_axis, "\n",
                                    "input$y_axis: ", input$y_axis, "\n",
                                    sep = ""))
}

### Combine as shiny app.
shinyApp(ui = ui, server = server)


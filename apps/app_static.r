source('global.r', local = TRUE)

library("GGally")
library("lubridate")

##### Private, static content ----
### Create PCA ggplots, 'pca_',blck,rep
s_pca <- NULL
for (blck in s_blocks){
  for (rep in c("demo", 1:n_reps)){
    if (rep == "demo") {
      pca <- data.frame(prcomp(demo_dat)$x)
    } else {
      pca <- data.frame(prcomp(s_samp_dat[[as.integer(rep)]])$x)
    }
    pca_plot <- ggplot(pca, mapping = aes(x = PC1, y = PC2)) + geom_point() +
      theme_minimal()
    
    s_pca[[length(s_pca) + 1]] <- pca_plot
  }
}

### Create SPLOM ggplots, 'splom_',blck,rep
# TODO: currently no way to reach splom plots. add radio buttons to ui.
s_splom <- NULL
for (blck in s_blocks){
  for (rep in c("demo", 1:n_reps)){
    if (rep == "demo") {
      dat <- demo_dat
    } else {
      dat <- s_samp_dat[[as.integer(rep)]]
    }
    splom_plot <- GGally::ggpairs(data = dat) + #, alpha = .2) +
      theme_minimal()
    
    s_splom[[length(s_splom) + 1]] <- splom_plot
  }
} 

##### Server function, dynamic outputs ----
server <- function(input, output, session) {  ### INPUT, need to size to number of reps
  rv <- reactiveValues()
  rv$task_num <- 1
  rv$timer <- 120
  rv$timer_active <- TRUE
  rv$task_responses <- rep(NA, each = n_blocks * n_reps)
  
  ### Outputs
  output$timer_disp <- renderText({
    if (rv$timer_active) {
      paste("Time left: ", seconds_to_period(rv$timer))
    } else ("Time has expired, please enter your best guess and proceed.")
  })
  output$header_text <- renderText(s_header_text[rv$task_num])
  output$question_text <- renderText(s_question_text[rv$task_num])
  output$top_text <- renderText(s_top_text[rv$task_num])
  output$bottom_text <- renderText(s_bottom_text[rv$task_num])
  output$task_plot <- renderPlot({s_pca[[rv$task_num]]}) 
  output$ans_tbl <- renderTable({
    ans_tbl()[ , !(names(ans_tbl()) %in% "dataset")] # hide dataset from users
  })
  output$dev_msg <- renderPrint(cat("dev msg -- \n",
                                    "rv$timer: ", rv$timer, "\n",
                                    "rv$task_num: ", rv$task_num, "\n",
                                    "s_header_text[rv$task_num]): ", s_header_text[rv$task_num], "\n",
                                    "input$task_response: ", input$task_response, "\n",
                                    sep = ""))
  
  ### Response table
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
               dataset   = col_dataset,
               responses = col_responses)
  })
  
  ### Next task button
  observeEvent(input$next_task_button, {
    if (rv$task_num < length(s_header_text)){
      # if (input$task_response == ""){
      #   output$response_msg <- renderText("Please enter a response before continuing.")
      #   return()
      # }
      rv$task_responses[rv$task_num] <- input$task_response
      rv$task_num <- rv$task_num + 1
      output$response_msg <- renderText("")
      updateNumericInput(session, "task_response", value = "")
    }
  })
  
  ### Save reponse table
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
    save_n <- 1
    save_file <- paste0(sprintf("study_reponses%03d", save_n), ".csv")
    while (file.exists(save_file)){
      save_n <- save_n + 1
      save_file <- paste0(sprintf("study_reponses%03d", save_n), ".csv")
    }
    write.csv(ans_tbl(), file = save_file, row.names = FALSE)
    output$save_msg <- 
      renderPrint(paste0("Reponses saved as ", save_file))
  })
  
  ### Timer
  observe({
    invalidateLater(1000, session)
    isolate({
      if(rv$timer_active)
      {
        rv$timer <- rv$timer - 1
        if(rv$timer < 1)
        {
          rv$timer_active <- FALSE
          # showModal(modalDialog(
          #   title = "Important message",
          #   "Countdown completed!"
          # ))
        }
      }
    })
  })
  

 
}

### Combine as shiny app.
shinyApp(ui = ui, server = server)


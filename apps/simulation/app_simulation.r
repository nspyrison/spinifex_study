source('global.r', local = TRUE)

library("GGally")
library("lubridate") # For timer

##### Server function, dynamic outputs ----
server <- function(input, output, session) {  ### INPUT, need to size to number of reps
  ### Initialization -----
  rv <- reactiveValues()

  task_dat <- reactive({
    simulate_clusters(p = input$sim_p,
                      pnoise = input$sim_pnoise,
                      cl = input$sim_cl)
  })
  
  ### PCA Plot -----
  task_pca <- reactive({
    dat <- task_dat()
    col <- col_of(attributes(dat)$cluster)
    pch <- pch_of(attributes(dat)$cluster)
    dat_std <- tourr::rescale(dat)
    
    pca <- prcomp(dat_std)
    pca_x <- data.frame(pca$x)
    pca_rotation <- set_axes_position(data.frame(t(pca$rotation)), 
                                      "bottomleft")
    pca_x_axis <- eval(input$x_axis)
    pca_y_axis <- eval(input$y_axis)
    rot_x_axis <- paste0("V", substr(pca_x_axis,3,3))
    rot_y_axis <- paste0("V", substr(pca_y_axis,3,3))
    
    #x <- pca_x[eval(input$x_axis)]
    #y <- pca_x[eval(input$y_axis)]
    angle <- seq(0, 2 * pi, length = 360)
    circ <- set_axes_position(data.frame(x = cos(angle), y = sin(angle)), 
                              "bottomleft")
    zero  <- set_axes_position(0, "bottomleft")
    #x_range <- max(x) - min(x)
    #y_range <- max(y) - min(y)
    #a_ratio <- x_range / y_range
    
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
      theme(aspect.ratio = 1) +#a_ratio) +
      scale_color_brewer(palette = "Dark2") +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = 'none') +
      labs(x = eval(input$x_axis), y = eval(input$y_axis))
  })
  
  ### gtour Plot -----
  task_gtour <- reactive({
    dat <- task_dat()
    col <- col_of(attributes(dat)$cluster)
    pch <- pch_of(attributes(dat)$cluster)
    dat_std <- tourr::rescale(dat)
    
    tpath <- save_history(dat_std, tour_path = grand_tour(), max = 6)
    play_tour_path(tour_path = tpath, data = dat_std, col = col, pch = pch,
                   axes = "bottomleft")
  })
  
  ### Update axis selection -----
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
  
  ### Next task button -----
  ## TODO: Fix writing response to response_table as task_response is not trivial anymore.
  observeEvent(input$next_task_button, {
    if (rv$task_num < length(s_header_text)){
      # if (is.na(input$task_response)){
      #   output$response_msg <- renderText("Please enter a response before continuing.")
      #   return()
      # }
      #rv$task_responses[rv$task_num] <- input$task_response
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
  
  ### Simulation data -----
  observeEvent(input$save_sim, {
    save_num <- 1
    save_name <- sprintf("simulation_data%03d", save_num)
    save_file <- paste0(save_name, ".rds")
    while (file.exists(save_file)){
      save_num <- save_num + 1
      save_name <- sprintf("simulation_data%03d", save_num)
      save_file <- paste0(save_name, ".rds")
    }
    # assign(save_name, ans_tbl())
    # write.csv(get(save_name), file = save_file, row.names = FALSE)
    sim_save_name <- sprintf("simulation_data%03d", save_num)
    sim_save_file <- paste0(sim_save_name, ".rds")
    assign(sim_save_name, task_dat())
    saveRDS(get(sim_save_name), file = sim_save_file) # csv doesn't keep attributes
    output$save_msg <- renderPrint(cat(#"Reponses saved as ", save_file, ", 
                         # and 
                         "data saved as ", sim_save_file, ".", sep = ""))
  })
  
  ### Outputs -----
  output$task_pca      <- renderPlot({task_pca()}) 
  output$task_gtour    <- renderPlotly({task_gtour()})

  
  ### Dev msg -----
  output$dev_msg <- renderPrint(cat("dev msg -- \n",
                                    "sim_p: ", input$sim_p, "\n",
                                    "sim_pnoise: ", input$sim_pnoise, "\n",
                                    "sim_cl: ", input$sim_cl, "\n",
                                    sep = ""))
}

### Combine as shiny app.
shinyApp(ui = ui, server = server)


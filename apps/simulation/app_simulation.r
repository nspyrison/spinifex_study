source('global.r', local = TRUE)

library("GGally")


##### Server function, dynamic outputs ----
server <- function(input, output, session) {  ### INPUT, need to size to number of reps
  ### Initialization -----
  rv <- reactiveValues()
  
  ### Generate simulation data -----
  ### _Reactives -----
  task_dat <- reactive({ # for generating simulations
    simulate_clusters(p = input$sim_p,
                      pnoise = input$sim_pnoise,
                      cl = input$sim_cl)
  })
  
  # PCA Plot 
  task_pca <- reactive({
    dat <- task_dat()
    dat_std <- tourr::rescale(dat)
    class <- attributes(dat)$cluster
    
    app_PCA_plot(dat = dat_std, class = class, 
                 in_x = input$task_x_axis, in_y = input$task_y_axis)
  })
  
  # gtour Plot 
  task_gtour <- reactive({
    dat <- task_dat()
    col <- col_of(attributes(dat)$cluster)
    pch <- pch_of(attributes(dat)$cluster)
    dat_std <- tourr::rescale(dat)
    
    tpath <- save_history(dat_std, tour_path = grand_tour(), max = 6)
    play_tour_path(tour_path = tpath, data = dat_std, col = col, pch = pch,
                   axes = "bottomleft")
  })
  
  ### _Observes -----
  # Update axis selection
  observe({
    d <- ncol(task_dat())
    updateRadioButtons(session,
                       "task_x_axis",
                       choices  = paste0("PC", 1:d),
                       selected = "PC1")
    updateRadioButtons(session,
                       "task_y_axis",
                       choices  = paste0("PC", 1:d),
                       selected = "PC2")
  })
  
  ### Save button, simulation data
  observeEvent(input$save_sim, {
    save_num <- 1
    save_name <- sprintf("simulation_data%03d", save_num)
    save_file <- paste0(save_name, ".rds")
    while (file.exists(save_file)){
      save_num <- save_num + 1
      save_name <- sprintf("simulation_data%03d", save_num)
      save_file <- paste0(save_name, ".rds")
    }
    assign(save_name, task_dat())
    saveRDS(get(save_name), file = save_file) # csv doesn't keep attributes
    output$save_msg <- renderPrint(cat(
                         "data saved as ", save_file, ".", sep = ""))
  })
  
  ### Loaded simulation data ----
  ### _Reactives -----
  load_dat <- reactive({ # for reviewing simulations
    get(input$load_sim_name)
  })
  load_pnoise <- reactive({
    d <- load_dat()
    n_cl <- length(attr(d, "ncl"))
    sum(colSums(attr(d, "mncl") == 0) == n_cl)
  })
  load_mncl_reord <- reactive({
    d <- load_dat()
    p <- ncol(d)
    n_cl <- length(attr(d, "ncl"))
    reord <- attr(d, "col_reorder")
    mncl <- attr(d, "mncl")[, reord]
    rownames(mncl) <- paste0("cl ", letters[1:n_cl])
    colnames(mncl) <- paste0("V", 1:p)
    mncl
  })
  load_sum_squares <- reactive({
    d <- load_dat()
    p <- ncol(d)
    n_cl <- length(attr(d, "ncl"))
    reord <- attr(d, "col_reorder")
    mncl <- attr(d, "mncl")[, reord]
    rownames(mncl) <- paste0("cl ", letters[1:n_cl])
    colnames(mncl) <- paste0("V", 1:p)
    cl_mn <- rowMeans(mncl)
    cl_mn
    sum_sq <- apply(mncl, 2, function(x) {sum((x - cl_mn)^2)}) # sum of squares
    rank <- order(sum_sq, decreasing = T)
    
    rbind(sum_sq, rank) # double check rank, wasn't correct at one point.
  })
  load_vc_reord <- reactive({
    d <- load_dat()
    p <- ncol(d)
    reord <- attr(d, "col_reorder")
    vc <- round(attr(d, "vc")[reord, reord], 1)
    colnames(vc) <- paste0("V", 1:p)
    rownames(vc) <- paste0("V", 1:p)
    vc
  })
  
  # PCA plot, loaded sims.
  load_pca <- reactive({
    dat <- load_dat()
    dat_std <- tourr::rescale(dat)
    class <- attributes(dat)$cluster
    
    app_PCA_plot(dat = dat_std, class = class,
                 in_x = input$load_x_axis, in_y = input$load_y_axis)
  })
  
  # gtour plot, loaded sims.
  load_gtour <- reactive({
    dat <- load_dat()
    col <- col_of(attributes(dat)$cluster)
    pch <- pch_of(attributes(dat)$cluster)
    dat_std <- tourr::rescale(dat)
    
    tpath <- save_history(dat_std, tour_path = grand_tour(), max = 6)
    play_tour_path(tour_path = tpath, data = dat_std, col = col, pch = pch,
                   axes = "bottomleft")
  })
  
  ### _Observes -----
  # Update axis selection
  observe({
    d <- ncol(task_dat())
    updateRadioButtons(session,
                       "load_x_axis",
                       choices  = paste0("PC", 1:d),
                       selected = "PC1")
    updateRadioButtons(session,
                       "load_y_axis",
                       choices  = paste0("PC", 1:d),
                       selected = "PC2")
  })
  
  ### Outputs -----
  # Generate:
  output$task_pca   <- renderPlot({task_pca()}) 
  output$task_gtour <- renderPlotly({task_gtour()})
  # Review:
  output$load_pca   <- renderPlot({load_pca()}) 
  output$load_gtour <- renderPlotly({load_gtour()})
  output$load_sim_name <- renderPrint({input$load_sim_name})
  output$str_load_dat <- renderPrint({str(load_dat())})
  output$load_dat_attr <- renderPrint(cat("loaded sim parameter -- \n",
                                          "p: ", ncol(load_dat()), "\n",
                                          "pnoise: ", load_pnoise(), "\n",
                                          "cl: ", length(attr(load_dat(), "ncl")), "\n",
                                          sep = ""))
  output$load_mncl_reord <- renderPrint(load_mncl_reord())
  output$load_sum_squares <- renderPrint(load_sum_squares())
  output$load_vc_reord <- renderPrint(load_vc_reord())
  
  ### Dev msg -----
  output$dev_msg <- renderPrint(cat("dev msg -- \n",
                                    "sim_p: ", input$sim_p, "\n",
                                    "sim_pnoise: ", input$sim_pnoise, "\n",
                                    "sim_cl: ", input$sim_cl, "\n",
                                    "head(loaded_sim_names): ", head(loaded_sim_names), "\n",
                                    "load_num:", load_num, "\n",
                                    "load_num:", load_num, "\n",
                                    sep = ""))
}

### Combine as shiny app.
shinyApp(ui = ui, server = server)


source('global_simulation.r', local = TRUE)


##### Server function, dynamic outputs ----
server <- function(input, output, session) {  ### INPUT, need to size to number of reps
  ### Initialization
  rv <- reactiveValues()
  rv$curr_basis <- NULL
  
  observeEvent(input$browser, {
    browser()
  })
  
  ### Generate simulation data -----
  ### _Reactives -----
  task_dat <- reactive({ # for generating simulations
    ret <- simulate_clusters(p = input$sim_p,
                             pnoise = input$sim_pnoise,
                             cl = input$sim_cl)
    colnames(ret) <- paste0("V", 1:ncol(ret))
    return(ret)
  })
  
  # PCA Plot 
  task_pca <- reactive({
    dat <- task_dat()
    dat_std <- tourr::rescale(dat)
    class <- attributes(dat)$cluster
    
    APP_pca_plot(dat = dat_std, class = class, 
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
    p <- ncol(task_dat())
    updateRadioButtons(session,
                       "task_x_axis",
                       choices  = paste0("PC", 1:p),
                       selected = "PC1")
    updateRadioButtons(session,
                       "task_y_axis",
                       choices  = paste0("PC", 1:p),
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
  
  ### Load simulation data -----
  ### _Reactives -----
  load_dat <- reactive({ # for reviewing simulations
    ret <- get(input$load_sim_name)
    colnames(ret) <- paste0("V", 1:ncol(ret))
    return(ret)
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
  ### sum of squares is wrong approach.
  # load_sum_squares <- reactive({
  #   d <- load_dat()
  #   p <- ncol(d)
  #   n_cl <- length(attr(d, "ncl"))
  #   reord <- attr(d, "col_reorder")
  #   mncl <- attr(d, "mncl")[, reord]
  #   rownames(mncl) <- paste0("cl ", letters[1:n_cl])
  #   colnames(mncl) <- paste0("V", 1:p)
  #   cl_mn <- rowMeans(mncl)
  #   cl_mn
  #   sum_sq <- apply(mncl, 2, function(x) {sum((x - cl_mn)^2)}) # sum of squares distance from cluster means
  #   order <- sum_sq[order(sum_sq, decreasing = T)]
  #   
  #   rbind(sum_sq, order) 
  # })
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
    
    APP_pca_plot(dat = dat_std, class = class,
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
    p <- ncol(load_dat())
    updateRadioButtons(session,
                       "load_x_axis",
                       choices  = paste0("PC", 1:p),
                       selected = "PC1")
    updateRadioButtons(session,
                       "load_y_axis",
                       choices  = paste0("PC", 1:p),
                       selected = "PC2")
  })
  
  ### Load2 simulation data -----
  ### _Reactives -----
  load2_dat <- reactive({ # for reviewing simulations
    ret <- get(input$load2_sim_name)
    colnames(ret) <- paste0("V", 1:ncol(ret))
    return(ret)
  })
  load2_pnoise <- reactive({
    d <- load2_dat()
    n_cl <- length(attr(d, "ncl"))
    sum(colSums(attr(d, "mncl") == 0) == n_cl)
  })
  load2_mncl_reord <- reactive({
    d <- load2_dat()
    p <- ncol(d)
    n_cl <- length(attr(d, "ncl"))
    reord <- attr(d, "col_reorder")
    mncl <- attr(d, "mncl")[, reord]
    rownames(mncl) <- paste0("cl ", letters[1:n_cl])
    colnames(mncl) <- paste0("V", 1:p)
    mncl
  })
  load2_vc_reord <- reactive({
    d <- load2_dat()
    p <- ncol(d)
    reord <- attr(d, "col_reorder")
    vc <- round(attr(d, "vc")[reord, reord], 1)
    colnames(vc) <- paste0("V", 1:p)
    rownames(vc) <- paste0("V", 1:p)
    vc
  })
  #load2_basis
  load2_basis <- reactive({
    dat <- load2_dat()
    p <- ncol(dat)
    if (input$basis_init == "Random") ret <- tourr::basis_random(n = p, d = 2)
    if (input$basis_init == "PCA")    ret <- prcomp(dat)[[2]][, 1:2]
    if (input$basis_init == "Projection pursuit") {
      if (input$pp_type == "lda_pp" | input$pp_type == "lda_pp") {
        pp_cluster <- attributes(dat)$cluster
      } else {pp_cluster <- NA}
      tour_func <- APP_guided_tour(input$pp_type, pp_cluster)
      tour_hist <- save_history(dat, tour_func)
      tour_len  <- dim(tour_hist)[3]
      ret <- matrix(as.numeric(tour_hist[,, tour_len]), ncol = 2)
    }
    colnames(ret) <- c("x", "y")
    row.names(ret) <- colnames(dat)
    return(ret)
  })
  # Manual plot (obl_frame()), load2 sims.
  load2_manual <- reactive({
    dat <- load2_dat()
    dat_std <- tourr::rescale(dat)
    col <- col_of(attributes(dat)$cluster)
    pch <- pch_of(attributes(dat)$cluster)
    
    # leg work
    if (input$manip_var == "<none>") {m_var <- 1
    } else {m_var <- which(colnames(dat) == input$manip_var)}
    
    ret <- oblique_frame(data      = dat_std, 
                         basis     = rv$curr_basis, 
                         manip_var = m_var, 
                         theta     = 0, # perform rotation when setting rv$curr_basis
                         phi       = 0, 
                         col       = col,
                         pch       = pch,
                         axes      = "bottomleft",
                         alpha     = 1)
    return(ret)
  })
  
  ### _Observes -----
  # Update axis selection
  observe({
    p <- ncol(load2_dat())
    updateRadioButtons(session,
                       "load2_x_axis",
                       choices  = paste0("PC", 1:p),
                       selected = "PC1")
    updateRadioButtons(session,
                       "load2_y_axis",
                       choices  = paste0("PC", 1:p),
                       selected = "PC2")
  })
  
  ### Outputs -----
  # Generate:
  output$task_pca   <- renderPlot({task_pca()}) 
  output$task_gtour <- renderPlotly({task_gtour()})
  # Review (static + gtour):
  output$load_pca   <- renderPlot({load_pca()}) 
  output$load_gtour <- renderPlotly({load_gtour()})
  output$str_load_dat <- renderPrint({str(load_dat())})
  output$load_dat_attr <- renderPrint(cat("loaded sim parameter -- \n",
                                          "p: ", ncol(load_dat()), "\n",
                                          "pnoise: ", load_pnoise(), "\n",
                                          "cl: ", length(attr(load_dat(), "ncl")), "\n",
                                          sep = ""))
  output$load_mncl_reord <- renderPrint(load_mncl_reord())
  output$load_sum_squares <- renderPrint(load_sum_squares())
  output$load_vc_reord <- renderPrint(load_vc_reord())
  # Review (manual):
  output$load2_manual   <- renderPlot({load2_manual()}) 
  output$str_load2_dat <- renderPrint({str(load2_dat())})
  output$load2_dat_attr <- renderPrint(cat("loaded sim parameter -- \n",
                                          "p: ", ncol(load2_dat()), "\n",
                                          "pnoise: ", load2_pnoise(), "\n",
                                          "cl: ", length(attr(load2_dat(), "ncl")), "\n",
                                          sep = ""))
  output$load2_mncl_reord <- renderPrint(load_mncl_reord())
  output$load2_sum_squares <- renderPrint(load_sum_squares())
  output$load2_vc_reord <- renderPrint(load_vc_reord())
  
  
  ### Dev msg -----
  output$dev_msg <- renderPrint(cat("dev msg -- \n",
                                    "sim_p: ", input$sim_p, "\n",
                                    "sim_pnoise: ", input$sim_pnoise, "\n",
                                    "sim_cl: ", input$sim_cl, "\n",
                                    "head(loaded_sim_names): ", head(loaded_sim_names), "\n",
                                    "load_num:", load_num, "\n",
                                    "input$load_x_axis: ", input$load_x_axis, "\n",
                                    "input$load_y_axis: ", input$load_y_axis, "\n",
                                    sep = ""))
}

### Combine as shiny app.
shinyApp(ui = ui, server = server)


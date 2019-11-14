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
                   axes = axes_position)
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
    if (input$load_color_pts == "yes") {
      class <- attributes(dat)$cluster
    } else {class <- "black"}
    
    APP_pca_plot(dat = dat_std, class = class,
                 in_x = input$load_x_axis, in_y = input$load_y_axis)
  })
  
  # gtour plot, loaded sims.
  load_gtour <- reactive({
    dat <- load_dat()
    if (input$load_color_pts == "yes") {
      col <- pch <- attributes(dat)$cluster
    } else {col = "black"; pch = 20}
    dat_std <- tourr::rescale(dat)
    
    tpath <- save_history(dat_std, tour_path = grand_tour(), max = 6)
    play_tour_path(tour_path = tpath, data = dat_std, col = col, pch = pch,
                   axes = axes_position)
  })
  
  ### _Observes -----
  # Update axis selection
  observeEvent(load_dat() ,{ # Init axis choices when data changes
    p <- ncol(load_dat())
    updateRadioButtons(session,"load_x_axis", choices  = paste0("PC", 1:p), 
                       selected = "PC1")
    updateRadioButtons(session, "load_y_axis", choices  = paste0("PC", 1:p),
                       selected = "PC2")
  })
  
  
  ### Load2 simulation data -----
  ### _Reactives -----
  load2_dat <- reactive({ # for reviewing simulations
    ret <- get(input$load2_sim_name)
    colnames(ret) <- paste0("V", 1:ncol(ret))
    return(ret)
  })
  manip_var_num <- reactive({ 
    if (input$manip_var == "<none>") {return(1)}
    return(which(colnames(load2_dat()) == input$manip_var))
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

  # Manual plot (obl_frame()), load2 sims.
  load2_manual <- reactive({
    if (is.null(rv$curr_basis)) {stop("rv$curr_basis is null")}
    dat <- load2_dat()
    dat_std <- tourr::rescale(dat)
    if (input$load2_color_pts == "yes") {
      col <- pch <- attributes(dat)$cluster
    } else {col = "black"; pch = 20}
    
    # leg work
    if (input$manip_var == "<none>") {m_var <- 1
    } else {m_var <- which(colnames(dat) == input$manip_var)}
    # Manipulation type fixxed to radial, ie. input$manip_type == "Radial"
    
    ## Returns ggplot obj, make sure output is rendered and displayed as plot, not plotly.
    ret <- oblique_frame(data      = dat_std,
                         basis     = rv$curr_basis,
                         manip_var = m_var,
                         theta     = 0, # perform rotation when setting rv$curr_basis
                         phi       = 0,
                         col       = col,
                         pch       = pch,
                         axes      = axes_position,
                         alpha     = 1)
    
    return(ret)
  })
  
  ### _Observes -----
  # Obs Update axis choices
  observeEvent(load2_dat() ,{ # Init axis choices when data changes
    p <- ncol(load_dat())
    updateRadioButtons(session,"load2_x_axis", choices  = paste0("PC", 1:p), 
                       selected = "PC1")
    updateRadioButtons(session, "load2_y_axis", choices  = paste0("PC", 1:p),
                       selected = "PC2")
  })
  # Obs Update manip_var choices
  observeEvent(load2_dat(), { # Init manip_var choices when data changes
    these_colnames <- colnames(load2_dat())
    updateSelectInput(session, "manip_var", choices = these_colnames, 
                      selected = these_colnames[1])
  })
  observeEvent({ # Obs Basis
    load2_dat()
    input$load2_x_axis
    input$load2_y_axis
  },
  {
    dat <- load2_dat()
    dat_std <- tourr::rescale(dat)
    pca <- prcomp(dat_std)
    x <- input$load2_x_axis
    y <- input$load2_y_axis
    x_num <- as.integer(substr(x, nchar(x), nchar(x)))
    y_num <- as.integer(substr(y, nchar(y), nchar(y)))
    rv$curr_basis <- pca$rotation[, c(x_num, y_num)]
  })
  
  ### Obs manip_slider motion
  observeEvent(input$manip_slider, {
    if (input$manip_var != "<none>") {
      theta <- phi <- NULL
      mv_sp <- create_manip_space(rv$curr_basis, manip_var_num())[manip_var_num(), ]
      if ("Radial" == "Radial") { # Fixed to "Radial" # input$manip_type == "Radial"
        theta <- atan(mv_sp[2] / mv_sp[1])
        phi_start <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
        phi <- (acos(input$manip_slider) - phi_start) * - sign(mv_sp[1])
      }
      ret <- oblique_basis(basis = rv$curr_basis, manip_var = manip_var_num(),
                           theta = theta, phi = phi)
      row.names(ret) <- colnames(load2_dat())
      
      rv$curr_basis <- ret
    }
  })
  ### Obs update manip_slider
  observeEvent( # Update slider value if other parameters change
    {manip_var_num()
      load2_dat()
      input$load_x_axis
      input$load_y_axis
    },
    {
      if (is.null(rv$curr_basis)) {stop("curr basis null")} # init curr_basis
      mv_sp <- create_manip_space(rv$curr_basis, manip_var_num())[manip_var_num(), ]
      if ("Radial" == "Radial") { # Fixed to Radial # input$manip_type == "Radial"
        phi_i <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
        this_val <- round(cos(phi_i), 1) # Rad
      }
      updateSliderInput(session, "manip_slider", value = this_val)
    })
  
  
  ### Outputs -----
  # Generate:
  output$task_pca        <- renderPlot({task_pca()}) 
  output$task_gtour      <- renderPlotly({task_gtour()})
  # Review (static + gtour):
  output$load_pca        <- renderPlot({load_pca()}) 
  output$load_gtour      <- renderPlotly({load_gtour()})
  output$str_load_dat    <- renderPrint({str(load_dat())})
  output$load_dat_attr   <- renderPrint(cat("loaded sim parameter -- \n",
                                            "p: ", ncol(load_dat()), "\n",
                                            "pnoise: ", load_pnoise(), "\n",
                                            "cl: ", length(attr(load_dat(), "ncl")), "\n",
                                            sep = ""))
  output$load_mncl_reord  <- renderPrint(load_mncl_reord())
  output$load_vc_reord    <- renderPrint(load_vc_reord())
  # Review (manual):
  output$load2_manual     <- renderPlot({load2_manual()}) 
  output$str_load2_dat    <- renderPrint({str(load2_dat())})
  output$load2_dat_attr   <- renderPrint(cat("loaded sim parameter -- \n",
                                          "p: ", ncol(load2_dat()), "\n",
                                          "pnoise: ", load2_pnoise(), "\n",
                                          "cl: ", length(attr(load2_dat(), "ncl")), "\n",
                                          sep = ""))
  output$load2_mncl_reord <- renderPrint(load_mncl_reord())
  output$load2_vc_reord   <- renderPrint(load_vc_reord())
  output$load2_curr_basis <- renderTable(rv$curr_basis)
  
  
  ### Dev msg -----
  output$dev_msg <- renderPrint(cat("dev msg -- \n",
                                    "sim_p: ", input$sim_p, "\n",
                                    "sim_pnoise: ", input$sim_pnoise, "\n",
                                    "sim_cl: ", input$sim_cl, "\n",
                                    "head(loaded_sim_names): ", head(loaded_sim_names), "\n",
                                    "load_num:", load_num, "\n",
                                    "input$load_x_axis: ", input$load_x_axis, "\n",
                                    "input$load_y_axis: ", input$load_y_axis, "\n",
                                    "rv$curr_basis: ", rv$curr_basis, "\n",
                                    sep = ""))
}

### Combine as shiny app.
shinyApp(ui = ui, server = server)


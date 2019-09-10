source('global.R', local = TRUE)

library("GGally")

##### Private, static content ----
### Create PCA ggplots, 'pca_',blck,rep
for (blck in s_blocks){
  for (rep in c("demo", s_reps)){
    if (rep == "demo") {
      pca <- data.frame(prcomp(demo_dat)$x)
    } else {
      pca <- data.frame(prcomp(s_samp_dat[[as.integer(rep)]])$x)
    }
    plot <- ggplot(pca, mapping = aes(x = PC1, y = PC2)) + geom_point() +
      theme_minimal()
    assign(paste0("pca_", blck, rep), plot)
  }
} 

### Create SPLOM ggplots, 'splom_',blck,rep
# TODO: currently no way to reach splom plots. add radio buttons to ui.
for (blck in s_blocks){
  for (rep in c("demo", s_reps)){
    if (rep == "demo") {
      dat <- demo_dat
    } else {
      dat <- s_samp_dat[[as.integer(rep)]]
    }
    plot <- GGally::ggpairs(data = dat, alpha = .2) +
      theme_minimal()
    assign(paste0("splom_", blck, rep), plot)
  }
} 


##### UI, combine tabPanels -----
ui <- fluidPage( ### INPUT, need to size to number of reps
  titlePanel("Multivariate data visualization study"),
  navlistPanel(
    panel_study_intro,
    "Number of clusters, n",
    panel_n_intro,
    panel_n1,
    panel_n2,
    panel_n3,
    "Important dimensions, d",
    panel_d_intro,
    panel_d1,
    panel_d2,
    panel_d3,
    "Covariance, s",
    panel_s_intro,
    panel_s1,
    panel_s2,
    panel_s3,
    "Wrap up",
    panel_survey,
    panel_finalize
  )
  , verbatimTextOutput("dev_msg")
)

##### Server function, dynamic outputs ----
server <- function(input, output, session) {  ### INPUT, need to size to number of reps
  output$plot_n1 <- renderPlot(pca_n1)
  output$plot_n2 <- renderPlot(pca_n2)
  output$plot_n3 <- renderPlot(pca_n3)
  output$plot_d1 <- renderPlot(pca_d1)
  output$plot_d2 <- renderPlot(pca_d2)
  output$plot_d3 <- renderPlot(pca_d3)
  output$plot_s1 <- renderPlot(pca_s1)
  output$plot_s2 <- renderPlot(pca_s2)
  output$plot_s3 <- renderPlot(pca_s3)
  output$plot_ndemo <- renderPlot(pca_ndemo)
  output$plot_ddemo <- renderPlot(pca_ddemo)
  output$plot_sdemo <- renderPlot(pca_sdemo)
  output$ans_tbl <- renderTable({
    ans_tbl()[ , !(names(ans_tbl()) %in% "dataset")] # mask dataset from users
  })
  output$dev_msg <- renderPrint(cat("dev msg -- \n",
                                    "browser: ", input$browser, "\n",
                                    sep = ""))
  
  ans_tbl <- reactive({
    data.frame(blockrep = col_blockrep, ### INPUT, need to size to number of reps
               question = col_question,
               dataset = col_dataset,
               answer = c(input$ans_n1, 
                          input$ans_n2,
                          input$ans_n3,
                          input$ans_d1,
                          input$ans_d2,
                          input$ans_d3,
                          input$ans_s1,
                          input$ans_s2,
                          input$ans_s3,
                          input$ans_ease,
                          input$ans_confidence,
                          input$ans_understand,
                          input$ans_use,
                          input$ans_high_dim,
                          input$ans_data_vis,
                          input$ans_previous_knowledge
                          )
    )
  })
  
  observeEvent(input$save_ans, {
    df <- ans_tbl()
    if (max(is.na(df)) == 1) { # Check that all tasks have answers.
      output$save_msg <- renderText("Please verify that all questions have been answered.")
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
}

### Combine as shiny app.
shinyApp(ui = ui, server = server)


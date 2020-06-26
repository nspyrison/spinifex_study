library("ggplot2")

n_controls <- 1:4
s_controls <- c("p", "n", "d", "s")
s_reps <- 1:4
s_questions <- c("How many dimensions does the data have?",
                 "How many clusters exsit?",
                 "How few dimensions could the data be represented in?",
                 "Which dimensions are highly correlated?")

### Create plots, 'plot_',ctrl,rep
x <- NULL
gg <- NULL
for (ctrl in s_controls){
  for (rep in s_reps){
    x <- data.frame(prcomp(mtcars)$x)
    gg <- ggplot(x, mapping = aes(x = PC1, y = PC2)) + geom_point()
    assign(paste0("plot_", ctrl, rep), gg)
  }
}

### Create tabPanels, 'panel_',ctrl,rep
panel = NULL
for (i in n_controls){
  for (rep in s_reps){
    ctrl <- s_controls[i]
    quest <- s_questions[i]
    panel <- tabPanel(paste0("task ", ctrl, rep), 
                      h2(paste0("task ", ctrl, rep)),
                      plotOutput(paste0(paste0("plot_", ctrl, rep))),
                      h3(quest),
                      numericInput(paste0("plot_", ctrl, rep), quest, "")
    )
    assign(paste0("panel_", ctrl, rep), panel)
  }
}


panel_intro <- tabPanel("Introduction",
                        h3("Welcome to the study")
)

panel_survey <- 
  tabPanel("Survey",
           sliderInput("q_easeofuse", "How easy was this to use?",
                       min = 1, max = 10, value = NULL
           ),
           sliderInput("q_understand", "How easy was this to understand?",
                       min = 1, max = 10, value = NULL
           ),
           sliderInput("q_useagain", "How likely would you be to use this visual?",
                       min = 1, max = 10, value = NULL
           )
  )

panel_finalize <- tabPanel("finalize",
                           h4("Thank you for participating.")
)


ui <- fluidPage(
  titlePanel("Multivariate data visualization survey"),
  navlistPanel(
    panel_intro,
    "Data diminsionality, p",
    panel_p1,
    panel_p2,
    panel_p3,
    panel_p4,
    "Number of clusters, n",
    panel_n1,
    panel_n2,
    panel_n3,
    panel_n4,
    "Important dimensions, d",
    panel_d1,
    panel_d2,
    panel_d3,
    panel_d4,
    "Variance-covariance, s",
    panel_s1,
    panel_s2,
    panel_s3,
    panel_s4,
    panel_survey,
    panel_finalize
  )
  
)

server <- function(input, output) {
  output$plot_p1 <- renderPlot(plot_p1)
  output$plot_p2 <- renderPlot(plot_p2)
  output$plot_p3 <- renderPlot(plot_p3)
  output$plot_p4 <- renderPlot(plot_p4)
  output$plot_n1 <- renderPlot(plot_n1)
  output$plot_n2 <- renderPlot(plot_n2)
  output$plot_n3 <- renderPlot(plot_n3)
  output$plot_n4 <- renderPlot(plot_n4)
  output$plot_d1 <- renderPlot(plot_d1)
  output$plot_d2 <- renderPlot(plot_d2)
  output$plot_d3 <- renderPlot(plot_d3)
  output$plot_d4 <- renderPlot(plot_d4)
  output$plot_s1 <- renderPlot(plot_s1)
  output$plot_s2 <- renderPlot(plot_s2)
  output$plot_s3 <- renderPlot(plot_s3)
  output$plot_s4 <- renderPlot(plot_s4)
  
  out_tbl <- reactive({
    
  })
}

shinyApp(ui = ui, server = server)


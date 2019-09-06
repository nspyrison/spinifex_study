##### Initialization -----
library("ggplot2")
library("spinifex")

dat <- tourr::rescale(tourr::flea[, 1:6])
#dat <- c(flea[, 1:6], olive, wine, mtcars)

n_blocks <- 1:4
s_blocks <- c("n", "d", "s")
s_reps <- 1:4
s_questions <- c("How many clusters exist?",
                 "How few dimensions could the data be represented in?",
                 "Which dimensions are highly correlated?")

### Create ggplots, 'plot_',blck,rep
pca <- NULL
plot <- NULL
for (blck in s_blocks){
  for (rep in c("demo", s_reps)){
    pca <- data.frame(prcomp(dat)$x)
    plot <- ggplot(pca, mapping = aes(x = PC1, y = PC2)) + geom_point()
    assign(paste0("plot_", blck, rep), plot)
  }
}

### Create tabPanels, 'panel_',blck,rep
panel = NULL
for (i in n_blocks){
  for (rep in c("demo", s_reps)){
    blck <- s_blocks[i]
    quest <- s_questions[i]
    panel <- tabPanel(paste0("Task ", blck, rep), 
                      h2(paste0("Task ", blck, rep)),
                      plotOutput(paste0("plot_", blck, rep)),
                      numericInput(paste0("ans_", blck, rep), quest, "")
    )
    assign(paste0("panel_", blck, rep), panel)
  }
}

### Introduction tabPanels -----
panel_intro <- tabPanel("Study introduction",
                        h3("Welcome to the study")
)
panel_n_intro <- tabPanel("Introduction -- clusters, n",
                          h3("In this section you will be asked to determine the number of clusters contained in the data."),
                          h4("Each task contains different data, with the display as demonstrated below."),
                          h4("You have 2 minutes to study the display before being prompted to submit your answer."),
                          plotOutput("plot_ndemo")
)
panel_d_intro <- tabPanel("Introduction -- important variables, d",
                          h3("In this section you will be asked to determine the number of how few variables accurately portray the variation in the data."),
                          h4("Each task contains different data, with the display as demonstrated below."),
                          h4("You have 2 minutes to study the display before being prompted to submit your answer."),
                          plotOutput("plot_ddemo")
)
panel_s_intro <- tabPanel("Introduction -- covariance, s",
                          h3("In this section you will be asked to determine which variables are highly correlated."),
                          h4("Each task contains different data, with the display as demonstrated below."),
                          h4("You have 2 minutes to study the display before being prompted to submit your answer."),
                          plotOutput("plot_sdemo")
)

### Survey tabPanel ----
survey_questions <- c("This visualization was easy to use.",
                      "I am confident of my answers.",
                      "This visualization is easily understandable.",
                      "I would recomend using this visualization.",
                      "I am an expert on multivarite data and related visualiztion.",
                      "I have broad experience with data disualization.",
                      "I had previous knowledge of this visualization.")
panel_survey <- 
  tabPanel("Survey",
           h3("How much do you agree with the following statments."),
           h4(survey_questions[1]),
           sliderInput("ans_ease", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           ),
           h4(survey_questions[2]),
           sliderInput("ans_confidence", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           ),
           h4(survey_questions[3]),
           sliderInput("ans_understand", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           ),
           h4(survey_questions[4]),
           sliderInput("ans_use", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           ),
           h4(survey_questions[5]),
           sliderInput("ans_high_dim", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           ),
           h4(survey_questions[6]),
           sliderInput("ans_data_vis", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           ),
           h4(survey_questions[7]),
           sliderInput("ans_previous_knowledge", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           ) # ask for sex?
)

### Answer table columns
col_blockrep <- c(paste0(rep(s_blocks, each=max(s_reps)), s_reps),
                  paste0("survey", 1:7))
col_question <- c(rep(s_questions, each = 4),
                  survey_questions)
#col_dataset <- 

panel_finalize <- tabPanel("Review answers",
                           tableOutput("ans_tbl"),
                           actionButton("save_ans", "save results"),
                           verbatimTextOutput("save_msg"),
                           h4("Thank you for participating.")
)

##### ui, combine tabPanels -----
ui <- fluidPage(
  titlePanel("Multivariate data visualization study"),
  navlistPanel(
    panel_intro,
    "Number of clusters, n",
    panel_n_intro,
    panel_n1,
    panel_n2,
    panel_n3,
    panel_n4,
    "Important dimensions, d",
    panel_d_intro,
    panel_d1,
    panel_d2,
    panel_d3,
    panel_d4,
    "Covariance, s",
    panel_s_intro,
    panel_s1,
    panel_s2,
    panel_s3,
    panel_s4,
    "Wrap up",
    panel_survey,
    panel_finalize
  )
  , verbatimTextOutput("dev_msg")
)

##### server, render outputs ----
server <- function(input, output, session) {
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
  output$plot_pdemo <- renderPlot(plot_pdemo)
  output$plot_ndemo <- renderPlot(plot_ndemo)
  output$plot_ddemo <- renderPlot(plot_ddemo)
  output$plot_sdemo <- renderPlot(plot_sdemo)
  output$ans_tbl <- renderTable(ans_tbl())
  
  output$dev_msg <- renderPrint(cat("dev msg -- \n",
                                    "browser: ", input$browser, "\n",
                                    sep = ""))
  
  ans_tbl <- reactive({
    data.frame(blockrep = col_blockrep,
               question = col_question,
               #dataset = col_dataset,
               answer = c(input$ans_n1,
                          input$ans_n2,
                          input$ans_n3,
                          input$ans_n4,
                          input$ans_d1,
                          input$ans_d2,
                          input$ans_d3,
                          input$ans_d4,
                          input$ans_s1,
                          input$ans_s2,
                          input$ans_s3,
                          input$ans_s4,
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
    if (max(is.na(df)) == 1) {
      output$save_msg <- renderText("Please verify that all questions have been answered.")
      return()
    }
    if (min(df[(nrow(df) - 6):nrow(df), 3] == 5) == 1) {
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


#rm(list = ls())
library(shiny)
questions <- c("What is your name?","Can you code in R?","Do you find coding fun?","Last Question:How old are you?")

ui <- pageWithSidebar(
  headerPanel("Exam"),
  sidebarPanel(actionButton("goButton", "Next"),p("Next Question")),
  mainPanel(verbatimTextOutput("nText")))

server <- function(input, output,session) {
  
  # Inititating reactive values, these will `reset` for each session
  # These are just for counting purposes so we can step through the questions
  values <- reactiveValues()
  values$count <- 1
  
  # Reactive expression will only be executed when the button is clicked
  ntext <- eventReactive(input$goButton,{
    
    # Check if the counter `values$count` are not equal to the length of your questions
    # if not then increment quesions by 1 and return that question
    # Note that initially the button hasn't been pressed yet so the `ntext()` will not be executed
    if(values$count != length(questions)){
      values$count <- values$count + 1
      return(questions[values$count])
    }
    else{
      # otherwise just return the last quesion
      return(questions[length(questions)])
    }
  })
  
  output$nText <- renderText({
    # The `if` statement below is to test if the botton has been clicked or not for the first time,
    # recall that the button works as a counter, everytime it is clicked it gets incremented by 1
    # The initial value is set to 0 so we just going to return the first question if it hasnt been clicked
    if(input$goButton == 0){
      return(questions[1])
    }
    ntext()
  })  
}
shinyApp(ui = ui, server = server)
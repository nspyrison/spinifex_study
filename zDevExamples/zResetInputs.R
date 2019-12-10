library(shiny)

runApp(list(
  
  ui = pageWithSidebar(
    
    headerPanel("'Reset inputs' button example"),
    
    sidebarPanel(
      uiOutput('resetable_input'),
      tags$hr(),
      actionButton("reset_input", "Reset inputs")
    ),
    
    mainPanel(
      h4("Summary"),
      verbatimTextOutput("summary")
    )
    
  ),
  
  server = function(input, output, session) {
    
    output$summary <- renderText({
      return(paste(input$mytext, input$mynumber))
    })
    
    output$resetable_input <- renderUI({
      times <- input$reset_input
      div(id=letters[(times %% length(letters)) + 1],
          numericInput("mynumber", "Enter a number", 20),
          textInput("mytext", "Enter a text", "test"))
    })
    
  }
))
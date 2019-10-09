# Example from:
# https://stackoverflow.com/questions/21609436/r-shiny-conditionalpanel-output-value

shiny::runApp(list( 
  ui = pageWithSidebar(
    
    headerPanel("test"),
    
    sidebarPanel(
      selectInput(
        "var", "Var",
        0:9)),
    
    mainPanel(
      verbatimTextOutput("id"),
      conditionalPanel(
        condition="output.id!=0",
        h4('Visible')
      )
    )
  ),
  server = function(input, output) {
    
    output$id<-reactive({input$var})
    
  }
))
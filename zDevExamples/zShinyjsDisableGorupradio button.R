library(shiny)
library(shinyjs)
library(shinyWidgets)
if (interactive()) {
  
  ui <- fluidPage(
    useShinyjs(),
    shinyWidgets::radioGroupButtons(inputId = "somevalue", choices = c("A", "B", "C")),
    verbatimTextOutput("value")
  )
  server <- function(input, output) {
    output$value <- renderText({ input$somevalue })
    
    shinyjs::disable(id="somevalue")
    
  }
  shinyApp(ui, server)
}

library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),  # Include shinyjs
  
  actionButton("button", "Click me"),
  checkboxGroupInput("box", "check some boxes", 1:4, 2)
)

server <- function(input, output) {
  observeEvent(input$button, {
    shinyjs::toggleState("box")  # toggle is a shinyjs function
  })
}

shinyApp(ui, server)
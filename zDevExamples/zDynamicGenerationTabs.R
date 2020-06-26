library(shiny)   

ui <- (basicPage( 
  tabsetPanel(id = "tabs",
              tabPanel("Tab A", value = "A","This is Tab A content",textOutput("tabA")),
              tabPanel("Tab B", value = "B","Here's some content for tab B.",textOutput("tabB")))
))

server <- function(input, output, session) {
  
  observe({
    output[[paste0("tab", input$tabs)]] <- renderText({paste0("You are viewing tab ", input$tabs)})
  })
}

shinyApp(ui = ui, server = server)

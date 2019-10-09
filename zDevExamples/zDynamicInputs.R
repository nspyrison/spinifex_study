ui = pageWithSidebar(
  headerPanel("dynamic inputs test"),
  
  sidebarPanel(
    numericInput("inputs", "# of inputs", value = 3, min=1, max=10),
    numericInput("options", "# of options", value = 3, min=1, max=10)
  ),
  
  mainPanel(
    uiOutput("allInputs")
  )
  
)

server = function(input, output) {
  
  output$allInputs <- renderUI({
    i <- as.integer(input$inputs)
    j <- as.integer(input$options)
    lapply(1:i, function(i) {
      radioButtons(inputId = paste0("input_", i), label = paste("input", i),
                   choices = 1:j, inline = TRUE)
    })
  })
  
}

shinyApp(ui = ui, server = server)
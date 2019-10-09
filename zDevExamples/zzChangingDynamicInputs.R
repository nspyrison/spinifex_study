
ui = pageWithSidebar(
  headerPanel("test"),
  sidebarPanel(
    numericInput("inputs", "# of inputs", value = 3, min=1, max=10),
    numericInput("options", "# of options", value = 3, min=1, max=10)
  ),
  mainPanel(
    uiOutput("allInputs")
  )
)

server = function(input, output) {
  # Initialize list of inputs
  inputTagList <- tagList()
  
  output$allInputs <- renderUI({
    # Get value of button, which represents number of times pressed (i.e. number of inputs added)
    i <- input$inputs
    # Define unique input id and label
    newInputId <- paste0("input", i)
    newInputLabel <- paste("Input", i)
    # Define new input
    newInput <- radioButtons(newInputId, newInputLabel, 1:input$options)
    # Append new input to list of existing inputs
    inputTagList <<- tagAppendChild(inputTagList, newInput)
    # Return updated list of inputs
    inputTagList
  })
}


shinyApp(ui = ui, server = server)

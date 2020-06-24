library(shiny)
server = shinyServer(function(input, output, session) {
  
  output$color_pr <- renderPrint({
    req(input$select1)
    input$select1
  })
  
  output$panelStatus <- reactive({
    #input$select1=="show"
    T == T
  })
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
  
})

ui=shinyUI(fluidPage(
  
  radioButtons("select1", "Show text?",
               c("Yes" = "show", "No" = "noshow")),
  
  conditionalPanel(
    condition = 'output.panelStatus',
    verbatimTextOutput("color_pr"),
    verbatimTextOutput("panelStatus")
  )
))

shinyApp(ui=ui,server=server)
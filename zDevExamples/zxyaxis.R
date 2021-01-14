library(shiny)
opts <- paste0("PC", 1:4)
if (interactive()) {
  ui <- fluidPage(
    shiny::radioButtons("x_axis", "x axis", opts, opts[1], TRUE),
    shiny::radioButtons("y_axis", "y axis", opts, opts[2], TRUE),
    verbatimTextOutput("value"),
    actionButton("button", "reset y axis")
  )
  
  server <- function(input, output, session) {
    output$value <- renderText({ paste0("X: ", input$x_axis, ", Y: ", input$y_axis) })
    observeEvent(input$button, {
      updateRadioButtons(session, "y_axis", "y axis", choices = opts,
                         1, inline = TRUE)
    })
    observeEvent(input$x_axis, {
      if(input$x_axis == input$y_axis){
        sel <- opts[!(opts %in% input$x_axis)][1] ## First option not eq to x_axis
        updateRadioButtons(session, "y_axis", "y axis", opts,
                           sel, inline = TRUE)
        showNotification("notification", type = "message")
      }
    })
    observeEvent(input$y_axis, {
      if(input$x_axis == input$y_axis){
        sel <- opts[!(opts %in% input$y_axis)][1] ## First option not eq to y_axis
        updateRadioButtons(session, "x_axis", "x axis", opts,
                           sel, inline = TRUE)
        showNotification("notification", type = "message")
      }
    })

  }
  shinyApp(ui, server)
}

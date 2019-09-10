library(lubridate)
library(shiny)

ui <- fluidPage(
  hr(),
  actionButton('start','Start'),
  actionButton('stop','Stop'),
  actionButton('reset','Reset'),
  textOutput('timeleft')
)

server <- function(input, output, session) {
  
  # Initialize the timer, 10 seconds, not active.
  timer <- reactiveVal(12)
  active <- reactiveVal(FALSE)
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          showModal(modalDialog(
            title = "Important message",
            "Countdown completed!"
          ))
        }
      }
    })
  })
  
  # observers for actionbuttons
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$reset, {timer(12)})
  
}

shinyApp(ui, server)
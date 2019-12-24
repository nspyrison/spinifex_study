library(shiny)
library("ggplot2")
start <- Sys.time()
lag1 <- Sys.time()
different_scope <- TRUE

ui<-  shinyUI(fluidPage())

server<- shinyServer(function(input, output,session) {
  
  observe({
    invalidateLater(1000, session)

    now <- Sys.time()
    diff <- round(difftime(now, lag1, units = "secs"), 3)
    tot  <- round(difftime(now, start, units = "secs"), 3)
    lag1 <<- now

    if (different_scope){
      cat(paste0("tot: ", tot, " diff: ", diff, " [seconds] \n"))
    }
    
  })
 
})  
shinyApp(ui=ui, server=server) 
## Follow allong here: https://shiny.rstudio.com/articles/persistent-data-storage.html#gsheets
# saveData <- function(data) {
#   # The data must be a dataframe rather than a named vector
#   data <- data %>% as.list() %>% data.frame()
#   # Add the data as a new row
#   sheet_append(SHEET_ID, data)
# }
# loadData <- function() {
#   # Read the data
#   read_sheet(SHEET_ID)
# }

### Setup -----
library("shiny")
library("googlesheets4") ## Google sheets (with api v4) for read/write responses.
##seems to work fine without
# library("googledrive")
# library("tibble")
ss_id <- "1KaWOFGyfosxdMBHDXvEn3YMmdA_o7tsZ-ILyqjtbqQA" ## the 'id' or name of the google sheet, 1Ka* is "TEST_gsheet"


##ui.r BELOW ------
ui <- fluidPage(
  actionButton("add_row", "Add a time stamp row row to GS."),
  textOutput("txt"),
  actionButton("stop", "stop app via function")
)


### server.r BELOW -----
server <- function(input, output, session){
  
  ## Google sheets authentication
  tryCatch({
    drive_auth(email = "nicholas.spyrison@monash.edu")
  }, error = function(e){
    txt <- "App could not authenticate to Google sheet. Please try again in 5 minutes. Closing app in 15 seconds."
    showNotification(txt, type = "error")
    warning(txt)
    Sys.sleep(15)
    stopApp()
    return(NULL)
  })
  
  
  observeEvent(
    input$add_row,
    {
      sys_info <- as_tibble(t(Sys.info()[1:4]))
      df_to_add <- tibble(sys_info, ff_text = paste0("Hello google sheets! ", Sys.time()))
      
      sheet_append(ss_id, df_to_add) ## appends row(s), must be a df.
      output$txt <- renderText({paste0("Last called sheet_append() @ ", Sys.time())})
    })
  
  
  observeEvent(
    input$stop,
    {
      stopApp("string in StopApp()")
    })
  
} ## End server function

### Combine as shiny app.
shinyApp(ui = ui, server = server)

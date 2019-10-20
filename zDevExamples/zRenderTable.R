## Only run this example in interactive R sessions

  # table example
  shinyApp(
    ui = fluidPage(
               tableOutput('table'),
               tableOutput('TEST_tbl')
    ),
    server = function(input, output) {
      output$TEST_tbl      <- renderTable(head(iris))
      output$table         <- renderTable(iris)
    }
  )

#   
#   # DataTables example
#   shinyApp(
#     ui = fluidPage(
#       fluidRow(
#         column(12,
#                dataTableOutput('table')
#         )
#       )
#     ),
#     server = function(input, output) {
#       output$table <- renderDataTable(iris)
#     }
#   )
# }
library(shiny)

ui <- fluidPage(
  headerPanel("renderImage example"),
  textOutput("fp"),
  # Use imageOutput to place the image on the page
  sliderInput("n", label = "Image num", min = 1, value = 1, max = 3, step = 1),
  imageOutput("preImage")
  
)

server <- function(input, output, session) {
  filename <- reactive({
    # normalizePath(file.path('./images',
    #                         paste(input$n, '.png', sep='')))
    normalizePath(file.path("../apps/images/EEE_p4_0_1_t1__pca_x1_y2.png"))
  })
  output$fp <- renderText({filename()})

  output$preImage <- renderImage({
    # When input$n is 3, filename is ./images/3.png
    # Return a list containing the filename and alt text
    list(src = filename(),
         alt = paste("Image number", input$n))
  }, deleteFile = FALSE)
}

shinyApp(ui, server)
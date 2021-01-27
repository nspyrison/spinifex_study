library(shiny)
imgs_fp <- c("EEE_P4_0_1_t1__pca_x1y2.png", 
             "EEE_P4_0_1_t1__grand.gif", 
             "EEE_P4_0_1_t1__radial_mv2.gif")

if (interactive()) {
  ui <- fluidPage(
    h3("Images in ui"),
    column(4, 
           p("pca"),
           img(src = imgs_fp[1])),
    column(4, 
           p("grand"),
           img(src = imgs_fp[2])),
    column(4, 
           p("radial"),
           img(src = imgs_fp[3])),
    
        
    h3("Images from server renderImage."),
    imageOutput("image_plot"),
    # p("=== dir() info ===="), 
    # p(paste(dir("./apps/spinifex_study/www/images/"), collapse = ", "))
  )
  
  server <- function(input, output, session) {
    output$image_plot <- renderImage({
      list(src = normalizePath("./www/test.gif"))
    }, deleteFile = FALSE)

  }
  shinyApp(ui, server)
}

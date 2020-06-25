## following:
if (F)
  browseURL("https://stackoverflow.com/questions/31703241/activate-tabpanel-from-another-tabpanel/31719425#31719425")

library("shiny")
library("shinyjs")
require("V8") ## req for shinyjs::extendShinyjs()

app_jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

app_css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"

ui = fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = app_jscode),
  shinyjs::inlineCSS(app_css),
  navbarPage(title = "Navbar title!", id = "navbarid",
    tabPanel(title = "tab1", ## id and value args not needed
             br(),
             p("in tab 1."),
             actionButton("btn", label = "toggle locked tabs")),
    tabPanel(title = "tab2", ## id and value args not needed
             p("in tab 2."))
  )
)
server = function(input, output, session) {
  ## Sisable tab2 on page load
  js$disableTab("tab2")
  
  observeEvent(input$btn, {
    ## Enable tab2 when clicking the button
    shinyjs::js$enableTab("tab2") ## On a tab's title
    ## Switch to tab2
    updateNavbarPage(session, "navbarid", "tab2") ## On navbar's id, tab's title
    ### Taking it further:
    shinyjs::js$disableTab("tab1")
  })
}
shinyApp(ui = ui, server = server)


## TODO: reinit this file from the most recent changes from global_static. this
## version has been gutted to test the manual control app.
## Nick 20/10/2019
##


### Global init for each factor variant.


##### App static global.r -----
### Initialization 
library("ggplot2")
library("spinifex")
library("shiny")
library("tidyr")
library("plotly")
library("lubridate") # For timer
library("reactlog")  # Logging
library("GGally")


### Required inputs -----
##################################
##TODO: COPY CONTENT FROM _static
## replace 7 with 10 in the app_gtour.r
n_reps <- 3
s_blocks <- c("n", "d", "s")
s_block_names <- c("clusters, n", "important variable, r", "correlated variables, s")
s_block_questions <- c("TO BE REPLACED.",
                       "TO BE REPLACED.",
                       "TO BE REPLACED.")
s_survey_questions <- c("TO BE REPLACED.",
                        "TO BE REPLACED.",
                        "TO BE REPLACED.",
                        "TO BE REPLACED.",
                        "TO BE REPLACED.",
                        "TO BE REPLACED.",
                        "TO BE REPLACED.")
###############################
study_factor <- "manual"

### Variable initialization ----
n_blocks <- length(s_blocks)
s_blockrep_id <- paste0(rep(s_blocks, each = n_reps), rep(1:n_reps, n_reps))
n_task_pgs <- n_blocks * (n_reps + 1)

sim_intro <- readRDS("../simulation/simulation_data021.rds") # p = 6, pnoise = 2, cl = 3 
sim1 <- readRDS("../simulation/simulation_data001.rds") # "./apps/simulation/simulation_data001.rds"
sim2 <- readRDS("../simulation/simulation_data002.rds")
sim3 <- readRDS("../simulation/simulation_data003.rds")
s_dat <- list(sim_intro, sim1, sim2, sim3)


###### Text initialization -----
##################
## TODO: DOUBLE CHECK THIS IS SAME AS STATIC APP>
intro_header_row <- paste0("Introduction -- ", s_block_names)
m_blockrep_id <- matrix(paste0("Task -- ", s_blockrep_id), ncol = n_reps)
s_header_text <- c(rbind(intro_header_row , m_blockrep_id), "All tasks completed. Continue to the Survey tab.")

blank_row <- rep("", n_blocks)
m_questions <- matrix(rep(s_block_questions, each = n_reps), ncol = n_reps)
s_question_text <- c(rbind(blank_row, m_questions), "")

intro_top_row <- paste0(c("In this section you will be asked to determine the number of clusters contained in the data. ",
                          "In this section you will be asked to determine the number of how few variables accurately portray the variation in the data. ",
                          "In this section you will be asked to determine which variables are highly correlated. "), 
                        "Each task contains different data, with the display as demonstrated below.")
m_blank <- matrix("", ncol = n_reps, nrow = n_blocks)
s_top_text <- c(rbind(intro_top_row, m_blank), "")

intro_bottom_row <- "You have 2 minutes to study the display before being prompted to submit your answer."
s_bottom_text <- c(rbind(intro_bottom_row, m_blank), "")
#####################

##### tabPanels (UI objs) -----
### Task panel -----
l_choices <- list(NULL)
for (i in 1:5){ 
  l_choices[[i]] <- i
}
names(l_choices) <- paste0("choice ", 1:5)
panel_task <- tabPanel(
  "Tasks", 
  sidebarPanel(
    fluidRow(column(6, radioButtons(inputId = "x_axis", label = "x axis", choices = "PC1")),
             column(6, radioButtons(inputId = "y_axis", label = "y axis", choices = "PC2")))
    , selectInput('manip_var', 'Manip var', "<none>")
    , sliderInput("manip_slider", "Contribution",
                  min = 0, max = 1, value = 0, step = .1)
    , hr() # horizontal line
    , conditionalPanel(condition = "output.block_num == 1",
                     numericInput("blk1_ans", "How many clusters exist within the data?",
                                  value = 0, min = 0, max = 10))
    , conditionalPanel(condition = "output.block_num == 2",
                       div(style = 'width:400px;',
                           div(style = 'float:left; color:red; font-size:14px', 
                               strong('most important')),
                           div(style = 'float:right; color:red; font-size:14px', 
                               strong('least important'))),
                       tags$br(),
                       uiOutput("blk2Inputs"))
    , conditionalPanel(condition = "output.block_num == 3",
                       uiOutput("blk3Inputs"))
    , hr()
    , actionButton("next_task_button", "Next task")
    ### , h3("Current basis") # too noisy for app?
    ### , tableOutput("basis_tbl") 
  ),
  mainPanel(textOutput('timer_disp')
            ###, verbatimTextOutput("header_text") # Kim asked to remove 18/10/2019
            , verbatimTextOutput("top_text")
            , plotOutput("task_manual", height = "auto")
            , verbatimTextOutput("question_text")
            , verbatimTextOutput("response_msg")
            , verbatimTextOutput("bottom_text")
  )
)

##### UI, combine panels -----
ui <- fluidPage(
  navbarPage("Multivariate data visualization study",
             panel_task)
  , verbatimTextOutput("dev_msg")
  , verbatimTextOutput("block_num") #!! Need to call block_num for condition panels to evaluate
)

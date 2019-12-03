### Global init for each factor variant.


##### App static global.r -----
### Initialization 
library("ggplot2")
library("spinifex")
library("shiny")
library("tidyr")
#library("mvtnorm")
library("plotly")
library("GGally")
library("lubridate") # For timer
library("loggit")    # For logging
## TODO: propagate library changes.

study_factor <- "static"

log_base <- paste0("log_", study_factor)
log_num  <- 1
log_name <- sprintf(paste0(log_base, "%03d"), log_num)
log_file <- paste0(log_name, ".json")
while (file.exists(log_file)){ # Find an unused log number
  log_name <- sprintf(paste0(log_base, "%03d"), log_num)
  log_file <- paste0(log_name, ".json")
  log_num  <- log_num + 1
}

### Logging
## https://www.r-bloggers.com/adding-logging-to-a-shiny-app-with-loggit/
## use: loggit("INFO", "<main msg>", "<detail>")
## Uncomment to capture log file:
setLogFile(log_file); try_autosave <- TRUE
loggit("INFO", "app has started", "spinifex_study")

### Required inputs -----
## TODO: make sure to duplicate this section in other apps
n_reps <- 3
s_blocks <- c("n", "d", "s")
s_block_names <- c("clusters, n", "important variable, r", "correlated variables, s")
s_block_questions <- c("How many clusters exist?",
                       "Rate the importance of each variable in terms of distinugishing the given cluster.",
                       "Group any/all correlated variables.")
s_survey_questions <- c("What gender are you?",
                        "What age are you?",
                        "What is your highest level of completed education?",
                        "This visualization was easy to use.",
                        "I am confident of my answers.",
                        "This visualization is easily understandable.",
                        "I would recommend using this visualization",
                        "I am an expert on multivariate  data and related visualization",
                        "I have broad experience with data visualization.",
                        "I had previous knowledge of this visualization.")
study_factor <- "static"

### Variable initialization ----
n_blocks       <- length(s_blocks)
s_blockrep_id  <- paste0(rep(s_blocks, each = n_reps), rep(1:n_reps, n_reps))
training_start <- 2 # pg 1 is intro, pg 2:4 is training
task_start     <- training_start + n_blocks # ~ pg 5:13 is task  
survey_start   <- task_start + n_reps * n_blocks # ~ pg 14 is survey 

sim1_num  <- "017"
sim2_num  <- "004"
sim3_num  <- "020"
sim_intro <- readRDS("../simulation/simulation_data021.rds") # p = 6, pnoise = 2, cl = 3 
sim1      <- readRDS(paste0("../simulation/simulation_data", sim1_num, ".rds")) # "./apps/simulation/simulation_data001.rds"
sim2      <- readRDS(paste0("../simulation/simulation_data", sim2_num, ".rds"))
sim3      <- readRDS(paste0("../simulation/simulation_data", sim3_num, ".rds"))
s_dat <- list(sim1, sim2, sim3)


##### main_ui -----
main_ui <- fluidPage(
  ### Side panel only on "training" and "task" sections
  conditionalPanel(
    condition = "output.ui_section == 'training' || output.ui_section == 'task'",
    sidebarPanel(
      fluidRow(column(6, radioButtons(inputId = "x_axis", label = "x axis", choices = "PC1")),
               column(6, radioButtons(inputId = "y_axis", label = "y axis", choices = "PC2"))
      ),
      hr(), # horizontal line
      conditionalPanel(condition = "output.block_num == 1",
                       tags$b(s_block_questions[1]),
                       tags$br(), ## TODO breaks don't seem to work... 
                       numericInput("blk1_ans", "",
                                    value = 0, min = 0, max = 10)
      ),
      conditionalPanel(condition = "output.block_num == 2",
                       tags$b(s_block_questions[2]),
                       tags$br(), br(),
                       uiOutput("blk2Inputs")
      ),
      conditionalPanel(condition = "output.block_num == 3",
                       tags$b(s_block_questions[3]),
                       tags$br(), br(),
                       uiOutput("blk3Inputs")
      )
    ) 
  ), ### end conditionalPanel sidebarPanel for training and task
  mainPanel(
    ### _Intro mainPanel -----
    conditionalPanel(
      condition = "output.ui_section == 'intro'",
      h3("Welcome to the study")
      , br()
      , p("This a completely voluntary study that will take approximately 25-30 
          minutes to complete. If at any point you would like to stop, 
          please let the proctor know.")
      , br()
      , p("You are helping to compare the effectiveness of different visuals of 
          linear projections for multivariate data. You will help evaluate 1 
          graphic variation by participating. 
          The outline of the study is as follows:")
      , tags$b("Training")
      , tags$ul(
        tags$li("Video training")
        , tags$li("Graphic and ui familiarity -- questions encouraged")
      )
      , tags$b("Expiriment -- 2 minutes per task, no questions")
      , tags$ul(
        tags$li(paste0("Task 1 (x3 reps) -- ",     s_block_questions[1]))
        , tags$li(paste0("Task 2 (x3 reps) -- ",   s_block_questions[2]))
        , tags$li(paste0("Task 3 (x3 reps) -- ", s_block_questions[3]))
      )
      , tags$b("Follow-up")
      , tags$ul(
        tags$li("Short questionnaire")
        , tags$li("Response submission")
      )
      , p("After completing the survey let the proctor know and collect a 
          voucher for a free hot berverage on campus.")
      , p("Thank you again for participating.")
    ), # close conditionPanel -- intro section text
    ### _Training mainPanel -----
    conditionalPanel(
      condition = "output.ui_section == 'training'",
      conditionalPanel(condition = "output.block_num == 1",
                       h2("Training -- task 1")),
      conditionalPanel(condition = "output.block_num == 2",
                       h2("Training -- task 2")),
      conditionalPanel(condition = "output.block_num == 3",
                       h2("Training -- task 3")),
      p("This data has 6 variables. Principle Componant Analysis (PCA) defines 
        new axes components (as linear combinations of the original variable),
        ordered by the amount of variation they explain. The plot below displays
        the data for the components selected on the sidebar to the left."),
      p("Take time to familiarize yourself with the controls and feel free to 
          ask any questions. During the evaluation section, you will have 2 
          minutes to explore the data, responding as accurately and quickly 
          as possible."),
      conditionalPanel( # first block text
        condition = "output.rep_num == 1",
        tags$b("The first task is to estimate the number clusters in the data. 
          Click on the radio buttons on the side bar to select different PC 
          combinations to better understand the clustertering of the data. 
          When you are ready enter the number of clusters on the sidebar then
          click the 'Next page' button below.")
      ),
      conditionalPanel( # second block text
        condition = "output.rep_num == 2",
        tags$b("The second task is to rate each variables importance for 
        distinguishing the listed cluster. The points have colored and shape 
        assigned by cluster. The variable map (grey circle) on the display 
        shows the direction and magnitude that each variable contributes to the 
        current axes. Use the variable map to identitify the variables that 
        distingish between clusters. Look at several componets to rate
        the top four variables that help distinguish clusters.
               \n \n
        Consider cluster 'a' (green circles). Variables 2, 4, and 6 have 
        relatively large magnitudes and are in directions that help distinguish
        the purple squares (V4) and the orange triangles (V2 and V6). 
        List V2, V4, and V6 as very important for distinguishing cluster 'a'.
        Remember the axes can be changed to look at the data from another 
        perspective;Look at the other 3 variable and see if they 
        contribute in separating dimensions. Continue to the next page 
        when you content.")
      ),
      conditionalPanel( # Third block text
        condition = "output.rep_num == 3",
        tags$b("The third task is to identify groups of correllated variables. 
        Variables that point in the same direction and both have large 
        contributions on the variable map correllated. Looking at differnet 
        components try to identify any and all groups of correllated 
        variables.")
      )
    ), # close training section main panel text
    ### _Task mainPanel -----
    conditionalPanel(
      condition = "output.ui_section == 'task'",
      conditionalPanel(condition = "output.block_num == 1",
                       h2("Evaluation -- task 1")
      ),
      conditionalPanel(condition = "output.block_num == 2",
                       h2("Evaluation -- task 2")
      ),
      conditionalPanel(condition = "output.block_num == 3",
                       h2("Evaluation -- task 3")
      ),
      textOutput('timer_disp')
    ), # close task section conditional panel title text
    ### _Plot mainPanel
    conditionalPanel( 
      condition = "output.ui_section == 'training' || output.ui_section == 'task'"
      , plotOutput("task_pca", height = "auto")
      , htmlOutput("plot_msg")
    ), # close plot conditional panel
    ### _Survey mainPanel -----
    conditionalPanel(
      condition = "output.ui_section == 'survey'",
      selectInput("ans_gender", label = s_survey_questions[1], 
                  choices = c("decline to answer",
                              "female",
                              "male",
                              "inter-gender/other")
      ),
      selectInput("ans_age", label = s_survey_questions[2], 
                  choices = c("decline to answer",
                              "19 or younger",
                              "20 to 29",
                              "30 to 39",
                              "40 or older")
      ),
      selectInput("ans_edu", label = s_survey_questions[3], 
                  choices = c("decline to answer",
                              "High school",
                              "Undergraduate",
                              "Honors, masters, mba", 
                              "Doctorate")
      ),
      h3("How much do you agree with the following statments?"),
      h4(s_survey_questions[4]),
      sliderInput("ans_ease",
                  label = div(style = 'width:300px;',
                              div(style = 'float:left;', 'strongly disagree'),
                              div(style = 'float:right;', 'strongly agree')),
                  min = 1, max = 9, value = 5),
      h4(s_survey_questions[5]),
      sliderInput("ans_confidence",
                  label = div(style = 'width:300px;',
                              div(style = 'float:left;', 'strongly disagree'),
                              div(style = 'float:right;', 'strongly agree')),
                  min = 1, max = 9, value = 5),
      h4(s_survey_questions[6]),
      sliderInput("ans_understand",
                  label = div(style = 'width:300px;',
                              div(style = 'float:left;', 'strongly disagree'),
                              div(style = 'float:right;', 'strongly agree')),
                  min = 1, max = 9, value = 5),
      h4(s_survey_questions[7]),
      sliderInput("ans_use",
                  label = div(style = 'width:300px;',
                              div(style = 'float:left;', 'strongly disagree'),
                              div(style = 'float:right;', 'strongly agree')),
                  min = 1, max = 9, value = 5),
      h4(s_survey_questions[8]),
      sliderInput("ans_high_dim",
                  label = div(style = 'width:300px;',
                              div(style = 'float:left;', 'strongly disagree'),
                              div(style = 'float:right;', 'strongly agree')),
                  min = 1, max = 9, value = 5),
      h4(s_survey_questions[9]),
      sliderInput("ans_data_vis",
                  label = div(style = 'width:300px;',
                              div(style = 'float:left;', 'strongly disagree'),
                              div(style = 'float:right;', 'strongly agree')),
                  min = 1, max = 9, value = 5),
      h4(s_survey_questions[10]),
      sliderInput("ans_previous_knowledge",
                  label = div(style = 'width:300px;',
                              div(style = 'float:left;', 'strongly disagree'),
                              div(style = 'float:right;', 'strongly agree')),
                  min = 1, max = 9, value = 5),
      actionButton("save_ans", "save responses"),
      htmlOutput("save_msg"),
      conditionalPanel(
        condition = "output.is_saved == 1",
        h3("Thank you for participating!"),
        br(),
        h4("Let the proctor know you have completed the study and have a good day.")
      )
    ) # close survey condition panel 
    
  ) # close mainPanel()
) # close fluid page wrapper 



##### UI, combine panels -----
ui <- fluidPage(
  titlePanel("Multivariate data visualization study"),
  main_ui,
  conditionalPanel(
    condition = "output.pg_num < 14",
    actionButton("next_pg_button", "Next page")
  )
  # , verbatimTextOutput("dev_msg")
  # , actionButton("browser", "browser()")
  # , tableOutput("ans_tbl")
)


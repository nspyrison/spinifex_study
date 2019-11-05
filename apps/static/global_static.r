### Global init for each factor variant.


##### App static global.r -----
### Initialization 
library("ggplot2")
library("spinifex")
library("shiny")
library("tidyr")
library("mvtnorm")
library("plotly")
library("GGally")
library("lubridate") # For timer
library("reactlog")  # Logging


### Required inputs -----
## TODO: make sure to duplicate this section in other apps
n_reps <- 3
s_blocks <- c("n", "d", "s")
s_block_names <- c("clusters, n", "important variable, r", "correlated variables, s")
s_block_questions <- c("How many clusters exist?",
                       "Rank the variables in order of importance to distinguish groups?",
                       "Group any/all correlated variables.")
s_survey_questions <- c("What gender are you?",
                        "What age are you?",
                        "What is your highest level of completed education?",
                        "This visualization was easy to use.",
                        "I am confident of my answers.",
                        "This visualization is easily understandable.",
                        "I would recommend using this visualization",
                        "I am an expert on multivariate  data and related visualization",
                        "I have broad experience with data disualization.",
                        "I had previous knowledge of this visualization.")
study_factor <- "static"

### Variable initialization ----
n_blocks       <- length(s_blocks)
s_blockrep_id  <- paste0(rep(s_blocks, each = n_reps), rep(1:n_reps, n_reps))
training_start <- 2
task_start     <- training_start + n_blocks
survey_start   <- task_start + n_reps * n_blocks

sim_intro <- readRDS("../simulation/simulation_data021.rds") # p = 6, pnoise = 2, cl = 3 
sim1      <- readRDS("../simulation/simulation_data001.rds") # "./apps/simulation/simulation_data001.rds"
sim2      <- readRDS("../simulation/simulation_data002.rds")
sim3      <- readRDS("../simulation/simulation_data003.rds")
s_dat <- list(sim_intro, sim1, sim2, sim3)


###### Text initialization -----
## TODO: make sure to duplicate this section in other apps
training_header_text <- paste0("Training -- ", s_block_names)
training_top_text <- c("In this section you will be asked to determine the number of clusters contained in the data. "
                       , "In this section you will be asked to determine the number of how few variables accurately portray the variation in the data. "
                       , "In this section you will be asked to determine which variables are highly correlated. ")
training_bottom_timer_text <- "You have 2 minutes to study the display before being prompted to submit your answer."

## DEV NOTE:
# s_question_text replaced with s_block_questions[block_num()]
# s_top_text replaced with training_top_text[block_num()]
# s_bottom_text replaced with training_bottom_timer_text


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
                       numericInput("blk1_ans", "How many clusters exist within the data?",
                                    value = 0, min = 0, max = 10)
      ),
      conditionalPanel(condition = "output.block_num == 2",
                       div(style = 'width:400px;',
                           div(style = 'float:left; color:red; font-size:14px', 
                               strong('most important')),
                           div(style = 'float:right; color:red; font-size:14px', 
                               strong('least important'))),
                       tags$br(),
                       uiOutput("blk2Inputs")
      ),
      conditionalPanel(condition = "output.block_num == 3",
                       uiOutput("blk3Inputs")
      ),
    ) 
  ), ### end conditionalPanel sidebarPanel for training and task
  mainPanel(
    ### _Intro mainPanel -----
    conditionalPanel(
      condition = "output.ui_section == 'intro'",
      h3("Welcome do the study.")
      , br()
      , p("This a completely voluntary study that will take approximately 25-30 
      minutes to complete. If at any point you would like to stop, please let 
          the proctor know.")
      , br()
      , p("You are helping to compare the effectiveness of different visuals of 
          linear projection for multi-variate data. You will be assigned to one 
          graphic, go through a training period, experiment through 3 diffent 
          tasks and fill a short follow-up survey. 
          The outline of the study is as follows:")
      , tags$b("Training")
      , tags$ul(
        tags$li("Video training")
        , tags$li("Graphic and ui familiarity -- questions encouraged")
      )
      , tags$b("Expiriment -- 2 minutes per task, no questions")
      , tags$ul(
        tags$li("Task one (3 reps) -- How many clusters are contained within the data?")
        , tags$li("Task two (3 reps) -- Rank the most important variables distinguishing groups")
        , tags$li("Task three (3 reps) -- Group correlated variables (if any)")
      )
      , tags$b("Follow-up")
      , tags$ul(
        tags$li("Short questionnaire")
        , tags$li("Response submission")
      )
      , p("Make sure to provide your email address and computer to the proctor
          if you wish to be entered in the prize pool for top three scoring 
          participants will receive a $50 gift card to Coles. Also, mark if you 
          want to be emailed about the subsequent publication.")
      , p("Thank you again for participating.")
    ), # close conditionPanel -- intro
    ### _Training mainPanel -----
    conditionalPanel(
      condition = "output.ui_section == 'training'",
      h1("TRAINING CONTENT HERE.")
      , h4(training_bottom_timer_text)
    ),
    ### _Task mainPanel -----
    conditionalPanel(
      condition = "output.ui_section == 'task'",
      h1("TASK CONTENT HERE.")
      , textOutput('timer_disp')
    ),
    ### _bottom half of training and task mainPanels
    conditionalPanel( 
      condition = "output.ui_section == 'training' || output.ui_section == 'task'"
      , plotOutput("task_pca", height = "auto")
      , verbatimTextOutput("question_text")
      , verbatimTextOutput("response_msg")
    ),
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
      verbatimTextOutput("save_msg"),
      h4("Thank you for participating.")
    ) # close survey mainPanel
    
  ) # close mainPanel()
) # close main_panel assignment 


### TEST_UI_SECTION -----
TEST_ui_section <- fluidPage(
  sidebarPanel(
    fluidRow(column(6, radioButtons(inputId = "x_axis", label = "x axis", choices = "PC1")),
             column(6, radioButtons(inputId = "y_axis", label = "y axis", choices = "PC2"))
    ),
    hr(), # horizontal line
    conditionalPanel(condition = "output.block_num == 1",
                     numericInput("blk1_ans", "How many clusters exist within the data?",
                                  value = 0, min = 0, max = 10)
    ),
    conditionalPanel(condition = "output.block_num == 2",
                     div(style = 'width:400px;',
                         div(style = 'float:left; color:red; font-size:14px', 
                             strong('most important')),
                         div(style = 'float:right; color:red; font-size:14px', 
                             strong('least important'))),
                     tags$br(),
                     uiOutput("blk2Inputs")
    ),
    conditionalPanel(condition = "output.block_num == 3",
                     uiOutput("blk3Inputs")
    ),
  ), # close sidebarPanel
  mainPanel(
    ### _Intro mainPanel -- TEST_UI
    conditionalPanel(
      condition = "output.ui_section == 'intro'",
      h3("Welcome do the study.")
      , br()
      , p("This a completely voluntary study that will take approximately 25-30 
      minutes to complete. If at any point you would like to stop, please let 
          the proctor know.")
      , br()
      , p("You are helping to compare the effectiveness of different visuals of 
          linear projection for multi-variate data. You will be assigned to one 
          graphic, go through a training period, experiment through 3 diffent 
          tasks and fill a short follow-up survey. 
          The outline of the study is as follows:")
      , tags$b("Training")
      , tags$ul(
        tags$li("Video training")
        , tags$li("Graphic and ui familiarity -- questions encouraged")
      )
      , tags$b("Expiriment -- 2 minutes per task, no questions")
      , tags$ul(
        tags$li("Task one (3 reps) -- How many clusters are contained within the data?")
        , tags$li("Task two (3 reps) -- Rank the most important variables distinguishing groups")
        , tags$li("Task three (3 reps) -- Group correlated variables (if any)")
      )
      , tags$b("Follow-up")
      , tags$ul(
        tags$li("Short questionnaire")
        , tags$li("Response submission")
      )
      , p("Make sure to provide your email address and computer to the proctor
          if you wish to be entered in the prize pool for top three scoring 
          participants will receive a $50 gift card to Coles. Also, mark if you 
          want to be emailed about the subsequent publication.")
      , p("Thank you again for participating.")
    ), # close conditionPanel -- intro
    conditionalPanel( # NOTE: This could be done in top_text or similar, which ever is easyier atm.
      condition = "output.ui_section == 'training' ",
      h1("training"),
      h1("TRAINING CONTENT HERE.")
      , h4(training_bottom_timer_text)
    ),
    conditionalPanel( 
      condition = "output.ui_section == 'task' ",
      h1("task")
       , textOutput('timer_disp')
    ),
    conditionalPanel( # bottom half of training and task sections
      condition = "output.ui_section == 'training' || output.ui_section == 'task'"
      , plotOutput("task_pca", height = "auto")
      , verbatimTextOutput("question_text")
      , verbatimTextOutput("response_msg")
    ),
    conditionalPanel(
      condition = "output.ui_section == 'survey' ",
      h1("survey")
    )
  ) # close mainPanel()
) # close TEST_ui_section assignment
##### UI, combine panels -----
ui <- fluidPage(
  titlePanel("Multivariate data visualization study")
  #, TEST_ui_section
  , main_ui
  , actionButton("next_pg_button", "Next page")
  , verbatimTextOutput("dev_msg")
  , tableOutput("ans_tbl")
)


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

## REMOVE?
# l_choices <- list(NULL)
# for (i in 1:5){ 
#   l_choices[[i]] <- i
# }
# names(l_choices) <- paste0("choice ", 1:5)

##### Main tabPanel -----
main_panel <- fluidPage(
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
      h3("Thank you for participating!")
      , br()
      , p("This a completely voluntary study that will take approximately 25-30 
      minutes to complete.")
      , br()
      , p("You are helping to compare the effectiveness of different visuals of 
       linear projection for multi-variate data. Each participant will be 
       assigned to one visual, they will then watch a short video demonstrating 
       how to perform the 3 different tasks with their visuals. Participants 
       are then able to interact with the visual, and ask any final clarifying 
       questions before starting the evaluated section. During this section, 3 
       repetitions 
       of the 3 differing tasks will be recorded. Each repetition will be capped 
       2 minutes before being asked to proceed to the next question. After the 
       evaluation section there is a 10-question follow-up asking about 
       demographic information, the assigned visual, and your familiarity with
       multi-variate data. The outline of the study is as follows:")
      , tags$ul(
        tags$li("Video training")
        , tags$li("Visual and ui familiarity -- questions allowed")
        , tags$li("Task one (3 reps) -- How many clusters are contained within the data?")
        , tags$li("Task two (3 reps) -- Rank the most important variables distinguishing groups")
        , tags$li("Task three (3 reps) -- Group correlated variables (if any)")
        , tags$li("Follow-up questionnaire")
        , tags$li("Response submission")
      )
      , p("Make sure to provide your email address and computer to the proctor
   if you wish to be entered in the prize pool for top three scoring 
   participants will receive a $50 gift card to Coles. Also, mark if you want to
   be emailed about the subsequent publication.")
      , p("Thank you again for participating.")
    ),
    ### _Training mainPanel -----
    conditionalPanel(
      condition = "output.ui_section == 'training'",
      h1("TRAINING CONTENT HERE.") ##TODO
      , plotOutput("task_pca", height = "auto")
      , verbatimTextOutput("question_text")
      , verbatimTextOutput("response_msg")
      , h4(training_bottom_timer_text)
    ),
    ### _Task mainPanel -----
    conditionalPanel(
      condition = "output.ui_section == 'task'",
      h1("TASK CONTENT HERE.") ##TODO 
      , textOutput('timer_disp')
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
  conditionalPanel(
    condition = "output.ui_section == 'intro' ",
    h1("intro"), # Working alone.
    h3("Thank you for participating!")
    , br()
    , p("This a completely voluntary study that will take approximately 25-30 
      minutes to complete.")
    , br()
    , p("You are helping to compare the effectiveness of different visuals of 
       linear projection for multi-variate data. Each participant will be 
       assigned to one visual, they will then watch a short video demonstrating 
       how to perform the 3 different tasks with their visuals. Participants 
       are then able to interact with the visual, and ask any final clarifying 
       questions before starting the evaluated section. During this section, 3 
       repetitions 
       of the 3 differing tasks will be recorded. Each repetition will be capped 
       2 minutes before being asked to proceed to the next question. After the 
       evaluation section there is a 10-question follow-up asking about 
       demographic information, the assigned visual, and your familiarity with
       multi-variate data. The outline of the study is as follows:")
    , tags$ul(
      tags$li("Video training")
      , tags$li("Visual and ui familiarity -- questions allowed")
      , tags$li("Task one (3 reps) -- How many clusters are contained within the data?")
      , tags$li("Task two (3 reps) -- Rank the most important variables distinguishing groups")
      , tags$li("Task three (3 reps) -- Group correlated variables (if any)")
      , tags$li("Follow-up questionnaire")
      , tags$li("Response submission")
    )
    , p("Make sure to provide your email address and computer to the proctor
   if you wish to be entered in the prize pool for top three scoring 
   participants will receive a $50 gift card to Coles. Also, mark if you want to
   be emailed about the subsequent publication.")
    , p("Thank you again for participating.")
  ),
  conditionalPanel(
    condition = "output.ui_section == 'training' ",
    h1("training"), # working except for task_pca :/
    h1("TRAINING CONTENT HERE.") ##TODO
    , plotOutput("task_pca", height = "auto")
    , verbatimTextOutput("question_text")
    , verbatimTextOutput("response_msg")
    , h4(training_bottom_timer_text)
  ),
  conditionalPanel(
    condition = "output.ui_section == 'task' ",
    h1("task")
  ),
  conditionalPanel(
    condition = "output.ui_section == 'survey' ",
    h1("survey")
  )
)
  
##### UI, combine panels -----
ui <- fluidPage(
  titlePanel("Multivariate data visualization study")
  , TEST_ui_section
  #, main_panel
  , actionButton("next_task_button", "Next task")
  , verbatimTextOutput("dev_msg")
  , tableOutput("ans_tbl")
)


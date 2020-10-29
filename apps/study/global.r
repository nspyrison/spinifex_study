### Global initalize for each factor variant.

##### global.r, spinifex_study -----

### Setup -----
library("ggplot2")
library("spinifex")
library("shiny")
library("tidyr")
library("dplyr")
library("plotly")
library("GGally")
library("shinyjs")   ## help with handling conditionalPanels.
library("lubridate") ## For timer
library("loggit")    ## For logging
library("here")      ## Fixing base dir.
set.seed(20200927)   ## if tourr starts using seeds

#### Simulated data series,
## "series" or iteration of data to look at. Should be an even hundred
height_px <- 500L
pal <- RColorBrewer::brewer.pal(8, "Dark2")[c(1, 2, 3, 6, 8)]
#scale_colour_manual(values = pal)

bas_p4 <- matrix(c(.5, .5,
                   -.5, .5,
                   -.5, -.5,
                   .5, -.5),
                 ncol = 2, nrow = 4, byrow = TRUE)
bas_p6 <- matrix(c(.2887, .5,
                   -.2887, .5,
                   -.5774, 0,
                   -.2887, -.5,
                   .2887, -.5,
                   .5774, 0),
                 ncol = 2, nrow = 6, byrow = TRUE)

## Loads plotting functions and preloaders (WIP), run after setting pal and bas_p6.
source(here::here("apps/study/preload_ggplots.r"))



#### Logging -----
## browseURL("https://www.r-bloggers.com/adding-logging-to-a-shiny-app-with-loggit/")
## use: loggit("INFO", "<main msg>", "<detail>")
## SET do_log to TRUE to start logging 
do_log            <- FALSE
do_disp_dev_tools <- TRUE
do_browse_err     <- FALSE
if(do_browse_err == TRUE)
  options(error = browser)
cat("do_log:", do_log)

#### Set log file, participant_num is the next number after reading last writen participant_num
## TODO: need to read the unique perm numbers in load file, and find the perm_numbers in the min count table. 
## Initialize
participant_num <- 1
log_file <- "inialize"
if(do_log == TRUE){
  full_perm_num <- 1 + participant_num %% 56
  log_file <- paste0("log_participant_", participant_num, ".json")
  ## TODO: Need to get participant_num, from google docs.
  while (file.exists(log_file)){ ## Find an unused log number
    participant_num <- participant_num + 1
    full_perm_num <- 1 + participant_num %% 56
    log_file <- paste0("log_participant_", participant_num, ".json")
  }
  set_logfile(log_file)
}else{ ## When do_log == F
  participant_num  <- sample(1:999, 1)
  full_perm_num <- 1 + participant_num %% 56
  log_file <- paste0("log_participant_", participant_num,
                     "Logging is off! Log and responses not being recorded.")
}
full_perm_num <- 1 + participant_num %% 56
cat("do_log, log_file: ", do_log, log_file, " /n")

#### Select factor and block permutations
## The permutation number
this_factor_perm   <- 1 + (full_perm_num - 1) %% 6 ## %% is mod
this_location_perm <- 1 #1 + floor((full_perm_num - 1) / 3) %% 3
this_vc_perm       <- 1 #1 + floor((full_perm_num - 1) / 9) %% 6
## The permutations
factor_perms   <- rbind(c(1, 1,  2, 2,  3, 3), ## The 3 permutations of the 3 factor orders
                        c(1, 1,  3, 3,  2, 2),
                        c(2, 2,  3, 3,  1, 1),
                        c(2, 2,  1, 1,  3, 3),
                        c(3, 3,  1, 1,  2, 2),
                        c(3, 3,  2, 2,  1, 1))
location_perms <- rbind(c(1, 2,  3, 1,  2, 3))
vc_perms       <- rbind(c(1, 1,  2, 2,  3, 3))
  # rbind(c(1, 2), ## The 6 permutations of the 3 location orders
  #                       c(1, 3),
  #                       c(2, 3),
  #                       c(2, 1),
  #                       c(3, 1),
  #                       c(3, 2))
## set factor and block names
factor_nms   <- c("pca", "grand", "radial")
location_nms <- c("0_1", "33_66", "50_50")
vc_nms       <- c("EEE", "EEV", "banana")
p_dim_nms <- this_p_dim_nm_ord <- c("p4", "p6")
## The decoded names 
this_factor_nm_ord <- 
  factor_nms[factor_perms[this_factor_perm, ]]
this_location <- 
  location_nms[location_perms[this_location_perm, ]]
this_vc_nm_ord <- 
  vc_nms[vc_perms[this_vc_perm, ]]


## Context, "onStart()" and onStop()
context_line <- paste0("Spinifex STUDY, --- (spinifex v", packageVersion("spinifex"),
                       ") --- Started ", Sys.time())
## This won't work on shiny app.
# this_Sys.info <- paste(Sys.info()[1:5], collapse = ", ")
## dummy onStart(function(){})
context_msg <- paste(sep = " \n",
                     context_line,
                     paste0("Log file: ", log_file),
                     paste0("Participant number: ", participant_num, ".")
)
if(do_log == TRUE) loggit("INFO", "=====Spinifex study app start.=====")
cat(context_msg)
## onStop()
onStop(function(){
  cat(context_msg)
  if(do_log == TRUE){
    loggit("INFO", "=====Spinifex study app stop.=====")
    set_logfile(logfile = NULL, confirm = TRUE)
  }
  ## Try to autosave if not saved and do_log == T?
  #### note that rv$resp_tbl is out of scope to the global file.
})
## Survey questions; n = 21 = 9 + 12
survey_questions <- c("Which sex are you?",
                        "Which age group do you belong to?",
                        "What is your English proficiency?",
                        "What is your highest completed education?",
                        "I am experienced with data visualization.",
                        "I am experienced with tabular data.",
                        "I am experienced with clustering classification techniques.",
                        "I am experienced with multivariate statistical analysis.",
                        "I am experienced with machine learning.",
                        rep(c("I was already familiar with this visualization.",
                              "I found this visualization easy to use.",
                              "I felt confident in my answers with this visualization.",
                              "I liked using this visualization."), 3)
)

#### Load data and tour paths -----
this_sim_nms <- paste(rep(this_vc_nm_ord, 3), rep(p_dim_nms, 3), rep(this_location, 3), sep = "_")
root <- ("~/R/spinifex_study/apps/data") # here("apps/data/") ## Filepaths cannot be too long....

## Load all sim names, for dev control
sim_nms <- c("EEE_p4_0_1",    "EEE_p4_33_66",    "EEE_p4_50_50",
             "EEV_p4_0_1",    "EEV_p4_33_66",    "EEV_p4_50_50",
             "banana_p4_0_1", "banana_p4_33_66", "banana_p4_50_50",
             "EEE_p6_0_1",    "EEE_p6_33_66",    "EEE_p6_50_50",
             "EEV_p6_0_1",    "EEV_p6_33_66",    "EEV_p6_50_50",
             "banana_p6_0_1", "banana_p6_33_66", "banana_p6_50_50")
sim_nms <- c(paste0("EEE_p4_0_1_t", 1:3), ## 3 training sets
             as.vector(outer(sim_nms, paste0("_rep", 1:3), FUN = "paste0"))) ## 
sim_fps <- paste0(root, "/", sim_nms, ".rda")
for(i in 1:length(sim_nms)){
  load(sim_fps[i])
}
## Load the few tpaths.
tpath_nms <- paste0("tpath_", c("p4_t", "p4", "p6"))
tpath_fps <- paste0(root, "/", tpath_nms, ".rda")
for(i in 1:length(tpath_nms)){
  load(tpath_fps[i])
}

# ## Load just needed files
# sim_fps <- paste0(root, "/", this_sim_nms, ".rda")
# tpath_fps <- paste0(root, "/tpath_", this_sim_nms, ".rda")
# for(i in 1:length(this_sim_nms)){
#   ## Load sims by the obj name stored in .rda files.
#   load(sim_fps[i])
#   load(tpath_fps[i])
# }


#TODO: is this the best spot for it?
## Enumerate and eagerly eval all ggplots



##### Global variable initialization -----
n_trainings        <- 2L #length(s_t_dat)      ## ~2
n_factors          <- length(factor_nms)       ## ~3
n_p_dim            <- length(p_dim_nms)        ## ~2
n_survey_questions <- length(survey_questions) ## ~21
PC_cap             <- 4L ## Number of principal components to choose from.

#### Define section start pages,
intro_pgs   <- 1L:3L   ## Structure, video, splash screen
period1_pgs <- 4L:7L   ## Training, rep 1, rep 2, and intermission
period2_pgs <- 8L:11L  ## Training, rep 1, rep 2, and intermission
period3_pgs <- 12L:14L ## Training, rep 1, rep 2
survey_pg   <- 15L     ## Survey

##### UI START -----
### header_ui -----
header_ui <- fluidPage(
  titlePanel("Multivariate data visualization study"),
  actionButton("next_pg_button", "Next page")
)

##### sidebar_ui ----
sidebar_ui <- conditionalPanel(
  condition = "output.eval == 'training'",
  sidebarPanel(width = 3L,
               ## TODO ENABLE THESE REMOVED TRAINING TEXT DISP AFTER PLANNING
               #,
               ##### _Training text -----
               # conditionalPanel(
               #   condition = "output.section_nm == 'training'",
               #   conditionalPanel( ## interface familiarity
               #     condition = "output.section_pg == 1",
               #     p("In this study, you will be working with 3 visualization techniques of
               #     multivariate data. Each one uses 2-dimensional projections created
               #     from different combinations of variables. The variable map (grey circle)
               #     shows the angle and magnitude that each variable contributes to the
               #     projection."),
               #     p("Principal Component Analysis (PCA) is displayed first. Use the radio
               #       buttons on the left sidebar panel to select new components to be
               #       displayed. Observe how the clusters and the variable contributions
               #       change."),
               #     p("Now switch to the grand tour factor. Play the animation. Notice how
               #       different clusters move as the variable contributions change. Drag
               #       the slider to select a different frame or animate at your own pace."),
               #     p("Change to the radial tour. You can select which components are on
               #       the axes. Using the drop-down, select the variable with the largest
               #       line segment. Use the slider to change the variable's contribution.
               #       Watch how the contributions and clusters move as a result. Select a
               #       change the y-axis to PC3 and back, notice that this resets the
               #       projection."),
               #   ), ## Close coditionalPanel()
               #   conditionalPanel( ## Rraining task 1, pg 2
               #     condition = "output.task == 2",
               #     strong("The data points are colored by their cluster again.
               #            Variables that have a large
               #            contribution in line with two clusters are important to
               #            distinguish them. However, you cannot rule out that variables
               #            with a small contribution are unimportant.
               #            Use this information to identify which variables distinguish
               #            the 2 clusters.")
               #   ), ## Close coditionalPanel()
               #   hr()
               # ), ### Close training text coditionalPanel()
  
  ##### _Training control inputs -----
  ## Factor selection
    conditionalPanel(
      condition = "output.section_nm == 'training' && output.section_pg < 6",
      radioButtons(inputId = "factor", label = "Factor",
                   choices = factor_nms,
                   selected = factor_nms[1L],
                   inline = TRUE),
      fluidRow(column(4, radioButtons(inputId = "simVc", label = "VC",
                                      choices = c("EEE", "EEV", "banana"), selected = "EEE")),
               column(4, radioButtons(inputId = "simP", label = "# Clusters & var",
                                      choices = list("3cl in 4v" = "p4",
                                                     "4cl in 6v" = "p6"), 
                                      selected = "p4")),
               column(4, radioButtons(inputId = "simLocation", label = "Location (1sig, 1noise)",
                                      choices = list("0%/100%" = "0_1",
                                                     "33%/66%" = "33_66", 
                                                     "50%/50%" = "50_50"), 
                                      selected = "0_1"))
      )
    ), 
  
  ##### _Task response input -----
    ## Task 2
    conditionalPanel(
      condition = "(output.any_active == true) ", 
      checkboxGroupInput(
        inputId = "task_response",
        label   = "Check any/all variables contribute more than average to the cluster seperation green circles and orange triangles.",
        choices = "V1",
        inline  = TRUE
      )
    )
  ) ## Close conditionalPanel(), assigning sidebar_ui
)
  

##### Initialize survey columns -----
surv_lab <- HTML("<div style=\"width:300px;\">
                    <div style=\"float:left;\">strongly disagree</div>
                    <div style=\"float:right;\">strongly agree</div>
                  </div>")
survey_fct_q_start <- 9L
col_p1 <- column(4L,
                 h3(this_factor_nm_ord[1]),
                 hr(),
                 h4(survey_questions[survey_fct_q_start + 1]),
                 sliderInput(paste0("survey", survey_fct_q_start + 1),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(survey_questions[survey_fct_q_start + 2]),
                 sliderInput(paste0("survey", survey_fct_q_start + 2),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(survey_questions[survey_fct_q_start + 3]),
                 sliderInput(paste0("survey", survey_fct_q_start + 3),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(survey_questions[survey_fct_q_start + 4]),
                 sliderInput(paste0("survey", survey_fct_q_start + 4),
                             label = surv_lab, min = 1, max = 9, value = 5)
)

col_p2 <- column(4,
                 h3(this_factor_nm_ord[2]),
                 hr(),
                 h4(survey_questions[survey_fct_q_start + 5]),
                 sliderInput(paste0("survey", survey_fct_q_start + 5),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(survey_questions[survey_fct_q_start + 6]),
                 sliderInput(paste0("survey", survey_fct_q_start + 6),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(survey_questions[survey_fct_q_start + 7]),
                 sliderInput(paste0("survey", survey_fct_q_start + 7),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(survey_questions[survey_fct_q_start + 8]),
                 sliderInput(paste0("survey", survey_fct_q_start + 8),
                             label = surv_lab, min = 1, max = 9, value = 5)
)
col_p3 <- column(4,
                 h3(this_factor_nm_ord[3]),
                 hr(),
                 h4(survey_questions[survey_fct_q_start + 9]),
                 sliderInput(paste0("survey", survey_fct_q_start + 9),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(survey_questions[survey_fct_q_start + 10]),
                 sliderInput(paste0("survey", survey_fct_q_start + 10),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(survey_questions[survey_fct_q_start + 11]),
                 sliderInput(paste0("survey", survey_fct_q_start + 11),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(survey_questions[survey_fct_q_start + 12]),
                 sliderInput(paste0("survey", survey_fct_q_start + 12),
                             label = surv_lab, min = 1, max = 9, value = 5)
)

##### main_ui -----
main_ui <- mainPanel(width = 9,
  textOutput("timer_disp"),
  ### _Intro mainPanel -----
  conditionalPanel(
    condition = "output.section_nm == 'intro'",
    conditionalPanel(
      condition = "output.pg == 1", ## First page
      h3("Welcome to the study"),
      br(),
      p("This a completely voluntary study that will take approximately 45-50
          minutes to complete. If at any point you would like to stop,
          please let the invigilator know."),
      br(),
      p("You are helping to compare the effectiveness of different
          multivariate data visualization techniques.
          The study is structured as follows:"),
      p("Training -- questions encouraged"),
      HTML("<ul>
              <li>Video training: you will first watch a five minute video
                  explaining the techniques</li>
              <li>Interface familiarity: you will get to explore the interface
                    for the different tasks, answer questions about the data, and
                    receive feedback</li>
            </ul>"),
      p("Evaluation, for each of the 3 visuals -- independent effort with no questions"),
      HTML("<ul>
              <li>Cluster seperation task (x2 difficulties, 180 sec)</li>
            </ul>"),
      p("Wrap up study"),
      HTML("<ul>
              <li>Complete survey</li>
              <li>Save and exit from app</li>
            </ul>"),
      p("We really appreciate your participation in this study.")
    ), ## End first page
    conditionalPanel(
      condition = "output.pg == 2", ## Video
      h2("Video training"), br(), br(),
      p("Watch the following video before proceeding:"), br(),
      ## Adding the 'a' tag to the sidebar linking external file
      p("Minimize the study and watch the training video."),
      a(href='training.mp4', target='blank', 'training video (4:17)'),
      br(), br(),
      p("If this link only contains audio let the invigilator know.")
    ) ## End of video
  ), ## Close conditionalPanel -- intro section text
  
  ### _Training mainPanel -----
  conditionalPanel(
    condition = "output.section_nm == 'training'",
    conditionalPanel(condition = "output.section_pg == 1", ## ui intro
                     h2("Training -- interface")
    ),
    conditionalPanel(condition = "output.section_pg == 2",
                     h2("Training -- cluster seperation task")
    ),
    conditionalPanel(condition = "output.section_pg == 3",
                     h2("Training -- cluster seperation task, set 2")
    ),
    conditionalPanel( ## splash page
      condition = "output.section_pg == 4",
      h1(), h1(), h1(),
      h1("Training complete, Great job!"),
      h4("Take a break and strech if you feel like it."),
      HTML("<h3><span style='color:red'>
          Keep in mind that we are evaluating the factors, not you.
          Don't worry if you don't fully understand a visualization or find the task difficult.
           </span></h3>"),
      h4("Ask any final clarification questions. Then continue on to the
        evaluation section. The task is timed, with a time remaining displayed on top.")
    ),
    hr()
  ), ## close training section main panel text
  
  ### _Task mainPanel -----
  conditionalPanel(
    condition = "output.section_nm == 'task'",
    h2(textOutput('task_header')),
    hr()
  ), ## close task section conditional panel title text
  
  ### _Plot mainPanel ----
  conditionalPanel(
    condition = "(output.eval == 'training'", 
    htmlOutput("plot_msg"),
    plotOutput("pca_plot", height = "100%"),
    ## PCA axis selection
    conditionalPanel(
      condition = "output.factor_nm == 'pca'",
      fluidRow(radioButtons(inputId = "x_axis", label = "x axis",
                            choices = paste0("PC", 1:PC_cap),
                            selected =  "PC1", inline = TRUE),
               radioButtons(inputId = "y_axis", label = "y axis",
                            choices = paste0("PC", 1:PC_cap),
                            selected =  "PC2", inline = TRUE)
      )
    ),
    plotOutput("radial_plot", height = "100%"),
    ## Radial manip var radio buttons
    conditionalPanel(
      condition = "output.factor_nm == 'radial'",
      radioButtons(inputId = "manip_var_nm", label = "Manip variable:",
                   choices =  "V1", selected = "V1")
    ), ## Close conditionalPanel()
    uiOutput("radial_slider"),
    uiOutput("grand_ui")
  ), ## Close plot conditional panel
  
  ### _Survey mainPanel -----
  conditionalPanel(
    condition = "output.section_nm == 'survey'",
    conditionalPanel(
      condition = "output.is_saved == 0",
      selectInput("survey1", label = survey_questions[1],
                  choices = c("decline to answer", "female", "male",
                              "intersex, non-binary, or other")
      ),
      selectInput("survey2", label = survey_questions[2],
                  choices = c("decline to answer", "19 or younger", "20 to 29",
                              "30 to 39", "40 or older")
      ),
      selectInput("survey3", label = survey_questions[3],
                  choices = c("decline to answer", "fluent",
                              "conversational", "less than conversational")
      ),
      selectInput("survey4", label = survey_questions[4],
                  choices = c("decline to answer", "high school",
                              "undergraduate", "honors, masters, mba", "doctorate")
      ),
      h3("To what extent do you agree with the following statements?"),
      strong(survey_questions[5]),
      sliderInput("survey5", label = surv_lab,
                  min = 1, max = 9, value = 5),
      strong(survey_questions[6]),
      sliderInput("survey6",label = surv_lab,
                  min = 1, max = 9, value = 5),
      strong(survey_questions[7]),
      sliderInput("survey7",label = surv_lab,
                  min = 1, max = 9, value = 5),
      strong(survey_questions[8]),
      sliderInput("survey8",label = surv_lab,
                  min = 1, max = 9, value = 5),
      strong(survey_questions[9]),
      sliderInput("survey9",label = surv_lab,
                  min = 1, max = 9, value = 5),
      fluidRow(col_p1, col_p2, col_p3),
      hr(),
      actionButton("save_resp", "save responses")
    ),
    htmlOutput("save_msg"),
    conditionalPanel(
      condition = "output.is_saved == 1",
      h3("Thank you for participating!"),
      br(),
      h4("Let the invigilator know you have completed the study and have a good day.")
    )
  ) ## close survey condition panel
) ## close mainPanel() End of main_ui section.

### _dev_tools
dev_tools <- conditionalPanel(
  "output.dev_tools == true",
  p("===== Development display below ====="),
  actionButton("browser", "browser()"),
  p("Variable level diff from avg: "), textOutput("task_diff"),
  p("Variable level response: "), textOutput("task_resp"),
  p("Variable level score: "), textOutput("task_var_score"),
  p("Task score: "), textOutput("task_score"),
  ##
  textOutput("dev_msg"),
  tableOutput("resp_tbl")
) ## close conditionPanel, assigning dev_tools

##### UI, combine panels -----
ui <- fluidPage(useShinyjs(), ## Required in ui to use shinyjs.
                header_ui,
                sidebar_ui,
                main_ui,
                dev_tools
)

##### App local functions -----
app_html_red <- function(string){
  paste0("<strong><span style='color:red'>", string, "</span><strong>")
}


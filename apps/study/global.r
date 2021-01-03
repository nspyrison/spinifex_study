### Global initialize for each factor variant.

##### global.r, spinifex_study -----

### Setup -----
source('resp_tbl.r', local = TRUE) ## Needs initialization from global.r.
library("shiny")
library("googlesheets4") ## Google sheets (with api v4) for read/write responses.
library("shinyjs")   ## Help with handling conditionalPanels
library("lubridate") ## For timer
library("here")      ## Fixing base dir
do_disp_dev_tools <- TRUE ## Expects: TRUE / FALSE
options(shiny.autoreload = TRUE)
#options(error = browser) ## occasionally helpful for troubleshooting
#set.seed(20200927)   ## If tourr starts using seeds
time_alotted <- 180L ## Seconds for the task
height_px <- 500L
pal <- RColorBrewer::brewer.pal(8L, "Dark2")[c(1L, 2L, 3L, 6L, 8L)] ## Even more color safe
ss_id <- "1K9qkMVRkrNO0vufofQJKWIJUyTys_8uVtEBdJBL_DzU" ## the 'id' or name of the google sheet
## auth code from 23/12/2020, nitro lapop: 4/1AY0e-g5NhEF12mV_4U_d1MzO0GrnNZUlaCCNvq-lLTPJ0Ry8iubLXQJ9uCI
## Prolific.co to see the study draft page go to:
if(F)
  browseURL("https://app.prolific.co/studies/5fd808e32ce90812aeb9cd90")

## name                       id
## <chr>                      <chr>
## spinifex_study resp_tbl    1K9qkMVRkrNO0vufofQJKWIJUyTys_8uVtEBdJBL_DzU

bas_p4 <- matrix(c(.5,  .5,
                   -.5, .5,
                   -.5, -.5,
                   .5,  -.5),
                 ncol = 2, nrow = 4, byrow = TRUE)
bas_p6 <- matrix(c(.2887,  .5,
                   -.2887, .5,
                   -.5774, 0,
                   -.2887, -.5,
                   .2887,  -.5,
                   .5774,  0),
                 ncol = 2, nrow = 6, byrow = TRUE)



#### participant and perm_number -----
## Initialize
participant_num <- 1L
## TODO: need to read the unique perm numbers in load file, and find the perm_numbers in the min count table. 
if(F){ ## Read response sheet and set participant number
  prev_saves <- read_sheet(ss_id)
  participant_num <- length(unique(prev_saves$participant_num)) + 1
}else{ ## ELSE assign random participant and perm numbers.
  participant_num <- sample(1L:999L, 1L)
}
full_perm_num <- 1 + participant_num %% 56L

#### Select factor and block permutations
## Possible permutations
factor_perms   <- rbind(c(1L, 1L,  2L, 2L,  3L, 3L),
                        c(1L, 1L,  3L, 3L,  2L, 2L),
                        c(2L, 2L,  3L, 3L,  1L, 1L),
                        c(2L, 2L,  1L, 1L,  3L, 3L),
                        c(3L, 3L,  1L, 1L,  2L, 2L),
                        c(3L, 3L,  2L, 2L,  1L, 1L))
location_perms <- rbind(c(1L, 1L,  2L, 2L,  3L, 3L),
                        c(1L, 1L,  3L, 3L,  2L, 2L),
                        c(2L, 2L,  3L, 3L,  1L, 1L),
                        c(2L, 2L,  1L, 1L,  3L, 3L),
                        c(3L, 3L,  1L, 1L,  2L, 2L),
                        c(3L, 3L,  2L, 2L,  1L, 1L))
vc_perms       <- rbind(c(1L, 1L,  2L, 2L,  3L, 3L))
## set factor and block names
factor_nms   <- c("pca", "grand", "radial")
location_nms <- c("0_1", "33_66", "50_50")
vc_nms       <- c("EEE", "EEV", "banana")
p_dim_nms <- this_p_dim_nm_ord <- c("p4", "p6")
## The permutation numbers
r_fct <- nrow(factor_perms)     ##~6
r_loc <- nrow(location_perms)   ##~6
r_vc  <- nrow(vc_perms)         ##~1
r_perms <- r_fct * r_loc * r_vc ##~36
this_factor_perm   <- 1L + (full_perm_num - 1L) %% r_fct
this_location_perm <- 1L #1 + floor((full_perm_num - 1) / r_fct) %% r_loc
this_vc_perm       <- 1L #1 + floor((full_perm_num - 1) / (r_fct * r_Loc)) %% r_vc
## The decoded names
this_factor_nm_ord <-
  factor_nms[factor_perms[this_factor_perm, ]]
this_vc_nm_ord <-
  vc_nms[vc_perms[this_vc_perm, ]]
this_location_nm_ord <-
  location_nms[location_perms[this_location_perm, ]]



init_resp_tbl   <- make_resp_tbl(participant_num)   ## func from ./resp_tbl.r
init_survey_tbl <- make_survey_tbl(participant_num) ## func from ./resp_tbl.r


## Context, "onStart()" and onStop()
context_line <- paste0("Spinifex STUDY, --- (spinifex v", packageVersion("spinifex"),
                       ") --- Started ", Sys.time())
message("ran onStart() code.")
## This won't work on shiny app.
# this_Sys.info <- paste(Sys.info()[1:5], collapse = ", ")
## dummy onStart(function(){})
context_msg <- paste(sep = " \n",
                     context_line,
                     paste0("Participant number: ", participant_num, "."),
                     paste0("Perm number: ", full_perm_num, ".")
)
cat(context_msg)

## Survey questions; n = 21 = 9 + 12
survey_questions <- c("Which sex are you?",
                      "Which age group do you belong to?",
                      "What is your English proficiency?",
                      "What is your highest completed education?",
                      "You are experienced with data visualization.",
                      "You are experienced with cleaning data.",
                      rep(c("I was already familiar with this visualization.",
                            "I found this visualization easy to use.",
                            "I felt confident in my answers with this visualization.",
                            "I liked using this visualization."), 3)
)
init_survey_tbl$question = survey_questions


#### Load data and tour paths -----
root <- ("./www/data/") ## Local 

this_sim_nms <- paste(rep(this_vc_nm_ord, 3L), rep(p_dim_nms, 3L), rep(this_location_nm_ord, 3L), sep = "_")
this_sim_nms <- c(paste0("EEE_p4_0_1_t", 1L:3L), 
                  as.vector(outer(this_sim_nms, paste0("_rep", 1L:3L), FUN = "paste0")) ## cross product paste
) 


# ## Load all sim names, for dev control
# sim_nms <- c("EEE_p4_0_1",    "EEE_p4_33_66",    "EEE_p4_50_50",
#              "EEV_p4_0_1",    "EEV_p4_33_66",    "EEV_p4_50_50",
#              "banana_p4_0_1", "banana_p4_33_66", "banana_p4_50_50",
#              "EEE_p6_0_1",    "EEE_p6_33_66",    "EEE_p6_50_50",
#              "EEV_p6_0_1",    "EEV_p6_33_66",    "EEV_p6_50_50",
#              "banana_p6_0_1", "banana_p6_33_66", "banana_p6_50_50")
# sim_nms <- c(paste0("EEE_p4_0_1_t", 1:3), ## 3 training sets
#              as.vector(outer(sim_nms, paste0("_rep", 1:3), FUN = "paste0"))) ## cross product paste

sapply(this_sim_nms, function(i){
  this_fq_fp_sim_nm <-paste0(root, i, ".rda")
  load(this_fq_fp_sim_nm, envir = globalenv())
  })




##### Global variable initialization -----
n_factors          <- length(factor_nms)       ## ~3
n_p_dim            <- length(p_dim_nms)        ## ~2
n_survey_questions <- length(survey_questions) ## ~18 = 6 + 3 * 4
PC_cap             <- 4L ## Number of principal components to choose from.

#### Define section start pages,
intro_pgs   <- 1L:3L   ## Structure, video, splash screen
period1_pgs <- 4L:7L   ## Training, rep 1, rep 2, and intermission
period2_pgs <- 8L:11L  ## Training, rep 1, rep 2, and intermission
period3_pgs <- 12L:14L ## Training, rep 1, rep 2
survey_pg   <- 15L     ## Survey


##### UI START -----

### intro_page1 -----
intro_page1 <- conditionalPanel( ## First page conditionalPanel
  condition = "output.pg == 1",
  h3("Welcome to the study"),
  br(),
  p("This a completely voluntary study that will take approximately 15-20
    minutes to complete. You may quit the study an any time by closing the 
    browser. All data stored (including) partial is not identified."),
  br(),
  p("You will be helping to compare and contrast between 3 different forms of 
    viewing multivariate data. The structure of the study is:"),
  p(),
  p("Introduction"),
  tags$ul(
    tags$li("This study overview"),
    tags$li("Explanatory video (n min)")
  ),
  p("Period 1"),
  tags$ul(
    tags$li("Practice task"),
    tags$li("Task, 2x (60 sec each)")
  ),
  p("Period 2"),
  tags$ul(
    tags$li("Practice task"),
    tags$li("Task, 2x (60 sec each)")
  ),
  p("Period 3"),
  tags$ul(
    tags$li("Practice task"),
    tags$li("Task, 2x (60 sec each)")
  ),
  p("Wrap up"),
  tags$ul(
    tags$li("Survey"),
    tags$li("Save and exit")
  ),
  p("Before we get started, Prolific.co participants must enter their Prolific ID below"),
  textInput(inputId = "prolific_id", 
            label = "If you are a Prolific.co participants, enter your Prolific ID (otherwise blank)",
            placeholder = "<Prolific ID or blank>"),
  p("We really appreciate your participation in this study.")
) ## End of first page conditionalPanel, assigning intro_page1

### intro_page2 -----
intro_page2 <- conditionalPanel(
  condition = "output.pg == 2", 
  h2("Video training"), tags$br(), tags$br(),
  p("Watch the following video before proceeding:"), tags$br(), 
  # Adding the 'a' tag to the sidebar linking external file
  p("Minimize the study and watch the training video."),
  #tags$a(href='training.mp4', target='blank', 'training video (4:17)'), 
  tags$br(), tags$br(), 
  p("If this link only contains audio let the invigilator know.")
)  ## End of conditionalPanel, assigning intro_page2

### intro_page3 -----
intro_page3 <- conditionalPanel(
  condition = "output.pg == 3", 
  h2("<Intro Intermission>"), tags$br(), tags$br()
)  ## End of conditionalPanel, assigning intro_page3

## training_page -----
training_page <- conditionalPanel(
  condition = "output.section_nm == 'training'",
  conditionalPanel(condition = "output.section_pg == 1", 
                   h2("Training -- interface")),
  conditionalPanel(condition = "output.section_pg == 2",
                   h2("Training -- task 1")),
  conditionalPanel(condition = "output.section_pg == 3",
                   h2("Training -- task 1, set 2")),
  conditionalPanel(condition = "output.section_pg == 4",
                   h2("Training -- task 2")),
  conditionalPanel(condition = "output.section_pg == 5",
                   h2("Training -- task 2, set 2")),
  conditionalPanel( ##TODO: double check, splash page spacing. ## splash page 
    condition = "output.section_pg == 6",
    h1(), h1(), h1(),
    h1("Training complete, Great job!"),
    h4("Take a break and strech if you feel like it."),
    HTML("<h3><span style='color:red'>
          Keep in mind that we are evaluating the factors, not your performance. 
          Don't worry if you don't fully understand the theory or find a task difficult.
           </span></h3>"),
    h4("Ask any final clarification questions. Then continue on to the 
        evaluation section. Task 1 is limited to 1 minute, and task 2 is limited
        to 3 minutes (time displayed on top).")
  ),
  textOutput('stopwatch_disp'),
  hr()
) ## Close conditionalPanel, assigning training_page

### Initialize for survey -----
.surv_lab <-  div(style = 'width:300px;',
                  div(style = 'float:left;', '|<- disagree'),
                  div(style = 'float:right;', 'agree ->|'))
.surv_lab2 <-  div(style = 'width:220px;',
                  div(style = 'float:left;', '|<- disagree'),
                  div(style = 'float:right;', 'agree ->|'))
# .surv_lab <- p(style = "float: left;",  "strongly disagree",
#                span(style = "float: right;", "strongly agree"))
# .surv_lab <- HTML("<div>
#        <p style='text-align: right'>Text on the left.</p>
#        <p style='text-align: left'>Text on the right.</p>
#        </div>
#        <div style='clear: both;'></div>")
col_p1 <- column(4L,
                 h3(this_factor_nm_ord[1L]),
                 hr(),
                 h4(survey_questions[7L]),
                 sliderInput("survey7", label = .surv_lab2,
                             min = 1L, max = 9L, value = 5L, step = 1),
                 h4(survey_questions[8L]),
                 sliderInput("survey8", label = .surv_lab2,
                             min = 1L, max = 9L, value = 5L, step = 1),
                 h4(survey_questions[9L]),
                 sliderInput("survey9", label = .surv_lab2,
                             min = 1L, max = 9L, value = 5L, step = 1),
                 h4(survey_questions[10L]),
                 sliderInput("survey10",
                             label = .surv_lab2,
                             min = 1L, max = 9L, value = 5L, step = 1)
)

col_p2 <- column(4L,
                 h3(this_factor_nm_ord[3L]),
                 hr(),
                 h4(survey_questions[11L]),
                 sliderInput("survey11", label = .surv_lab2,
                             min = 1L, max = 9L, value = 5L, step = 1),
                 h4(survey_questions[12L]),
                 sliderInput("survey12", label = .surv_lab2,
                             min = 1L, max = 9L, value = 5L, step = 1),
                 h4(survey_questions[13L]),
                 sliderInput("survey13", label = .surv_lab2,
                             min = 1L, max = 9L, value = 5L, step = 1),
                 h4(survey_questions[14L]),
                 sliderInput("survey14", label = .surv_lab2,
                             min = 1L, max = 9L, value = 5L, step = 1),
)
col_p3 <- column(4L,
                 h3(this_factor_nm_ord[5L]),
                 hr(),
                 h4(survey_questions[15L]),
                 sliderInput("survey15", label = .surv_lab2,
                             min = 1L, max = 9L, value = 5L, step = 1),
                 h4(survey_questions[16L]),
                 sliderInput("survey16", label = .surv_lab2,
                             min = 1L, max = 9L, value = 5L, step = 1),
                 h4(survey_questions[17L]),
                 sliderInput("survey17", label = .surv_lab2,
                             min = 1L, max = 9L, value = 5L, step = 1),
                 h4(survey_questions[18L]),
                 sliderInput("survey18", label = .surv_lab2,
                             min = 1L, max = 9L, value = 5L, step = 1)
)

### survey_page -----
suvery_page <- conditionalPanel(
  condition = "output.section_nm == 'survey'",
  ## Before save button:
  conditionalPanel( 
    condition = "output.is_saved == false",
    selectInput("survey1", label = survey_questions[1L],
                choices = c("decline to answer",
                            "female",
                            "male",
                            "intersex/other")
    ),
    selectInput("survey2", label = survey_questions[2L],
                choices = c("decline to answer",
                            "19 or younger",
                            "20 to 29",
                            "30 to 39",
                            "40 or older")
    ),
    selectInput("survey3", label = survey_questions[3L],
                choices = c("decline to answer",
                            "English first language",
                            "English not first language")),
    selectInput("survey4", label = survey_questions[4L], 
                choices = c("decline to answer",
                            "High school",
                            "Undergraduate",
                            "Honors, masters, mba", 
                            "Doctorate")),
    h3("How much do you agree with the following statements?"),
    h4(survey_questions[5L]),
    sliderInput("survey5", label = .surv_lab,
                min = 1L, max = 9L, value = 5L, step = 1),
    h4(survey_questions[6L]),
    sliderInput("survey6",label = .surv_lab,
                min = 1L, max = 9L, value = 5L, step = 1),
    fluidRow(col_p1, col_p2, col_p3),
    hr(),
    actionButton("save_survey", "Save survey responses")
  ),
  ## After save button:
  conditionalPanel(
    condition = "output.is_saved == true",
    conditionalPanel(
      condition = "output.is_saved == 1",
      h3("Reponses saved. Thank you for participating!"),
      conditionalPanel("output.do_disp_prolific_code == true",
                       br(),
                       h3("Enter the completion code '18B6C620' to redeem payment.")
      )
    )
  )
) ## close condition panel, assigning survey_page


### header_page -----
header_page <- fluidPage(
  titlePanel("User study"),
  conditionalPanel(
    condition = "(output.pg != 15)",
    actionButton("next_pg_button", "Next page")
  )
)

##### sidebar_panel ----
sidebar_panel <- conditionalPanel(
  condition = "(output.plot_active == true)",
  sidebarPanel(
    checkboxGroupInput(
      inputId = "var_resp",
      label   = "Check any/all variables that contribute more than average to the cluster seperation green circles and orange triangles.",
      choices = "V1",
      inline  = TRUE)
  )
) ## Close fluidPage(), assigning sidebar_panel

p1_intermission <- conditionalPanel("output$p2_intermission == true",
                                    h2("<Intermission 1 text>"))
p2_intermission <- conditionalPanel("output$p2_intermission == true",
                                    h2("<Intermission 2 text>"))
  
  
##### main_page -----
pca_choices <- paste0("PC", 1L:PC_cap)
main_page <- mainPanel(
  width = 9L,
  intro_page1,
  intro_page2,
  intro_page3,
  training_page,
  suvery_page,
  ### Header, timer, and plot display
  conditionalPanel(
    condition = "output.plot_active == true",
    h2(textOutput('header')),
    hr(),
    textOutput("timer_disp"),
    hr(),
    ## Image text and image.
    verbatimTextOutput("image_fp"),
    imageOutput("image_plot"),
  ), ## close task section conditional panel title text
  
  ### _Plot mainPanel ----
  ## PCA axis selection
  conditionalPanel(
    condition = "output.factor == 'pca'",
    fluidRow(radioButtons(inputId = "x_axis", label = "x axis",
                          choices = pca_choices,
                          selected = "PC1", inline = TRUE),
             radioButtons(inputId = "y_axis", label = "y axis",
                          choices = pca_choices,
                          selected = "PC2", inline = TRUE)
    )
  ),
  ## Radial manip var radio buttons
  conditionalPanel(
    condition = "output.factor == 'radial'",
    radioButtons(inputId = "manip_var_nm", label = "Manip variable:",
                 choices =  "V1", selected = "V1")
  ), ## Close conditionalPanel(), plot interactions
  ## No input for grand tour.
) ## Close mainPanel() End of main_page section.





### _dev_tools
dev_tools <- conditionalPanel(
  "output.dev_tools == true",
  p("===== Development display below ====="),
  actionButton("browser", "browser()"),
  textOutput("dev_msg"),
  tableOutput("resp_row"),
  tableOutput("resp_tbl"),
  verbatimTextOutput("save_survey"),
  conditionalPanel(
    "output.plot_active == true",
    p("Variable level diff from avg: "), textOutput("diff"),
    p("Variable level response: "), textOutput("var_resp"),
    p("Variable level marks: "), textOutput("var_marks"),
    p("Task marks: "), textOutput("marks")
  )
) ## close conditionPanel, assigning dev_tools

##### ui, combined *_page pieces -----
ui <- fluidPage(useShinyjs(), ## Required in ui to use shinyjs.
                header_page,
                sidebarLayout(
                  sidebar_panel,
                  mainPanel(main_page)
                ),
                dev_tools
)

## App text formatting function (js wrapper for bold, red text)
text_boldred <- function(string){
  paste0("<strong><span style='color:red'>", string, "</span><strong>")
}

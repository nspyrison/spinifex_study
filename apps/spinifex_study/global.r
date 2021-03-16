### Global initialize for each factor variant.

##### global.r, spinifex_study -----

### Setup -----
source("resp_tbl.r", local = TRUE) ## Needs initialization from global.r.
source("solve_partcipant_num_from_gs4read.r", local = TRUE) ## Needs initialization from global.r.
require("shiny")
require("googlesheets4") ## Google sheets (with api v4) for read/write responses.
require("dplyr")
do_disp_dev_tools <- FALSE ## Expects: TRUE / FALSE
options(shiny.autoreload = TRUE) ## May reduce caching errors
#options(error = browser) ## occasionally helpful for troubleshooting
time_allotted <- 60L ## Seconds for the task
height_px <- 500L ## Default height [pixels] for plot
ss_id <- "1K9qkMVRkrNO0vufofQJKWIJUyTys_8uVtEBdJBL_DzU" ## Hash or name of the google sheet
## Google sheets id number:
## spinifex_study resp_tbl    1K9qkMVRkrNO0vufofQJKWIJUyTys_8uVtEBdJBL_DzU
#' @example 
#' browseURL("https://docs.google.com/spreadsheets/d/1K9qkMVRkrNO0vufofQJKWIJUyTys_8uVtEBdJBL_DzU/edit#gid=2008893390")

#### Prolific.co 
##to see the study draft page go to:
#' @example 
#' browseURL("https://app.prolific.co/studies/5fd808e32ce90812aeb9cd90")

#### Initialize factor and block permutations
## Possible permutations (by Evaluation number, not period number)
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
factor_nms      <- c("pca", "grand", "radial")
factor_descrip_1 <- c("not animated", "animated", "animated" )
factor_descrip_2 <- c("select 2 axes", "random path, no selection", "select variable to rotate")
factor_examp_fp <- paste0("images/", 
                          c("EEE_p4_0_1_t1__pca_x1y2.png", "EEE_p4_0_1_t1__grand.gif", "EEE_p4_0_1_t1__radial_mv2.gif"))
location_nms    <- c("0_1", "33_66", "50_50")
vc_nms          <- c("EEE", "EEV", "banana")
p_dim_nms       <- this_p_dim_nm_ord <- c("p4", "p6")
## The permutation counts
n_fct <- nrow(factor_perms)     ##~6
n_loc <- nrow(location_perms)   ##~6
n_vc  <- nrow(vc_perms)         ##~1
n_perms <- n_fct * n_loc * n_vc ##~36

#### Google authentication -----
#### Setup:
## https://gargle.r-lib.org/articles/non-interactive-auth.html#project-level-oauth-cache
## step 1) manually store token 
#' @example 
#' googlesheets4::gs4_auth(cache = "apps/study/.secrets") 
#' ## keep in mind it must get pushed with shiny deploy
## step 2) for down stream auth, use
#### step 2) option a):
# options(gargle_oauth_cache = ".secrets",
#         gargle_oauth_email = "nicholas.spyrison@monash.edu")
#### step 2) option b):
# googlesheets4::gs4_auth(
#   cache = ".secrets", email = "nicholas.spyrison@monash.edu")

## tryCatch for google api4 auth
was_auth_issue <- FALSE
tryCatch({
  googlesheets4::gs4_auth(
    cache = ".secrets", email = "nicholas.spyrison@monash.edu")
}, error = function(e){
  was_auth_issue <- TRUE
  txt <- "Could not authenticate to Google sheets. Please try again in 5 minutes. Closing app in 15 seconds."
  warning(txt)
  return(e)
})

#### Assign participant_num and perm_num -----
## Read response sheet and set participant number to first not use integer

## Initialize to an under evaled number, stays if API quota, otherwise overwriten.
participant_num <- NA_integer_
prev_saves <- NULL
was_quota_issue <- FALSE ## keep in mind API quota issue.
if(T){
  ## tryCatch for api quota limit
  tryCatch({
    prev_saves <- googlesheets4::read_sheet(ss_id, sheet = 1L, range = "B:S")
  }, error = function(e){
    was_quota_issue <- TRUE
    txt <- "Google API quota reached, Please try again in 2 minutes. Closing app in 15 seconds."
    stop(txt)
    return(e)
  })
  participant_num <- solve_partcipant_num_from_gs4read(prev_saves, n_perms = 36L)
}
full_perm_num <- 1L + (participant_num - 1L) %% n_perms ## n_perms ~36L

## Select permutation orders
this_factor_perm   <- 1L + (full_perm_num - 1L) %% n_fct
this_location_perm <- 1L + floor((full_perm_num - 1L) / n_fct) %% (n_fct * n_loc)
this_vc_perm       <- 1L # Fixed parameter #1L + floor((full_perm_num - 1L) / (n_fct * n_loc)) %% n_perms
## The decoded names
this_factor_nm_ord   <- factor_nms[factor_perms[this_factor_perm, ]]
this_vc_nm_ord       <- vc_nms[vc_perms[this_vc_perm, ]]
this_location_nm_ord <- location_nms[location_perms[this_location_perm, ]]
## Factor descriptions & filepaths
this_factor_descrip_1 <- factor_descrip_1[factor_perms[this_factor_perm, ]]
this_factor_descrip_2 <- factor_descrip_2[factor_perms[this_factor_perm, ]]
this_factor_examp_fp  <- factor_examp_fp[factor_perms[this_factor_perm, ]]

## Survey questions; n = 21 = 9 + 12
survey_questions <- c("What are your preferred pronouns?",
                      "Which age group do you belong to?",
                      "What is your highest completed education?",
                      "I understand the how to perform the task.",
                      "I am experienced with data visualization.",
                      "I am experienced with data analysis.",
                      rep(c("I was already familiar with this method.",
                            "I found this visualization easy to use.",
                            "I felt confident in my answers with this method.",
                            "I liked using this method."), 3L)
)

## Functions from source('resp_tbl.r', local = TRUE)
init_resp_tbl   <- make_resp_tbl(participant_num, n_perms)
init_survey_tbl <- make_survey_tbl(participant_num, n_perms)
init_resp_tbl   <- read_join_ans_tbl(init_resp_tbl) ## Adds responses.

## Context, "onStart()" and onStop()
## quasi onStart():
message(paste(sep = " \n",
              "Ran onStart() code.",
              paste0("Spinifex user study, --- Started@ ", Sys.time()),
              paste0("Participant number: ", participant_num, "."),
              paste0("Perm number: ", full_perm_num, ".")
))

##### Global variable initialization -----
n_factors          <- length(factor_nms)       ## ~3
n_p_dim            <- length(p_dim_nms)        ## ~2
n_survey_questions <- length(survey_questions) ## ~18 = 6 + 3 * 4
PC_cap             <- 4L ## Number of principal components to choose from.

## Define section start pages,
intro_pgs   <- 1L:3L   ## Structure, video, splash screen
period1_pgs <- 4L:7L   ## Training, rep 1, rep 2, and intermission
period2_pgs <- 8L:11L  ## Training, rep 1, rep 2, and intermission
period3_pgs <- 12L:14L ## Training, rep 1, rep 2
survey_pg   <- 15L     ## Survey

##### UI start -----
## Above is all initialization and selecting parameters.
## Below is mostly ui objects themselves.

### intro_page1 -----
intro_page1 <- conditionalPanel( ## First page conditionalPanel
  condition = "output.pg == 1",
  h3("Welcome to the study"),
  br(),
  p("This a completely voluntary study that will take approximately 15-20
    minutes to complete. You may quit the study an any time by closing the 
    browser. No identifiable information is collected."),
  br(),
  p("You will be helping to compare and contrast between 3 forms of 
    visuals for multivariate data. The structure of the study is:"),
  p(),
  p("Introduction"),
  tags$ul(
    tags$li("Study overview"),
    tags$li("Explanatory video (4m, 45s)")
  ),
  p("First method"),
  tags$ul(
    tags$li("Training round"),
    tags$li("Evaluation, 2x 60 seconds")
  ),
  p("Second method"),
  tags$ul(
    tags$li("Training round"),
    tags$li("Evaluation, 2x 60 seconds")
  ),
  p("Third method"),
  tags$ul(
    tags$li("Training round"),
    tags$li("Evaluation, 2x 60 seconds")
  ),
  p("Wrap up"),
  tags$ul(
    tags$li("Survey")
  ),
  p("Before we get started, Prolific.co participants must enter their Prolific ID below"),
  textInput(inputId = "prolific_id",
            label = "If you are a Prolific.co participant, enter your Prolific ID (otherwise blank)",
            placeholder = "<Prolific ID or blank>"),
  p("We really appreciate your participation in this study.")
) ## End of first page conditionalPanel, assigning intro_page1

### intro_page2 -----
intro_page2 <- conditionalPanel(
  condition = "output.pg == 2",
  h3("Explainatory video"), br(), br(),
  p("Watch the following video explaining the task and methods before proceeding."), br(),
  tags$video(id = "video", type = "video/mp4", src = "video2_576p.mp4", 
             width = 1024L, height = 576L, controls = NA),
) ## End of conditionalPanel, assigning intro_page2

### intermission_page -----
intermission_page <- conditionalPanel(
  condition = "output.is_intermission == true",
  h3("Intermission"), br(), br(),
  p("Take a minute to stretch, drink some water, or look out the window."),
  p("When you are ready to continue with the next visualization proceed to the next page.")
)

#### Survey, initialization -----
.likert_label <- div("|<- most disagree,  most agree ->|")
col_p1 <- column(4L,
                 h3(paste0("First -- ", this_factor_nm_ord[1L])),
                 br(),
                 tags$ul(
                   tags$li(this_factor_descrip_1[1L]),
                   tags$li(this_factor_descrip_2[1L])
                 ),
                 img(src = this_factor_examp_fp[1L],
                     height="75%", width="75%", align = "center"),
                 lapply(7L:10L, function(i){
                   list(
                     h4(survey_questions[i]),
                     sliderInput(paste0("survey", i), label = .likert_label,
                                 min = 1L, max = 5L, value = 3L, step = 1L)
                   )
                 })
)
col_p2 <- column(4L,
                 h3(paste0("Second -- ", this_factor_nm_ord[3L])),
                 hr(),
                 tags$ul(
                   tags$li(this_factor_descrip_1[3L]),
                   tags$li(this_factor_descrip_2[3L])
                 ),
                 img(src = this_factor_examp_fp[3L],
                     height="75%", width="75%", align = "center"),
                 lapply(11L:14L, function(i){
                   list(
                     h4(survey_questions[i]),
                     sliderInput(paste0("survey", i), label = .likert_label,
                                 min = 1L, max = 5L, value = 3L, step = 1L)
                   )
                 })
)
col_p3 <- column(4L,
                 h3(paste0("Third -- ", this_factor_nm_ord[5L])),
                 hr(),
                 tags$ul(
                   tags$li(this_factor_descrip_1[5L]),
                   tags$li(this_factor_descrip_2[5L])
                 ),
                 img(src = this_factor_examp_fp[5L],
                     height="75%", width="75%", align = "center"),
                 lapply(15L:18L, function(i){
                   list(
                     h4(survey_questions[i]),
                     sliderInput(paste0("survey", i), label = .likert_label,
                                 min = 1L, max = 5L, value = 3L, step = 1L)
                   )
                 })
)

### survey_page -----
survey_page <- conditionalPanel(
  condition = "output.section_nm == 'survey'",
  ## Before save button:
  conditionalPanel( 
    condition = "output.is_saved == false",
    selectInput("survey1", label = survey_questions[1L],
                choices = c("decline to answer",
                            "he/him",
                            "she/her",
                            "thy/them or other")),
    selectInput("survey2", label = survey_questions[2L],
                choices = c("decline to answer",
                            "18 to 24",
                            "25 to 35",
                            "36 to 45",
                            "45 to 60",
                            "60 and up")),
    selectInput("survey3", label = survey_questions[3L],
                choices = c("decline to answer",
                            "Undergraduate degree (BA/BSc/other)",
                            "Graduate degree (MA/MSc/MPhil/other)",
                            "Doctorate degree (PhD/other)")),
    h3("How much do you agree with the following statements?"),
    lapply(4L:6L, function(i){
      list(
        h4(survey_questions[i]),
        sliderInput(paste0("survey", i), label = .likert_label,
                    min = 1L, max = 5L, value = 3L, step = 1L)
      )
    }),
    fluidRow(col_p1, col_p2, col_p3),
    hr(),
    actionButton("save_survey", "Save survey responses")
  ),
  ## After save button:
  conditionalPanel(
    condition = "output.is_saved == true",
    conditionalPanel(
      condition = "output.is_saved == 1",
      h3("Responses saved. Thank you for participating!"),
      conditionalPanel(
        "output.do_disp_prolific_code == true",
        br(),
        h3("Enter the prolifico.co completion code '62CA46F3' to redeem payment.")
      )
    )
  )
) ## Close condition panel, assigning survey_page

##### sidebar_panel ----
sidebar_panel <- conditionalPanel(
  condition = "(output.plot_active == true)",
  sidebarPanel(width = 3L, 
    checkboxGroupInput(
      inputId = "var_resp",
      label   = "Check any/all variables that contribute more than average to the cluster separation green circles and orange triangles.",
      choices = "V1",
      inline  = TRUE)
  )
) ## Close fluidPage(), assigning sidebar_panel



##### main_panel -----
pca_choices <- paste0("PC", 1L:PC_cap)
main_panel <- mainPanel(
  width = 9L,
  ### Text pages
  intro_page1,
  intro_page2,
  intermission_page,
  survey_page,
  ### Plot_active pages:
  ## Header, timer, and plot display
  conditionalPanel(
    condition = "output.plot_active == true",
    h3(textOutput("header")),
    textOutput("timer_disp"),
    ## Image text and image.
    conditionalPanel("output.is_time_remaining == true",
                     imageOutput("image_plot")
                     )
  ), ## close task section conditional panel title text

  #### _Plot mainPanel ----
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
                 choices =  "V1",
                 selected = "V1")
  ), ## Close conditionalPanel(), done listing factor inputs
  ## No input for grand tour.
  h4(textOutput("training_msg")),
  conditionalPanel(condition = "output.pg != 15 && output.is_app_loaded == true",
                   actionButton("next_pg_button", "Next page")
  )
) ## Close mainPanel() End of main_page section.


#### dev_disp -----
dev_disp <- conditionalPanel(
  "output.do_disp_dev_tools == true",
  p("===== Development display below ====="),
  actionButton("browser", "browser()"),
  verbatimTextOutput("pg_next_pg"),
  verbatimTextOutput("image_fp"),
  p("resp_row() & rv$resp_tbl"),
  tableOutput("resp_row"),
  tableOutput("resp_tbl"),
  conditionalPanel(
    "output.plot_active == true",
    p("Variable level diff from avg: "), textOutput("diff"),
    p("Variable level response: "), textOutput("var_resp"),
    p("Variable level marks: "), textOutput("var_marks"),
    p("task_marks: "), textOutput("task_marks")
  )
) ## close conditionalPanel, assigning dev_disp

## customize showNotification placement.
# css_notification <- tags$head(
#   tags$style( ## CSS to change the position of shiny::showNotification().
#     HTML(".shiny-notification {
#              position:fixed;
#              top: calc(70%);
#              left: calc(70%);
#              }
#              "
#     )
#   )
# )

#### ui, combined HTML -----
ui <- fluidPage(titlePanel("Multivariate visualization user study"),
                conditionalPanel("output.is_app_loaded == false",
                                 h3("Please wait while the app loads. If it's not ready in 20 seconds please try refreshing")
                ),
                #css_notification,
                sidebarLayout(
                  sidebar_panel,
                  main_panel
                ),
                dev_disp
)

## App text formatting function (js wrapper for bold, red text)
text_boldred <- function(string){
  paste0("<strong><span style='color:red'>", string, "</span><strong>")
}

### Global initialize for each factor variant.

##### global.r, spinifex_study -----

### Setup -----
source("resp_tbl.r", local = TRUE) ## Needs initialization from global.r.
library("shiny")
library("googlesheets4") ## Google sheets (with api v4) for read/write responses.
do_disp_dev_tools <- TRUE ## Expects: TRUE / FALSE
options(shiny.autoreload = TRUE) ## May reduce caching errors
#options(error = browser) ## occasionally helpful for troubleshooting
time_alotted <- 60L ## Seconds for the task
height_px <- 500L ## Default height [pixels] for plot
ss_id <- "1K9qkMVRkrNO0vufofQJKWIJUyTys_8uVtEBdJBL_DzU" ## Hash or name of the google sheet
## Google sheets id number:
## spinifex_study resp_tbl    1K9qkMVRkrNO0vufofQJKWIJUyTys_8uVtEBdJBL_DzU
## Google sheets auth code
## 23/12/2020, nitro acer laptop: 4/1AY0e-g5NhEF12mV_4U_d1MzO0GrnNZUlaCCNvq-lLTPJ0Ry8iubLXQJ9uCI

## Prolific.co to see the study draft page go to:
# if(F)
#   browseURL("https://app.prolific.co/studies/5fd808e32ce90812aeb9cd90")

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
                          c("EEE_P4_0_1_t1__pca_x1y2.png", "EEE_P4_0_1_t1__grand.gif", "EEE_P4_0_1_t1__radial_mv2.gif"))
location_nms    <- c("0_1", "33_66", "50_50")
vc_nms          <- c("EEE", "EEV", "banana")
p_dim_nms       <- this_p_dim_nm_ord <- c("p4", "p6")
## The permutation counts
r_fct <- nrow(factor_perms)     ##~6
r_loc <- nrow(location_perms)   ##~6
r_vc  <- nrow(vc_perms)         ##~1
r_perms <- r_fct * r_loc * r_vc ##~36

#### Assign participant_num and perm_num -----
## Read response sheet and set participant number to first not use integer

participant_num <- 1L ## Initialize
## API reads UNLOCKED. keep in mind API quota issue.
if(T){
  ## tryCatch for api quota limit
  prev_saves <- NULL
  tryCatch({
    prev_saves <- googlesheets4::read_sheet(ss_id, sheet = 1L)
  }, error = function(e){
    txt <- "Google API quota reached, Please try again in 2 minutes. Closing app in 15 seconds."
    showNotification(txt, type = "error", duration = 15L)
    warning(txt)
    Sys.sleep(15L)
    stopApp()
    return(NULL)
  })
used_nums <- unique(prev_saves$participant_num)
opts <- 1L:(max(used_nums) + 1L)
open_nums <- opts[!opts %in% used_nums] ## All not used numbers
participant_num <- min(open_nums)
}else{participant_num <- sample(1L:999L, 1L)}
full_perm_num <- 1L + participant_num %% r_perms

## Select permutation orders
this_factor_perm   <- 1L + (full_perm_num - 1L) %% r_fct
this_location_perm <- 1L + floor((full_perm_num - 1L) / r_fct) %% (r_fct * r_loc)
this_vc_perm       <- 1L # Fixed parameter #1L + floor((full_perm_num - 1L) / (r_fct * r_loc)) %% r_perms
## The decoded names
this_factor_nm_ord   <- factor_nms[factor_perms[this_factor_perm, ]]
this_vc_nm_ord       <- vc_nms[vc_perms[this_vc_perm, ]]
this_location_nm_ord <- location_nms[location_perms[this_location_perm, ]]
## Factor descriptions & filepaths
this_factor_descrip_1 <- factor_descrip_1[factor_perms[this_factor_perm, ]]
this_factor_descrip_2 <- factor_descrip_2[factor_perms[this_factor_perm, ]]
this_factor_examp_fp  <- factor_examp_fp[factor_perms[this_factor_perm, ]]

## Functions from source('resp_tbl.r', local = TRUE)
init_resp_tbl   <- make_resp_tbl(participant_num)
init_survey_tbl <- make_survey_tbl(participant_num)


## Context, "onStart()" and onStop()
context_line <- paste0("Spinifex STUDY, --- (spinifex v",
                       packageVersion("spinifex"), ") --- Started ", Sys.time())
message("ran onStart() code.")
## quasi onStart():
context_msg <- paste(sep = " \n",
                     context_line,
                     paste0("Participant number: ", participant_num, "."),
                     paste0("Perm number: ", full_perm_num, ".")
)
cat(context_msg)

## Survey questions; n = 21 = 9 + 12
survey_questions <- c("What are your prefered pronouns?",
                      "Which age group do you belong to?",
                      "What is your highest completed education?",
                      "I understand the how to perform the task.",
                      "I am experienced with data visulization.",
                      "I am experienced with data analysis.",
                      rep(c("I was already familiar with this method.",
                            "I found this visualization easy to use.",
                            "I felt confident in my answers with this method.",
                            "I liked using this method."), 3L)
)
init_survey_tbl$question = survey_questions

#### Load data -----
### Still needed for evaluation, sizable app content uses dat()
root <- ("./www/data/")
these_sim_nms <- paste(rep(this_vc_nm_ord, 3L), 
                       rep(p_dim_nms, 3L), 
                       rep(this_location_nm_ord, 3L), sep = "_")
these_sim_nms <- c(paste0("EEE_p4_0_1_t", 1L:3L), 
                   as.vector(outer(these_sim_nms, 
                                   paste0("_rep", 1L:3L),
                                   FUN = "paste0")
                  ) ## cross product paste
) 
sapply(these_sim_nms, function(i){
  this_sim_nm <-paste0(root, i, ".rda")
  load(this_sim_nm, envir = globalenv())
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
    tags$li("Training round"),
    tags$li("Evaluation, 2x 60 seconds")
  ),
  p("Period 2"),
  tags$ul(
    tags$li("Training round"),
    tags$li("Evaluation, 2x 60 seconds")
  ),
  p("Period 3"),
  tags$ul(
    tags$li("Training round"),
    tags$li("Evaluation, 2x 60 seconds")
  ),
  p("Wrap up"),
  tags$ul(
    tags$li("Survey & save")
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
  h2("Video training"), br(), br(),
  p("Watch the following video before proceeding:"), br(),
  p("Minimize the study and watch the training video."),
  ##TODO UPDATE AND review with new video; youtube needed?
  ## Adding the 'a' tag to the sidebar to link to external file
  #tags$a(href='training.mp4', target='blank', 'training video (4:17)'),
) ## End of conditionalPanel, assigning intro_page2

### intermission_page -----
intermission_page <- conditionalPanel(
  condition = "output.is_intermission == true",
  h2("Intermission"), br(), br(),
  p("Take a minute to strech, drink some water, or look out the window."),
  p("When you are ready to continue with the next visulization proceed to the next page.")
)

#### Survey, initialization -----
.surv_lab  <-  div(style = 'width:300px;',
                   div(style = 'float:left;', '|<- disagree'),
                   div(style = 'float:right;', 'agree ->|'))
.surv_lab2 <-  div(style = 'width:220px;',
                   div(style = 'float:left;', '|<- disagree'),
                   div(style = 'float:right;', 'agree ->|'))
col_p1 <- column(4L,
                 h3(paste0("First -- ", this_factor_nm_ord[1L])),
                 br(),
                 tags$ul(
                   tags$li(this_factor_descrip_1[1L]),
                   tags$li(this_factor_descrip_2[1L])
                 ),
                 img(src = this_factor_examp_fp[1L],
                     height="75%", width="75%", align = "center"),
                 h4(survey_questions[7L]),
                 sliderInput("survey7", label = .surv_lab2,
                             min = 1L, max = 5L, value = 3L, step = 1L),
                 h4(survey_questions[8L]),
                 sliderInput("survey8", label = .surv_lab2,
                             min = 1L, max = 5L, value = 3L, step = 1L),
                 h4(survey_questions[9L]),
                 sliderInput("survey9", label = .surv_lab2,
                             min = 1L, max = 5L, value = 3L, step = 1L),
                 h4(survey_questions[10L]),
                 sliderInput("survey10",
                             label = .surv_lab2,
                             min = 1L, max = 5L, value = 3L, step = 1L)
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
                 h4(survey_questions[11L]),
                 sliderInput("survey11", label = .surv_lab2,
                             min = 1L, max = 5L, value = 3L, step = 1L),
                 h4(survey_questions[12L]),
                 sliderInput("survey12", label = .surv_lab2,
                             min = 1L, max = 5L, value = 3L, step = 1L),
                 h4(survey_questions[13L]),
                 sliderInput("survey13", label = .surv_lab2,
                             min = 1L, max = 5L, value = 3L, step = 1L),
                 h4(survey_questions[14L]),
                 sliderInput("survey14", label = .surv_lab2,
                             min = 1L, max = 5L, value = 3L, step = 1L),
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
                 h4(survey_questions[15L]),
                 sliderInput("survey15", label = .surv_lab2,
                             min = 1L, max = 5L, value = 3L, step = 1L),
                 h4(survey_questions[16L]),
                 sliderInput("survey16", label = .surv_lab2,
                             min = 1L, max = 5L, value = 3L, step = 1L),
                 h4(survey_questions[17L]),
                 sliderInput("survey17", label = .surv_lab2,
                             min = 1L, max = 5L, value = 3L, step = 1L),
                 h4(survey_questions[18L]),
                 sliderInput("survey18", label = .surv_lab2,
                             min = 1L, max = 5L, value = 3L, step = 1L)
)

### survey_page -----
suvery_page <- conditionalPanel(
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
    h4(survey_questions[4L]),
    sliderInput("survey4", label = .surv_lab,
                min = 1L, max = 5L, value = 3L, step = 1L),
    h4(survey_questions[5L]),
    sliderInput("survey5", label = .surv_lab,
                min = 1L, max = 5L, value = 3L, step = 1L),
    h4(survey_questions[6L]),
    sliderInput("survey6",label = .surv_lab,
                min = 1L, max = 5L, value = 3L, step = 1L),
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
      conditionalPanel(
        "output.do_disp_prolific_code == true",
        br(),
        h3("Enter the completion code '18B6C620' to redeem payment.")
      )
    )
  )
) ## Close condition panel, assigning survey_page

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



##### main_panel -----
pca_choices <- paste0("PC", 1L:PC_cap)
main_panel <- mainPanel(
  width = 9L,
  ### Text pages
  intro_page1,
  intro_page2,
  intermission_page,
  suvery_page,
  ### Plot_active pages:
  ## Header, timer, and plot display
  conditionalPanel(
    condition = "output.plot_active == true",
    h2(textOutput("header")),
    textOutput("timer_disp"),
    ## Image text and image.
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
  ), ## Close conditionalPanel(), done listing factor inputs
  ## No input for grand tour.
  
  conditionalPanel(condition = "(output.pg != 15)",
                   actionButton("next_pg_button", "Next page")
  )
) ## Close mainPanel() End of main_page section.


### dev_disp -----
dev_disp <- conditionalPanel(
  "output.do_disp_dev_tools == true",
  p("===== Development display below ====="),
  actionButton("browser", "browser()"),
  verbatimTextOutput("pg_next_pg"),
  verbatimTextOutput("image_fp"),
  textOutput("dev_msg"),
  tableOutput("resp_row"),
  tableOutput("resp_tbl"),
  verbatimTextOutput("save_survey"),
  conditionalPanel(
    "output.plot_active == true",
    p("Variable level diff from avg: "), textOutput("diff"),
    p("Variable level response: "), textOutput("var_resp"),
    p("Variable level marks: "), textOutput("var_marks"),
    p("task_marks: "), textOutput("task_marks")
  )
) ## close conditionPanel, assigning dev_disp

##### ui, combined *_page pieces -----
ui <- fluidPage(
                titlePanel("Multivariate vis user study"),
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

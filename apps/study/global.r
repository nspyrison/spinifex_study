### Global initialize for each factor variant.

##### global.r, spinifex_study -----

### Setup -----
library("shiny")
library("googlesheets4") ## Google sheets (with api v4) for read/write responses.
library("shinyjs")   ## Help with handling conditionalPanels
library("lubridate") ## For timer
library("here")      ## Fixing base dir
set.seed(20200927)   ## If tourr starts using seeds
time_alotted <- 180L ## Seconds for the task
height_px <- 500L
pal <- RColorBrewer::brewer.pal(8, "Dark2")[c(1, 2, 3, 6, 8)]
ss_id <- "1K9qkMVRkrNO0vufofQJKWIJUyTys_8uVtEBdJBL_DzU" ## the 'id' or name of the google sheet
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

do_disp_dev_tools <- FALSE#TRUE
options(shiny.autoreload = TRUE)
# options(error = browser) ## occasionally helpful for troubleshooting

#### participant and perm_number -----
## Initialize
participant_num <- 1
## TODO: need to read the unique perm numbers in load file, and find the perm_numbers in the min count table. 
if(F){ ## Read response sheet and set participant number
  prev_saves <- read_sheet(ss_id)
  participant_num <- length(unique(prev_saves$participant_num)) + 1
}else{ ## ELSE assign random participant and perm numbers.
  participant_num <- sample(1:999, 1)
}
full_perm_num <- 1 + participant_num %% 56

#### Select factor and block permutations
## Possible permutations
factor_perms   <- rbind(c(1, 1,  2, 2,  3, 3),
                        c(1, 1,  3, 3,  2, 2),
                        c(2, 2,  3, 3,  1, 1),
                        c(2, 2,  1, 1,  3, 3),
                        c(3, 3,  1, 1,  2, 2),
                        c(3, 3,  2, 2,  1, 1))
location_perms <- rbind(c(1, 1,  2, 2,  3, 3),
                        c(1, 1,  3, 3,  2, 2),
                        c(2, 2,  3, 3,  1, 1),
                        c(2, 2,  1, 1,  3, 3),
                        c(3, 3,  1, 1,  2, 2),
                        c(3, 3,  2, 2,  1, 1))
vc_perms       <- rbind(c(1, 1,  2, 2,  3, 3))
## set factor and block names
factor_nms   <- c("pca", "grand", "radial")
location_nms <- c("0_1", "33_66", "50_50")
vc_nms       <- c("EEE", "EEV", "banana")
p_dim_nms <- this_p_dim_nm_ord <- c("p4", "p6")
## The permutation numbers
r_fct <- nrow(factor_perms)   ##~6
r_loc <- nrow(location_perms) ##~6
r_vc  <- nrow(vc_perms)       ##~1
r_perms <- r_fct * r_loc * r_vc ##~36
this_factor_perm   <- 1 + (full_perm_num - 1) %% r_fct
this_location_perm <- 1 #1 + floor((full_perm_num - 1) / r_fct) %% r_loc
this_vc_perm       <- 1 #1 + floor((full_perm_num - 1) / (r_fct * r_Loc)) %% r_vc
## The decoded names
this_factor_nm_ord <-
  factor_nms[factor_perms[this_factor_perm, ]]
this_vc_nm_ord <-
  vc_nms[vc_perms[this_vc_perm, ]]
this_location_nm_ord <-
  location_nms[location_perms[this_location_perm, ]]

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
root <- ("./www/data/") ## Local 

this_sim_nms <- paste(rep(this_vc_nm_ord, 3), rep(p_dim_nms, 3), rep(this_location_nm_ord, 3), sep = "_")
this_sim_nms <- c(paste0("EEE_p4_0_1_t", 1:3), 
                  as.vector(outer(this_sim_nms, paste0("_rep", 1:3), FUN = "paste0")) ## cross product paste
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
  titlePanel("User study"),
  actionButton("next_pg_button", "Next page")
)

##### sidebar_ui ----
sidebar_ui <- fluidPage(
  ##### _Task response input -----
  ## Task 2
  conditionalPanel(
    condition = "(output.plot_active == true) ", 
    checkboxGroupInput(
      inputId = "var_resp",
      label   = "Check any/all variables that contribute more than average to the cluster seperation green circles and orange triangles.",
      choices = "V1",
      inline  = TRUE)
  )
) ## Close conditionalPanel(), assigning sidebar_ui



##### main_ui -----
main_ui <- mainPanel(
  width = 9,
  ### _Timer_disp
  textOutput("timer_disp"),
  ### _Header
  conditionalPanel(
    condition = "output.plot_active == true",
    h2(textOutput('header')),
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
                          choices = paste0("PC", 1:PC_cap),
                          selected = "PC1", inline = TRUE),
             radioButtons(inputId = "y_axis", label = "y axis",
                          choices = paste0("PC", 1:PC_cap),
                          selected = "PC2", inline = TRUE)
    )
  ),
  ## Radial manip var radio buttons
  conditionalPanel(
    condition = "output.factor == 'radial'",
    radioButtons(inputId = "manip_var_nm", label = "Manip variable:",
                 choices =  "V1", selected = "V1")
  ), ## Close conditionalPanel()
  ## No input for grand tour.
) ## Close mainPanel() End of main_ui section.


### survey_ui -----
survey_ui <- mainPanel(
  conditionalPanel(
    "(output.pg == 15)", ## hardcoded.
    actionButton("save_survey", "Save survey responses")
  )
) 


### _dev_tools
dev_tools <- conditionalPanel(
  "output.dev_tools == true",
  p("===== Development display below ====="),
  actionButton("browser", "browser()"),
  textOutput("dev_msg"),
  tableOutput("resp_row"),
  tableOutput("resp_tbl"),
  conditionalPanel(
    "output.plot_active == true",
    p("Variable level diff from avg: "), textOutput("diff"),
    p("Variable level response: "), textOutput("var_resp"),
    p("Variable level marks: "), textOutput("var_marks"),
    p("Task marks: "), textOutput("marks")
  )
) ## close conditionPanel, assigning dev_tools

##### ui, combined *_ui pieces -----
ui <- fluidPage(useShinyjs(), ## Required in ui to use shinyjs.
                header_ui,
                sidebarLayout(
                  sidebarPanel(sidebar_ui),
                  mainPanel(main_ui)
                ),
                dev_tools
)

## App text formatting function (js wrapper for bold, red text)
text_boldred <- function(string){
  paste0("<strong><span style='color:red'>", string, "</span><strong>")
}

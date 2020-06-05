### Global init for each factor variant.

##### global.r, spinifex_study -----
### Setup -----
library("ggplot2")
library("spinifex")
library("shiny")
library("tidyr")
library("dplyr")
library("plotly")
library("GGally")
# library("shinyjs")   ## More control of shiny widgets
## not used yet
library("lubridate") ## For timer
library("loggit")    ## For logging
library("git2r")     ## For logging latest git commits

#### Logging
## browseURL("https://www.r-bloggers.com/adding-logging-to-a-shiny-app-with-loggit/")
## use: loggit("INFO", "<main msg>", "<detail>")
## Uncomment the following line to apply logging
do_log <- TRUE
do_disp_dev_tools <- FALSE
#### Simulated data series, 
## "series" or iteration of data to look at. Should be an even hundred
sim_series <- 300


## Set log file, finding the first unused number, will need to write to a google sheet or otherwise store a file.
log_base <- paste0("log_", Sys.info()[4], "_")
log_num  <- 1
log_file <- ""
if (do_log == T){
  log_name <- sprintf(paste0(log_base, "%03d"), log_num)
  log_file <- paste0(log_name, ".json")
  while (file.exists(log_file)){ ## Find an unused log number
    log_num  <- log_num + 1
    log_name <- sprintf(paste0(log_base, "%03d"), log_num)
    log_file <- paste0(log_name, ".json")
  }
  set_logfile(log_file)
} else { ## when do_log != T
  log_num  <- sample(1:3, 1)
  log_file <- "Logging is off! Log and responses not being recorded."
}


## Set group (factor order) based on log number mod 3
this_group <- 1 + (log_num - 1) %% 3 ## Expects [1,2,3] 
fct_ord_latin_sq <- rbind(c(1, 2, 3), ## ~ grp 1; "pca", "grand", "manual"
                          c(2, 3, 1), ## ~ grp 2; "grand", "manual", "pca"
                          c(3, 1, 2)  ## ~ grp 3; "manual", "pca", "grand"
)
fct_nm_vect <- c("pca", "grand", "manual") ## factor list
this_factor_order    <- fct_ord_latin_sq[this_group, ]
this_factor_nm_order <- fct_nm_vect[this_factor_order]


## Context, "onStart()" and onStop()
context_line <- paste0("Spinifex STUDY, --- (spinifex v", packageVersion("spinifex"),
                       ") --- Started ", Sys.time())
this_Sys.info <- paste(Sys.info()[1:5], collapse = ", ")
context_msg <- paste(sep = " \n",
                     context_line,
                     paste0("Log file: ", log_file), 
                     paste0("Group number: ", log_num, "."),
                     paste0("Sys.info()[1:5]: ", this_Sys.info),
                     paste0("Last spinifex_study commit: ", 
                            capture.output(git2r::repository("."))[3])
                     # paste0("Last spinifex commit: ", 
                     #        capture.output(git2r::repository("../spinifex/"))[3])
)
loggit("INFO", "=====Spinifex study app start.=====")
cat(context_msg)

onStop(function() {
  cat(context_msg)
  loggit("INFO", "=====Spinifex study app stop.=====")
  
  ## Try to autosave if not saved and do_log == T?
  #### note that rv$resp_tbl is out of scope to the global file.
})


##### Required inputs -----
## Tasks, and block difficulty
s_difficulty      <- c("easy", "hard")
s_task_prompts    <- c("How many clusters do you see?",
                       "Rate the relative importance of ANY/ALL variables in terms of 
                       distinugishing between the given clusters.")
s_task2_questions <- c("Very important distinguishing clusters 'a' from 'b'",
                       "Somewhat important distinguishing clusters 'a' from 'b'",
                       "Very important distinguishing clusters 'b' from 'c'",
                       "Somewhat important distinguishing clusters 'b' from 'c'")

## Survey questions
s_survey_questions <- c("What gender are you?",
                        "What age are you?",
                        "What is your English proficiency?",
                        "What is your highest completed education?",
                        "I am experienced with data visualization.",
                        "I have education in multivariate statistical analysis.",
                        rep(c("I was already familiar with this visualization.",
                              "I found this visualization easy to use.",
                              "I felt confident in my answers with this visualization.",
                              "I liked using this visualization."), 3)
)

## Load training data and tour paths
t_dat_len <- 4
s_t_dat <- s_t_tpath <-  list()
for (i in 1:t_dat_len) {
  s_t_dat[[i]]   <- readRDS(
    paste0("./data/simulation_data_t", i, ".rds")
  )
  s_t_tpath[[i]] <- readRDS(
    paste0("./data/grand_tpath_t", i, ".rds")
  )
}

## Load data and tour paths
dat_len   <- 12
s_dat <- s_tpath <-  list()
for (i in 1:dat_len) {
  s_dat[[i]] <- readRDS(
    paste0("./data/simulation_data", sim_series + i, ".rds")
  )
  s_tpath[[i]] <- readRDS(
    paste0("./data/grand_tpath", sim_series + i, ".rds")
  )
}


##### Global variable initialization -----
n_trainings        <- length(s_t_dat)            ## ~4
n_factors          <- length(fct_nm_vect)        ## ~3
n_tasks            <- 2                          ## ~2
n_task2_questions  <- length(s_task2_questions)  ## ~4
n_difficulty       <- length(s_difficulty)       ## ~3
n_blocks           <- 2                          ## ~2
n_survey_questions <- length(s_survey_questions) ## ~18
PC_cap             <- 4 ## Number of principal components to choose from.
pal                <- "Dark2"

#### Define section start pages, 
## may need manual changes when changing section sizes
## intro is pg 1; video training is pg 2
training_start_pg <- 3
task_start_pg     <- (training_start_pg + n_trainings * n_tasks + 1) + 1
## ~ pg9;(3+2*2+1+1; train_st, 2*2 task*blocks, splash pg, start on new pg)
survey_start_pg   <- task_start_pg + n_factors * n_blocks * n_tasks + 1
## ~ pg22, (9+3*3*2+1; task_st, 3*2*2 factor*task*block, start on new pg) 

##### UI START -----
### header_ui -----
header_ui <- fluidPage(
  titlePanel("Multivariate data visualization study"),
  conditionalPanel(
    condition = "output.section == 'training' && output.second_training == 'ask'",
    HTML("<h3><span style='color:red'>
          Do you want another training set?
           </span></h3>"),
    checkboxInput("second_training", "", value = FALSE)
  ),
  conditionalPanel(
    condition = "output.pg < 22",
    actionButton("next_pg_button", "Next page")
  )
)


# ##### sidebar_ui ----
sidebar_ui <- conditionalPanel(
  condition = "output.section == 'training'
              || output.section == 'task'",
  sidebarPanel(
    ##### _Training text -----
    conditionalPanel(
      condition = "output.section == 'training'",
      conditionalPanel( ## interface familiarity
        condition = "output.section_pg == 1",
        p("In this study, you will be working with 3 visualization techniques of
        multivariate data. Each one uses 2-dimensional projections created
        from different combinations of variables. The variable map (grey circle)
        shows the angle and magnitude that each variable contributes to the
        projection."),
        p("Principal Component Analysis (PCA) is displayed first. Use the radio
          buttons on the left sidebar panel to select new components to be
          displayed. Observe how the clusters and the variable contributions
          change."),
        p("Now switch to the grand tour factor. Play the animation. Notice how
          different clusters move as the variable contributions change. Drag
          the slider to select a different frame or animate at your own pace."),
        p("Change to the manual tour. You can select which components are on
          the axes. Using the drop-down, select the variable with the largest
          line segment. Use the slider to change the variable's contribution.
          Watch how the contributions and clusters move as a result. Select a
          change the y-axis to PC3 and back, notice that this resets the
          projection."),
      ),
      conditionalPanel( ## Rraining task 1, pg 1
        condition = "output.task == 1",
        strong("Now the data points are not colored by their cluster. How
        many clusters do you see in this training set?
        Make sure to use the controls and different factors.")
      ),
      conditionalPanel( ## Rraining task 1, pg 2
        condition = "output.task == 2",
        strong("The data points are now colored by their cluster again.
               Variables that have a large
               contribution in line with two clusters are important to
               distinguish them. However, you cannot rule out that variables
               with a small contribution are unimportant.
               Use this information to identify which variables distinguish
               the 2 clusters.")
      ),
      hr()
    ), ### end training text 
    
    ##### _Training control inputs -----
    ## Factor selection
    conditionalPanel(condition = "output.section == 'training' && output.section_pg < 6",
                     radioButtons(inputId = "factor", label = "Factor",
                                  choices = fct_nm_vect,
                                  selected = fct_nm_vect[1],
                                  inline = TRUE)
    ), ## PCA axis selection
    conditionalPanel(
      condition = "(output.factor == 'pca' || output.factor == 'manual') ||
                  (output.section == 'training' && input.factor != 'grand')",
      fluidRow(column(6, radioButtons(inputId = "x_axis", label = "x axis",
                                      choices = paste0("PC", 1:4), selected = "PC1")),
               column(6, radioButtons(inputId = "y_axis", label = "y axis",
                                      choices = paste0("PC", 1:4), selected = "PC2"))
      )
    ), ## Manip var/ magnitude selection
    conditionalPanel(condition = "output.factor == 'manual' ||
                       (output.section == 'training' && input.factor == 'manual')",
                     selectInput("manip_var_nm", "Manip var", "<none>"),
                     sliderInput("manip_slider", "Contribution",
                                 min = 0, max = 1, value = 0, step = .1)
    ),
    
    ##### _Task response input -----
    ## Task 1
    conditionalPanel(condition = "(output.task == 1 || output.task == 2)
                                 && output.factor != 'grand'",
                     hr()
    ),
    conditionalPanel(condition = "output.task == 1",
                     strong(s_task_prompts[1]),
                     br(),
                     numericInput("tsk1_resp", "",
                                  value = 0, min = 0, max = 10)
    ), ## Task 2
    conditionalPanel(condition = "output.task == 2",
                     strong(s_task_prompts[2]),
                     br(), br(),
                     checkboxGroupInput(
                       inputId = "tsk2_resp_very_ab",
                       label   = s_task2_questions[1],
                       choices = "V1",
                       inline  = TRUE
                     ),
                     checkboxGroupInput(
                       inputId = "tsk2_resp_some_ab",
                       label   = s_task2_questions[2],
                       choices = "V1",
                       inline  = TRUE
                     ),
                     hr(),
                     checkboxGroupInput(
                       inputId = "tsk2_resp_very_bc",
                       label   = s_task2_questions[3],
                       choices = "V1",
                       inline  = TRUE
                     ),
                     checkboxGroupInput(
                       inputId = "tsk2_resp_some_bc",
                       label   = s_task2_questions[4],
                       choices = "V1",
                       inline  = TRUE
                     )
    )
    
  ) ## Close sidebarPanel()
) ## Close conditionalPanel(), end sidebar_ui section

##### Init survey columns -----
.surv_lab <- HTML("<div style=\"width:300px;\">
                    <div style=\"float:left;\">strongly disagree</div>
                    <div style=\"float:right;\">strongly agree</div>
                  </div>")
col_p1 <- column(4, 
                 h3(this_factor_nm_order[1]),
                 hr(),
                 h4(s_survey_questions[7]),
                 sliderInput("survey7",label = .surv_lab,
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[8]),
                 sliderInput("survey8",label = .surv_lab,
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[9]),
                 sliderInput("survey9", label = .surv_lab,
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[10]),
                 sliderInput("survey10",
                             label = .surv_lab,
                             min = 1, max = 9, value = 5)
)

col_p2 <- column(4, 
                 h3(this_factor_nm_order[2]),
                 hr(),
                 h4(s_survey_questions[11]),
                 sliderInput("survey11", label = .surv_lab,
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[12]),
                 sliderInput("survey12",label = .surv_lab,
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[13]),
                 sliderInput("survey13", label = .surv_lab,
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[14]),
                 sliderInput("survey14", label = .surv_lab,
                             min = 1, max = 9, value = 5),
)
col_p3 <- column(4, 
                 h3(this_factor_nm_order[3]),
                 hr(),
                 h4(s_survey_questions[15]),
                 sliderInput("survey15", label = .surv_lab,
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[16]),
                 sliderInput("survey16", label = .surv_lab,
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[17]),
                 sliderInput("survey17", label = .surv_lab,
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[18]),
                 sliderInput("survey18", label = .surv_lab,
                             min = 1, max = 9, value = 5)
)

##### main_ui -----
main_ui <- mainPanel(
  ### _Intro mainPanel -----
  conditionalPanel(
    condition = "output.section == 'intro'",
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
              <li>Task 1 (x2 difficulties, 60 sec)</li>
              <li>Task 2 (x2 difficulties, 180 sec)</li>
            </ul>"),
      p("Wrap up study"),
      HTML("<ul>
              <li>Complete survey</li>
              <li>Save and exit from app</li>
            </ul>"),
      p("We really appreciate your participation in this study.")
    ), ## End first page
    conditionalPanel(
      condition = "output.pg == 2", ## Video, second page
      h2("Video training"), br(), br(),
      p("Watch the following video before proceeding:"), br(),
      ## Adding the 'a' tag to the sidebar linking external file
      p("Minimize the study and watch the training video."),
      #a(href='training.mp4', target='blank', 'training video (4:17)'),
      br(), br(),
      p("If this link only contains audio let the invigilator know.")
    )  ## end of video, second page
  ), ## close conditionPanel -- intro section text
  
  ### _Training mainPanel -----
  conditionalPanel(
    condition = "output.section == 'training'",
    conditionalPanel(condition = "output.section_pg == 1", ## ui intro
                     h2("Training -- interface")
    ),
    conditionalPanel(condition = "output.section_pg == 2",
                     h2("Training -- task 1")
    ),
    conditionalPanel(condition = "output.section_pg == 3",
                     h2("Training -- task 1, set 2")
    ),
    conditionalPanel(condition = "output.section_pg == 4",
                     h2("Training -- task 2")
    ),
    conditionalPanel(condition = "output.section_pg == 5",
                     h2("Training -- task 2, set 2")
    ),
    conditionalPanel( ## splash page
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
  ), ## close training section main panel text
  
  ### _Task mainPanel -----
  conditionalPanel(
    condition = "output.section == 'task'",
    h2(textOutput('task_header')),
    textOutput('timer_disp'),
    hr()
  ), ## close task section conditional panel title text
  
  ### _Plot mainPanel ----
  conditionalPanel(
    condition = "(output.section == 'training' && output.section_pg != 6) ||
      output.section == 'task'", ## output.section_pg == 6 is splash page.
    htmlOutput("plot_msg"),
    plotOutput("pca_plot", height = "auto"),
    plotOutput("mtour_plot", height = "auto"),
    plotlyOutput("gtour_plot", height = "auto")
  ), ## Close plot conditional panel
  
  ### _Survey mainPanel -----
  conditionalPanel(
    condition = "output.section == 'survey'",
    conditionalPanel(
      condition = "output.is_saved == 0",
      selectInput("survey1", label = s_survey_questions[1],
                  choices = c("decline to answer", "female", "male",
                              "intergender/other")
      ),
      selectInput("survey2", label = s_survey_questions[2],
                  choices = c("decline to answer", "19 or younger", "20 to 29", 
                              "30 to 39", "40 or older")
      ),
      selectInput("survey2", label = s_survey_questions[3],
                  choices = c("decline to answer", "Learned and used from birth", 
                              "Fluent conversational", "Less than conversational")
      ),
      selectInput("survey3", label = s_survey_questions[4],
                  choices = c("decline to answer", "High school", 
                              "Undergraduate", "Honors, masters, mba", "Doctorate")
      ),
      h3("How much do you agree with the following statements?"),
      h4(s_survey_questions[5]),
      sliderInput("survey4", label = .surv_lab,
                  min = 1, max = 9, value = 5),
      h4(s_survey_questions[6]),
      sliderInput("survey5",label = .surv_lab,
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


##### UI, combine panels -----
if (do_disp_dev_tools == F){
  ui <- fluidPage(header_ui,
                  sidebar_ui,
                  main_ui
  )
} else { ## if do_disp_dev_tools == T
  ui <- fluidPage(header_ui,
                  sidebar_ui,
                  main_ui, 
                  ### DEV helping displays:
                  actionButton("browser", "browser()"),
                  verbatimTextOutput("dev_msg"),
                  h4("task2 ans ptile:"),   verbatimTextOutput("task2_ans_ptile"),
                  h4("task2 ans:"),   verbatimTextOutput("task2_ans"),
                  h4("task2 score:"), verbatimTextOutput("task2_score"),
                  tableOutput("resp_tbl")
  )
}


##### App local functions below: -----
app_render_ <- function(slides, ## paste over spinifex render to add size
                        axes = "left",
                        alpha = 1,
                        cluster = NULL,
                        ...) {
  ## Initialize
  if (length(slides) == 2)
    data_slides  <- data.frame(slides[[2]])
  basis_slides   <- data.frame(slides[[1]])
  manip_var      <- attributes(slides$basis_slides)$manip_var
  n_slides       <- max(basis_slides$slide)
  p              <- nrow(basis_slides) / n_slides
  d              <- ncol(basis_slides) - 2
  angle          <- seq(0, 2 * pi, length = 360)
  circ           <- data.frame(x = cos(angle), y = sin(angle))
  ## Scale basis axes
  if (axes != "off"){
    zero         <- app_set_axes_position(0, axes)
    circ         <- app_set_axes_position(circ, axes)
    basis_slides <- data.frame(app_set_axes_position(basis_slides[, 1:2], axes), 
                               basis_slides[, (d+1):ncol(basis_slides)])
  }
  ## manip var axes asethetics
  axes_col <- rep("red", p)
  axes_siz <- rep(0.3, p)
  axes_col[manip_var] <- "blue"
  axes_siz[manip_var] <- .6
  
  x_max <- max(data_slides[, 1], circ[, 1])
  x_min <- min(data_slides[, 1], circ[, 1])
  y_max <- max(data_slides[, 2], circ[, 2])
  y_min <- min(data_slides[, 2], circ[, 2])
  x_range <- x_max - x_min
  y_range <- y_max - y_min
  
  gg <- 
    ## ggplot settings
    ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(panel.grid.major = element_blank(), ## Remove grid lines
                   panel.grid.minor = element_blank(), ## Remove grid lines
                   axis.text.x  = element_blank(),     ## Remove axis marks
                   axis.text.y  = element_blank(),     ## Remove axis marks
                   axis.title.x = element_blank(),     ## Remove axis titles for gtour
                   axis.title.y = element_blank(),     ## Remove axis titles for gtour
                   aspect.ratio = y_range / x_range, 
                   legend.box.background = element_rect(),
                   legend.title = element_text(size = 18, face = "bold"),
                   legend.text  = element_text(size = 18, face = "bold")) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::xlim(x_min, x_max) +
    ggplot2::ylim(y_min, y_max) +
    ## Projected data points
    suppressWarnings( ## Suppress for unused aes "frame".
      ggplot2::geom_point( 
        data = data_slides, size = 3, alpha = alpha,
        mapping = ggplot2::aes(x = x, y = y, frame = slide,
                               color = cluster, 
                               fill  = cluster, 
                               shape = cluster)
      )
    )
  
  if (axes != "off"){
    gg <- gg +
      ## Circle path 
      ggplot2::geom_path(
        data = circ, color = "grey80", size = .3, inherit.aes = F,
        mapping = ggplot2::aes(x = x, y = y)
      ) +
      ## Basis axes segments
      suppressWarnings( ## Suppress for unused aes "frame".
        ggplot2::geom_segment( 
          data = basis_slides, size = axes_siz, colour = axes_col,
          mapping = ggplot2::aes(x = x,
                                 y = y, 
                                 xend = zero[, 1], yend = zero[, 2], 
                                 frame = slide)
        )
      ) +
      ## Basis axes text labels
      suppressWarnings( ## Suppress for unused aes "frame".
        ggplot2::geom_text(
          data = basis_slides, 
          mapping = ggplot2::aes(x = x, y = y, 
                                 frame = slide, label = lab),
          colour = axes_col, size = 6, vjust = "outward", hjust = "outward")
      )
  }
  
  gg + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
}

app_oblique_frame <-
  function(basis        = NULL,
           data         = NULL,
           manip_var    = NULL,
           theta        = 0,
           phi          = 0,
           lab          = NULL,
           rescale_data = FALSE,
           ...) {
    
    if (is.null(basis) & !is.null(data)) {
      message("NULL basis passed. Initializing random basis.")
      basis <- tourr::basis_random(n = ncol(data))
    }
    
    p <- nrow(basis)
    m_sp <- create_manip_space(basis, manip_var)
    r_m_sp <- rotate_manip_space(manip_space = m_sp, theta, phi)
    
    basis_slides <- cbind(as.data.frame(r_m_sp), slide = 1)
    colnames(basis_slides) <- c("x", "y", "z", "slide")
    if(!is.null(data)){
      if (rescale_data) {data <- tourr::rescale(data)}
      data_slides  <- cbind(as.data.frame(data %*% r_m_sp), slide = 1)
      data_slides[, 1] <- scale(data_slides[, 1], scale = FALSE)
      data_slides[, 2] <- scale(data_slides[, 2], scale = FALSE)
      colnames(data_slides) <- c("x", "y", "z", "slide")
    }
    
    ## Add labels, attribute, and list
    basis_slides$lab <- 
      if(!is.null(lab)){
        rep(lab, nrow(basis_slides) / length(lab))
      } else {
        if(!is.null(data)) {abbreviate(colnames(data), 3)
        } else {paste0("V", 1:p)}
      }
    
    attr(basis_slides, "manip_var") <- manip_var
    
    slide <- if(!is.null(data)) {
      list(basis_slides = basis_slides, data_slides = data_slides)
    } else list(basis_slides = basis_slides)
    
    gg <- app_render_(slides = slide, ...) +
      ggplot2::coord_fixed() +
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
    
    return(gg)
  }

app_set_axes_position <- function(x, axes) {
  if (length(x) == 1) {x <- data.frame(x = x, y = x)}
  if (ncol(x) != 2) browser()
  position <- match.arg(axes, c("center", "bottomleft", "off", "left"))
  if (position == "off") return()
  if (position == "center") {
    scale <- 2 / 3
    x_off <- y_off <- 0
  } else if (position == "bottomleft") {
    scale <- 1 / 4
    x_off <- y_off <- -2 / 3
  } else if (position == "left") {
    scale <- 2 / 3
    x_off <- -5 / 3 
    y_off <- 0
  }
  
  ret <- as.data.frame(scale * x)
  ret[, 1] <- ret[, 1] + x_off
  ret[, 2] <- ret[, 2] + y_off
  return(ret)
}

app_vect2str <- function(vect){
  if (length(vect) == 0) return("<none>")
  .vect <- paste("V", vect)
  paste0(.vect,  collapse = ", ")
}

app_html_red <- function(string){
  paste0("<h3><span style='color:red'>", string, "</span></h3>")
}

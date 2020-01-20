### Global init for each factor variant.


##### App static global.r -----
### Initialization 
library("ggplot2")
library("spinifex")
library("shiny")
library("tidyr")
library("dplyr")
library("plotly")
library("GGally")
library("lubridate") # For timer
library("loggit")    # For logging

this_factor_id <- 1 # between 1 and 3 ## SET GROUP HERE
f_ls <- c("pca", "grand", "manual") # factor list
num_latin_sq <- rbind(c(1, 2, 3), # ~ grp 1; "pca", "grand", "manual"
                      c(2, 3, 1), # ~ grp 2; "grand", "manual", "pca"
                      c(3, 1, 2)  # ~ grp 3; "manual", "pca", "grand"
)
this_factor_num_order <- num_latin_sq[this_factor_id, ]
this_factor_order     <- f_ls[this_factor_num_order]


log_base <- paste0("log_", this_factor_id, "_", Sys.info()[4], "_")
log_num  <- 1
log_name <- sprintf(paste0(log_base, "%03d"), log_num)
log_file <- paste0(log_name, ".json")
while (file.exists(log_file)){ # Find an unused log number
  log_name <- sprintf(paste0(log_base, "%03d"), log_num)
  log_file <- paste0(log_name, ".json")
  log_num  <- log_num + 1
}

is_logging <- FALSE # init
### Logging
## https://www.r-bloggers.com/adding-logging-to-a-shiny-app-with-loggit/
## use: loggit("INFO", "<main msg>", "<detail>")
## Uncomment the following line to apply logging
# setLogFile(log_file); is_logging <- TRUE


### Required inputs -----
# tasks
s_task_id <- c("n", "p")
s_difficulty <- c("easy", "medium", "hard")
s_task_prompts <- c("How many clusters do you see?",
                    "Rate the relative importance of each variable in terms of 
                    distinugishing between the given clusters.")
s_task2_questions <- c("Very important distinguishing clusters 'a' from 'b'",
                       "Somewhat important distinguishing clusters 'a' from 'b'",
                       "Very important distinguishing clusters 'b' from 'c'",
                       "Somewhat important distinguishing clusters 'b' from 'c'")
s_sim_num  <- as.character(101:118)
sim_train1 <- readRDS("../simulation/simulation_data_train1.rds") # p = 6, pnoise = 2, cl = 3 
sim_train2 <- readRDS("../simulation/simulation_data_train2.rds") # p = 6, pnoise = 2, cl = 3
s_train <- list(sim_train1, sim_train2)
s_dat <- list()
for (i in 1:length(s_sim_num)) {
  s_dat[[i]] <- readRDS(
    paste0("../simulation/simulation_data", s_sim_num[i], ".rds")
  )
}

tpath_train1 <- readRDS("../simulation/grand_tpath_train1.rds") # p = 6, pnoise = 2, cl = 3 
tpath_train2 <- readRDS("../simulation/grand_tpath_train2.rds") # p = 6, pnoise = 2, cl = 3
s_tpath_train <- list(tpath_train1, tpath_train2)
s_tpath <- list()
for (i in 1:length(s_sim_num)) {
  s_tpath[[i]] <- readRDS(
    paste0("../simulation/grand_tpath_", s_sim_num[i], ".rds")
  )
}

# survey
s_survey_questions <- c("What gender are you?",
                        "What age are you?",
                        "What is your highest level of completed education?",
                        "I am experienced with data visualization.",
                        "I have education in multivariate statistical analysis.",
                        rep(c("I was already familiar with this visualization.",
                              "I found this visualization easy to use.",
                              "I felt confident in my answer with this visualization.",
                              "I liked using this visualization."), 3)
)


### Variable initialization -----
n_trainings        <- length(s_train)            # ~2
n_factors          <- length(f_ls)               # ~3
n_tasks            <- length(s_task_id)          # ~2
n_task2_questions  <- length(s_task2_questions)  # ~4
n_difficulty       <- length(s_difficulty)       # ~3
n_blocks           <- 3 #length(s_dat) / (n_tasks * n_factors) # 18/(2*3) = 3
n_survey_questions <- length(s_survey_questions) # ~17

PC_cap <- 4

s_taskblock_id <- paste0(rep(s_task_id, each = n_blocks), rep(1:n_blocks, n_tasks))
# intro is pg 1; video intro is pg 2
training_start <- 3
# ~ 9, pg 2:8 is training; (start on ui, 2x2 for tasks, splash)
task_start     <- (training_start + n_trainings * n_tasks + 1) + 1
# ~ 27, 9 + 3 * 3 * 2
survey_start   <- task_start + n_factors * n_blocks * n_tasks

### header_ui -----
header_ui <- fluidPage(
  titlePanel("Multivariate data visualization study"),
  conditionalPanel( 
    condition = "output.ui_section == 'training' && output.second_training == 'ask'",
    checkboxInput("second_training", "Do you want another training set?", 
                  value = FALSE)
  ),
  conditionalPanel(
    condition = "output.pg_num < 27",
    actionButton("next_pg_button", "Next page")
  )
)

### sidebar_ui ----
sidebar_ui <- conditionalPanel(
  condition = "output.ui_section == 'training' || output.ui_section == 'task'",
  sidebarPanel( 
    
    ### _Training text -----
    conditionalPanel(
      condition = "output.ui_section == 'training'",
      conditionalPanel( # interface familiarity 
        condition = "output.section_pg_num == 1",
        p("In this study, you will be working with 3 visualization techniques of
        multivariate data. Each one uses 2-dimensional projections created
        from different combinations of variables. The variable map (grey circle)
        shows the angle and magnitude that each variable contributes to the 
        projection."),
        p("Principal Component Analysis (PCA) is displayed first. Use the radio
          buttons on the left sidebar panel to select new components to be 
          displayed. Observe how the clusters and the variable line segments 
          move."),
        p("Now switch to the grand tour factor. Play the animation. Notice how 
          different clusters move as the variable contributions change. Drag
          the slider to select a different frame or pace."),
        p("Now try the manual tour. You can select which components are on 
          the axis. Using the drop-down, select the variable with the largest 
          line segment. Use the slider to change the variable's contribution. 
          Watch how the contributions and clusters move as a result. Select a 
          new pair of PC to reset the values."),
      ),
      conditionalPanel( # training task 1, pg 1
        condition = "output.task_num == 1",
        tags$b("Now the data points are not colored by their cluster. How
        many clusters do you see in this training set? 
        Make sure to use the controls and different factors.")
      ),
      conditionalPanel( # training task 1, pg 2 
        condition = "output.task_num == 2",
        tags$b("The data points are now colored by their cluster again. The 
               variable map (grey circle) shows the magnitude and direction
               that each variable contributes. Variables that have a large 
               contribution in line with two clusters are important to 
               distinguish them. However, you cannot rule out that variables 
               with a small contribution are unimportant.
               Use this information to identify which variables distinguish 
               the 2 clusters.")
      ),
      hr()
    ), ### end training text
    
    ### _Training control inputs -----
    # Factor selection
    conditionalPanel(condition = "output.ui_section == 'training'",
                     radioButtons(inputId = "factor", label = "Factor", 
                                  choices = f_ls, 
                                  selected = f_ls[1],
                                  inline = TRUE)
    ), # PCA axis selection
    conditionalPanel(
      condition = "(output.factor == 'pca' || output.factor == 'manual') || 
                  (output.ui_section == 'training' && input.factor != 'grand')",
      fluidRow(column(6, radioButtons(inputId = "x_axis", label = "x axis", 
                                      choices = paste0("PC", 1:4), selected = "PC1")),
               column(6, radioButtons(inputId = "y_axis", label = "y axis", 
                                      choices = paste0("PC", 1:4), selected = "PC2"))
      )
    ), # Manip var/ magnitude selection
    conditionalPanel(condition = "output.factor == 'manual' || 
                       (output.ui_section == 'training' && input.factor == 'manual')",
                     selectInput('manip_var', 'Manip var', "<none>"),
                     sliderInput("manip_slider", "Contribution",
                                 min = 0, max = 1, value = 0, step = .1)
    ), 
    
    
    
    ### _Task response input -----
    # Task 1
    conditionalPanel(condition = "(output.task_num == 1 || output.task_num == 2) && 
                                  output.factor != 'grand'", 
                     hr()
    ),
    conditionalPanel(condition = "output.task_num == 1",
                     tags$b(s_task_prompts[1]),
                     tags$br(),
                     numericInput("tsk1_ans", "",
                                  value = 0, min = 0, max = 10)
    ), # Task 2
    conditionalPanel(condition = "output.task_num == 2",
                     tags$b(s_task_prompts[2]),
                     tags$br(), br(),
                     checkboxGroupInput(
                       inputId = "tsk2_ans_very_ab", #tsk2_ans_cla_very
                       label   = s_task2_questions[1],
                       choices = "V1",
                       inline  = TRUE
                     ),
                     checkboxGroupInput(
                       inputId = "tsk2_ans_some_ab", #tsk2_ans_cla_very
                       label   = s_task2_questions[2],
                       choices = "V1",
                       inline  = TRUE
                     ),
                     hr(),
                     checkboxGroupInput(
                       inputId = "tsk2_ans_very_bc", #tsk2_ans_cla_very
                       label   = s_task2_questions[3],
                       choices = "V1",
                       inline  = TRUE
                     ),
                     checkboxGroupInput(
                       inputId = "tsk2_ans_some_bc", #tsk2_ans_cla_very
                       label   = s_task2_questions[4],
                       choices = "V1",
                       inline  = TRUE
                     )
    )
    
  )
) ### end sidebar_ui

##### init survey columns -----
col_p1 <- column(4, 
                 h3(this_factor_order[1]),
                 hr(),
                 h4(s_survey_questions[6]),
                 sliderInput("survey6",
                             label = div(style = 'width:300px;',
                                         div(style = 'float:left;', 'strongly disagree'),
                                         div(style = 'float:right;', 'strongly agree')),
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[7]),
                 sliderInput("survey7",
                             label = div(style = 'width:300px;',
                                         div(style = 'float:left;', 'strongly disagree'),
                                         div(style = 'float:right;', 'strongly agree')),
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[8]),
                 sliderInput("survey8",
                             label = div(style = 'width:300px;',
                                         div(style = 'float:left;', 'strongly disagree'),
                                         div(style = 'float:right;', 'strongly agree')),
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[9]),
                 sliderInput("survey9",
                             label = div(style = 'width:300px;',
                                         div(style = 'float:left;', 'strongly disagree'),
                                         div(style = 'float:right;', 'strongly agree')),
                             min = 1, max = 9, value = 5)
)
col_p2 <- column(4, 
                 h3(this_factor_order[2]),
                 hr(),
                 h4(s_survey_questions[10]),
                 sliderInput("survey10",
                             label = div(style = 'width:300px;',
                                         div(style = 'float:left;', 'strongly disagree'),
                                         div(style = 'float:right;', 'strongly agree')),
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[11]),
                 sliderInput("survey11",
                             label = div(style = 'width:300px;',
                                         div(style = 'float:left;', 'strongly disagree'),
                                         div(style = 'float:right;', 'strongly agree')),
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[12]),
                 sliderInput("survey12",
                             label = div(style = 'width:300px;',
                                         div(style = 'float:left;', 'strongly disagree'),
                                         div(style = 'float:right;', 'strongly agree')),
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[13]),
                 sliderInput("survey13",
                             label = div(style = 'width:300px;',
                                         div(style = 'float:left;', 'strongly disagree'),
                                         div(style = 'float:right;', 'strongly agree')),
                             min = 1, max = 9, value = 5)
)
col_p3 <- column(4, 
                 h3(this_factor_order[3]),
                 hr(),
                 h4(s_survey_questions[14]),
                 sliderInput("survey14",
                             label = div(style = 'width:300px;',
                                         div(style = 'float:left;', 'strongly disagree'),
                                         div(style = 'float:right;', 'strongly agree')),
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[15]),
                 sliderInput("survey15",
                             label = div(style = 'width:300px;',
                                         div(style = 'float:left;', 'strongly disagree'),
                                         div(style = 'float:right;', 'strongly agree')),
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[16]),
                 sliderInput("survey16",
                             label = div(style = 'width:300px;',
                                         div(style = 'float:left;', 'strongly disagree'),
                                         div(style = 'float:right;', 'strongly agree')),
                             min = 1, max = 9, value = 5),
                 h4(s_survey_questions[17]),
                 sliderInput("survey17",
                             label = div(style = 'width:300px;',
                                         div(style = 'float:left;', 'strongly disagree'),
                                         div(style = 'float:right;', 'strongly agree')),
                             min = 1, max = 9, value = 5)
)

##### main_ui -----
main_ui <- mainPanel(
  ### _Intro mainPanel -----
  conditionalPanel(
    condition = "output.ui_section == 'intro'",
    conditionalPanel(
      condition = "output.pg_num == 1", # First page
      h3("Welcome to the study")
      , br()
      , p("This a completely voluntary study that will take approximately 45-50 
          minutes to complete. If at any point you would like to stop, 
          please let the invigilator know.")
      , br()
      , p("You are helping to compare the effectiveness of different 
          multivariate data visualization techniques. 
          The study is structured as follows:")
      , p("Training -- questions encouraged")
      , tags$ul(
        tags$li("Video training: you will first watch a five minute video 
              explaining the techniques")
        , tags$li("Interface familiarity: you will get to explore the interface 
                for the different tasks, answer questions about the data, and 
                receive feedback")
      )
      , p("Evaluation, for each of the 3 visuals -- independent effort with no questions")
      , tags$ul(
        tags$li("Task 1 (x3 difficulties, 60 sec)")
        , tags$li("Task 2 (x3 difficulties, 180 sec)")
      )
      , p("Wrap up study")
      , tags$ul(
        tags$li("Complete survey")
        , tags$li("Save and exit from app")
        , tags$li("Collect a voucher for a free hot beverage on campus, from the invigilator.")
      )
      , p("We really appreciate your participation in this study.")
    ), # end first page
    conditionalPanel(
      condition = "output.pg_num == 2", # Video, second page
      h2("Video training"), tags$br(), tags$br(),
      p("Watch the following video before proceeding:"), tags$br(), 
      # Adding the 'a' tag to the sidebar linking external file
      p("Minimize the study and watch the training video."),
      #tags$a(href='training.mp4', target='blank', 'training video (4:17)'), 
      tags$br(), tags$br(), 
      p("If this link only contains audio let the invigilator know.")
    )  # end of video, second page
  ), # close conditionPanel -- intro section text
  
  ### _Training mainPanel -----
  conditionalPanel(
    condition = "output.ui_section == 'training'",
    conditionalPanel(condition = "output.section_pg_num == 1", # ui intro 
                     h2("Training -- interface")
    ),
    conditionalPanel(condition = "output.section_pg_num == 2",
                     h2("Training -- task 1")
    ),
    conditionalPanel(condition = "output.section_pg_num == 3",
                     h2("Training -- task 1, set 2")
    ),
    conditionalPanel(condition = "output.section_pg_num == 4",
                     h2("Training -- task 2")
    ),
    conditionalPanel(condition = "output.section_pg_num == 5",
                     h2("Training -- task 2, set 2")
    ),
    conditionalPanel( # splash page
      condition = "output.section_pg_num == 6",
      h1(), h1(), h1(),
      h1("Training complete, Great job!"),
      h4("Take a break and strech if you feel like it."),
      HTML("<h3><span style='color:red'>
          Keep in mind that we are evaluating the factors not your performance. 
          Don't worry if you don't fully understand the theory or find a task difficult.
           </span></h3>"),
      h4("Ask any final clarification questions. Then continue on to the 
        evaluation section. Task 1 is limited to 1 minute, and task 2 is limited
        to 3 minutes  (time displayed on top).")
    ),
    textOutput('stopwatch_disp'),
    hr()
  ), # close training section main panel text
  
  ### _Task mainPanel -----
  conditionalPanel(
    condition = "output.ui_section == 'task'",
    h2(textOutput('task_header')),
    textOutput('timer_disp'),
    hr()
  ), # close task section conditional panel title text
  
  ### _Plot mainPanel ----
  conditionalPanel( 
    condition = "(output.ui_section == 'training' && output.section_pg_num != 6)
      || output.ui_section == 'task'", # block_num == 6 is splash page.
    htmlOutput("plot_msg"),
    plotOutput("pca_plot", height = "auto"),
    plotOutput("mtour_plot", height = "auto"),
    plotlyOutput("gtour_plot", height = "auto")
  ), # close plot conditional panel
  
  ### _Survey mainPanel -----
  conditionalPanel(
    condition = "output.ui_section == 'survey'",
    selectInput("survey1", label = s_survey_questions[1], 
                choices = c("decline to answer",
                            "female",
                            "male",
                            "intergender/other")
    ),
    selectInput("survey2", label = s_survey_questions[2], 
                choices = c("decline to answer",
                            "19 or younger",
                            "20 to 29",
                            "30 to 39",
                            "40 or older")
    ),
    selectInput("survey3", label = s_survey_questions[3], 
                choices = c("decline to answer",
                            "High school",
                            "Undergraduate",
                            "Honors, masters, mba", 
                            "Doctorate")
    ),
    h3("How much do you agree with the following statements?"),
    h4(s_survey_questions[4]),
    sliderInput("survey4",
                label = div(style = 'width:300px;',
                            div(style = 'float:left;', 'strongly disagree'),
                            div(style = 'float:right;', 'strongly agree')),
                min = 1, max = 9, value = 5),
    h4(s_survey_questions[5]),
    sliderInput("survey5",
                label = div(style = 'width:300px;',
                            div(style = 'float:left;', 'strongly disagree'),
                            div(style = 'float:right;', 'strongly agree')),
                min = 1, max = 9, value = 5),
    fluidRow(col_p1, col_p2, col_p3),
    hr(),
    actionButton("save_ans", "save responses"),
    htmlOutput("save_msg"),
    conditionalPanel(
      condition = "output.is_saved == 1",
      h3("Thank you for participating!"),
      br(),
      h4("Let the invigilator know you have completed the study and have a good day.")
    )
  ) # close survey condition panel 
  
) # close mainPanel()


##### UI, combine panels -----
ui <- fluidPage(header_ui,
                sidebar_ui,
                main_ui
                , verbatimTextOutput("dev_msg")
                , actionButton("browser", "browser()")
                , tableOutput("ans_tbl")
)

### onStop -----
onStop(function(){
  cat("(onSessionEnded ran) \n")
  loggit("INFO", "Spinifex study app has stopped.")
  
  ### Try to autosave if not saved and logging.
  # rv$ is out of scope at this point.
})

##### App local functions
app_render_ <- function(slides, # paste over spinifex render to add size
                        axes = "left",
                        alpha = 1,
                        cluster = NULL,
                        ...) {
  # Initialize
  if (length(slides) == 2)
    data_slides <- data.frame(slides[[2]])
  basis_slides  <- data.frame(slides[[1]])
  manip_var     <- attributes(slides$basis_slides)$manip_var
  n_slides      <- max(basis_slides$slide)
  p             <- nrow(basis_slides) / n_slides
  d             <- ncol(basis_slides) - 2
  angle         <- seq(0, 2 * pi, length = 360)
  circ          <- data.frame(x = cos(angle), y = sin(angle))
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
    ggplot2::theme(panel.grid.major = element_blank(), # no grid lines
                   panel.grid.minor = element_blank(), # no grid lines
                   axis.text.x = element_blank(),      # no axis marks
                   axis.text.y = element_blank(),      # no axis marks
                   axis.title.x = element_blank(),     # no axis titles for gtour
                   axis.title.y = element_blank(),     # no axis titles for gtour
                   aspect.ratio = y_range / x_range, 
                   legend.box.background = element_rect(),
                   legend.title = element_text(size = 18, face = "bold"),
                   legend.text  = element_text(size = 18, face = "bold")) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::xlim(x_min, x_max) +
    ggplot2::ylim(y_min, y_max) +
    ## Projected data points
    suppressWarnings( # Suppress for unused aes "frame".
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
      suppressWarnings( # Suppress for unused aes "frame".
        ggplot2::geom_segment( 
          data = basis_slides, size = axes_siz, colour = axes_col,
          mapping = ggplot2::aes(x = x,
                                 y = y, 
                                 xend = zero[, 1], yend = zero[, 2], 
                                 frame = slide)
        )
      ) +
      ## Basis axes text labels
      suppressWarnings( # Suppress for unused aes "frame".
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
    
    # Add labels, attribute, and list
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
  #stopifnot(ncol(x) == 2)
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

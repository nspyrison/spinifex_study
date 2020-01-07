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

### Logging
## https://www.r-bloggers.com/adding-logging-to-a-shiny-app-with-loggit/
## use: loggit("INFO", "<main msg>", "<detail>")
## Uncomment the following line to apply logging
setLogFile(log_file)
loggit("INFO", "app has started", "spinifex_study")

this_factor_id <- 1 # between 1 and 3 ## SET GROUP HERE
f_ls <- c("pca", "grand", "manual") # factor list
latin_sq <- rbind(c(f_ls[1], f_ls[2], f_ls[3]), # ~ grp 1
                  c(f_ls[2], f_ls[3], f_ls[1]), # ~ grp 2
                  c(f_ls[3], f_ls[1], f_ls[2])  # ~ grp 2 
)
this_factor_order <- latin_sq[this_factor_id, ]

log_base <- paste0("log_factorid", this_factor_id, "_")
log_num  <- 1
log_name <- sprintf(paste0(log_base, "%03d"), log_num)
log_file <- paste0(log_name, ".json")
while (file.exists(log_file)){ # Find an unused log number
  log_name <- sprintf(paste0(log_base, "%03d"), log_num)
  log_file <- paste0(log_name, ".json")
  log_num  <- log_num + 1
}


### Required inputs -----
# blocks
s_block_id <- c("n", "d")
s_block_questions <- c("How many clusters exist?",
                       "Rate the importance of each variable in terms of 
                       distinugishing the given cluster.")
# reps (simulations)
s_sim_num  <- as.character(101:118)
sim_train1 <- readRDS("../simulation/simulation_data119.rds") # p = 6, pnoise = 2, cl = 3 
sim_train2 <- readRDS("../simulation/simulation_data120.rds") # p = 6, pnoise = 2, cl = 3 
s_train <- list(sim_train1, sim_train2)
s_dat <- list()
for (i in 1:length(s_sim_num)) {
  s_dat[[i]] <- readRDS(
    paste0("../simulation/simulation_data", s_sim_num[i], ".rds")
  )
}

# survey
s_survey_questions <- c("What gender are you?",
                        "What age are you?",
                        "What is your highest level of completed education?",
                        "I am experienced with data visualization.",
                        "I have education in multivariate statistical analysis.",
                        rep("I was already familiar with this visualization.", 3),
                        rep("I found this visualization easy to use.", 3),
                        rep("I felt confident in my answer with this visualization.", 3),
                        rep("I liked using this visualization.", 3))

### Variable initialization -----
n_trainings        <- length(s_train)    # ~2
n_factors          <- length(f_ls)       # ~3
n_blocks           <- length(s_block_id) # ~2
n_reps             <- length(s_dat) / (n_blocks * n_factors) # ~3
n_survey_questions <- length(s_survey_questions) # ~10

s_blockrep_id  <- paste0(rep(s_block_id, each = n_reps), rep(1:n_reps, n_blocks))
training_start <- 2 # pg 1 is intro, pg 2:7 is training, 1 for ui, 2, for blocks
task_start     <- (training_start - 1) + 1 + 2 * n_blocks + 1 
# ~ 8, pg 2 + 1 ui training + 2 trainings across 2 blocks, 1 splash screen  
survey_start   <- task_start + 3 * (n_reps * n_blocks)  # ~ 25, last pg is survey 

### header_ui -----
header_ui <- fluidPage(
  titlePanel("Multivariate data visualization study"),
  conditionalPanel( 
    condition = "output.ui_section == 'training' && output.second_training == 'ask'",
    checkboxInput("second_training", "Do you want another training set?", 
                  value = FALSE)
  ),
  conditionalPanel(
    condition = "output.pg_num < 22",
    actionButton("next_pg_button", "Next page")
  )
)

### sidebar_ui ----
sidebar_ui <- conditionalPanel(
  condition = "output.ui_section == 'training' || output.ui_section == 'task'",
  sidebarPanel(
    conditionalPanel(condition = "output.ui_section == 'training'",
                     radioButtons(inputId = "factor", label = "Visual", 
                                  choices = this_factor_order, 
                                  selected = this_factor_order[1],
                                  inline = TRUE)
    ),
    conditionalPanel(condition = "output.factor != 'grand' || 
                       (output.ui_section == 'training' && input.factor != 'grand')",
                     fluidRow(column(6, radioButtons(inputId = "x_axis", label = "x axis", choices = "PC1")),
                              column(6, radioButtons(inputId = "y_axis", label = "y axis", choices = "PC2"))
                     )
    ),
    conditionalPanel(condition = "output.factor == 'manual' || 
                       (output.ui_section == 'training' && input.factor == 'manual')",
                     selectInput('manip_var', 'Manip var', "<none>"),
                     sliderInput("manip_slider", "Contribution",
                                 min = 0, max = 1, value = 0, step = .1)
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
    )
  )
) ### end sidebar_ui

##### main_ui -----
main_ui <- mainPanel(
  ### _Intro mainPanel -----
  conditionalPanel(
    condition = "output.ui_section == 'intro'",
    h3("Welcome to the study")
    , br()
    , p("This a completely voluntary study that will take approximately 45-50 
          minutes to complete. If at any point you would like to stop, 
          please let the proctor know.")
    , br()
    , p("You are helping to compare the effectiveness of different 
          multivariate data visualization techniques. 
          The study is structured as follows:")
    , p("Training -- questions encouraged")
    , tags$ul(
      tags$li("Video training: you will first watch a five minute video explaining the techniques")
      , tags$li("Interface familiarity: you will get to explore the interface for the different tasks, answer questions about the data, and receive feedback")
    )
    , p("Evaluation, for each of the 3 visuals -- independent effort with no questions")
    , tags$ul(
      tags$li(paste0("Task 1 (x3 reps, 60 sec) "))
      , tags$li(paste0("Task 2 (x3 reps, 180 sec) "))
    )
    , p("Wrap up study")
    , tags$ul(
      tags$li("Complete survey")
      , tags$li("Save and exit from app")
      , tags$li("Collect a 
          voucher for a free hot beverage on campus, from the proctor.")
    )
    , p("We really appreciate your participation in this study.")
  ), # close conditionPanel -- intro section text
  
  ### _Training mainPanel -----
  conditionalPanel(
    condition = "output.ui_section == 'training'",
    conditionalPanel(condition = "output.rep_num == 1", # ui intro 
                     h2("Training -- interface")
    ),
    conditionalPanel(condition = "output.rep_num == 2",
                     h2("Training -- task 1")
    ),
    conditionalPanel(condition = "output.rep_num == 3",
                     h2("Training -- task 1 training 2")
    ),
    conditionalPanel(condition = "output.rep_num == 4",
                     h2("Training -- task 2")
    ),
    conditionalPanel(condition = "output.rep_num == 5",
                     h2("Training -- task 2 training 2")
    ), # splash page is 6, no header
    conditionalPanel( # interface familiarity 
      condition = "output.rep_num == 1", # rep_num == 1 is ui familiarity
      p("This data has 6 variables. Principal Component Analysis (PCA) defines 
        new axes components (as linear combinations of the original variable),
        ordered by the amount of variation they explain. The plot below displays
        the data for the components selected on the sidebar to the left."),
      p("Take time to familiarize yourself with the controls and feel free to 
          ask any questions. During the evaluation section, you will have 2 
          minutes to explore the data, responding as accurately and quickly 
          as possible.")
    ),
    conditionalPanel( # training block 1, pg 1
      condition = "output.block_num == 1",
      tags$b("The first task is to estimate the number clusters in the data. 
          Click on the radio buttons on the sidebar to select different PC 
          combinations to better understand the clustering of the data. 
          When you are ready to enter the number of clusters on the sidebar then
          click the 'Next page' button below.")
    ),
    conditionalPanel( # training block 1, pg 2 
      condition = "output.block_num == 2",
      tags$b("The second task is to rate each variables importance for 
        distinguishing the listed cluster. The points have colored and shape 
        assigned by their cluster. The variable map (grey circle) on the display 
        shows the direction and magnitude that each variable contributes to the 
        current axes. Use the variable map to identify the variables that 
        distinguish between clusters. Look at several components to rate
        the top four variables that help distinguish clusters.")
      ##TODO: Move to answer text.
      # Consider cluster 'a' (green circles). Variables 2, 4, and 6 have 
      # relatively large magnitudes and are in directions that help distinguish
      # the purple squares (V4) and the orange triangles (V2 and V6). 
      # List V2, V4, and V6 as very important for distinguishing cluster 'a'.
      # Remember the axes can be changed to look at the data from another 
      # perspective. Look at the other variables and see if they 
      # contribute in separate directions. Continue to the next page 
      # when you are content
    ),
    ##TODO: add text for block 2, task 1 and 2 here
    conditionalPanel( # splash page
      condition = "output.rep_num == 6",
      h1(),h1(),h1(),
      h1("Training complete, Great job!"),
      h3("Ask any final clarification questions. Then continue on to the 
        evaluation section, each task is now limited to 2 minutes (time 
           displayed on top).")
    ),
  ),# close training section main panel text
  
  ### _Task mainPanel -----
  conditionalPanel(
    condition = "output.ui_section == 'task'",
    conditionalPanel(condition = "output.block_num == 1 && output.factor == 'pca'",
                     h2("Evaluation -- task 1 (factor: pca)")),
    conditionalPanel(condition = "output.block_num == 1 && output.factor == 'grand'",
                     h2("Evaluation -- task 1 (factor: grand tour)")),
    conditionalPanel(condition = "output.block_num == 1 && output.factor == 'manual'",
                     h2("Evaluation -- task 1 (factor: manual tour)")),
    conditionalPanel(condition = "output.block_num == 2 && output.factor == 'pca'",
                     h2("Evaluation -- task 2 (factor: pca)")),
    conditionalPanel(condition = "output.block_num == 2 && output.factor == 'grand'",
                     h2("Evaluation -- task 2 (factor: grand tour)")),
    conditionalPanel(condition = "output.block_num == 2 && output.factor == 'manual'",
                     h2("Evaluation -- task 2 (factor: manual tour)")),
    textOutput('timer_disp')
  ), # close task section conditional panel title text
  
  ### _Plot mainPanel
  conditionalPanel( 
    condition = "(output.ui_section == 'training' && output.rep_num != 6)
      || output.ui_section == 'task'", # rep_num == 6 is splash page.
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
    h3("How much do you agree with the following statments?"),
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



##### UI, combine panels -----
ui <- fluidPage(
  header_ui,
  sidebar_ui,
  main_ui
  # , verbatimTextOutput("dev_msg")
  # , actionButton("browser", "browser()")
  # , tableOutput("ans_tbl")
)


app_render_ <- function(slides, # paste over spinifex render to add size
                        manip_col = "blue",
                        axes = "center",
                        alpha = 1,
                        cluster,
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
    zero         <- set_axes_position(0, axes)
    circ         <- set_axes_position(circ, axes)
    basis_slides <- data.frame(set_axes_position(basis_slides[, 1:d], axes), 
                               basis_slides[, (d+1):ncol(basis_slides)])
  }
  ## manip var axes asethetics
  axes_col <- "red"
  axes_siz <- 0.3
  
  xy_min <- min(circ[, 1:2], data_slides[, 1:2]) - .1
  xy_max <- max(circ[, 1:2], data_slides[, 1:2]) + .1
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
                   legend.box.background = element_rect(),
                   legend.title = element_text(size = 18, face = "bold"),
                   legend.text  = element_text(size = 18, face = "bold")) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::xlim(xy_min, xy_max) +
    ggplot2::ylim(xy_min, xy_max) +
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
                                 xend = zero, yend = zero, 
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
    
    gg
  }
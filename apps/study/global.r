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
library("shinyjs")   ## help with handling conditionalPanels.
library("lubridate") ## For timer
library("loggit")    ## For logging
library("here")      ## Fixing base dir.
set.seed(20200927)   ## if tourr starts using seeds


#### Logging
## browseURL("https://www.r-bloggers.com/adding-logging-to-a-shiny-app-with-loggit/")
## use: loggit("INFO", "<main msg>", "<detail>")
## Uncomment the following line to apply logging
do_log            <- FALSE
do_disp_dev_tools <- TRUE


#### Simulated data series,
## "series" or iteration of data to look at. Should be an even hundred
height_px <- 638L
width_px <- 1000L
sim_series <- 300
cat(do_log)

## Set log file, finding the first unused number, will need to write to a google sheet or otherwise store a file.
log_base <- paste0("log_", Sys.info()[4], "_")
log_num  <- 1
log_file <- ""
log_name <- "DUMMY"

if(do_log == TRUE){
  log_name <- sprintf(paste0(log_base, "%03d"), log_num)
  log_file <- paste0(log_name, ".json")
  while (file.exists(log_file)){ ## Find an unused log number
    log_num  <- log_num + 1
    log_name <- sprintf(paste0(log_base, "%03d"), log_num)
    log_file <- paste0(log_name, ".json")
  }
  set_logfile(log_file)
}else{ ## When do_log == F
  log_num  <- sample(1:3, 1)
  log_name <- "<NOT LOGGING>"
  log_file <- "Logging is off! Log and responses not being recorded."
}
cat("do_log, log_file: ", do_log, log_file, " /n")

## Set group (factor order) based on log number mod 3
this_group <- 1 + (log_num - 1) %% 3 ## Expects [1,2,3]
fct_ord_latin_sq <- rbind(c(1, 2, 3), ## ~ grp 1; "pca", "grand", "radial"
                          c(2, 3, 1), ## ~ grp 2; "grand", "radial", "pca"
                          c(3, 1, 2)  ## ~ grp 3; "radial", "pca", "grand"
)
fct_nm_vect <- c("pca", "grand", "radial") ## factor list
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
                     paste0("Sys.info()[1:5]: ", this_Sys.info)
)
if(do_log == TRUE) loggit("INFO", "=====Spinifex study app start.=====")
cat(context_msg)

onStop(function(){
  cat(context_msg)
  if(do_log == TRUE){
    loggit("INFO", "=====Spinifex study app stop.=====")
    set_logfile(logfile = NULL, confirm = TRUE)
  }
  ## Try to autosave if not saved and do_log == T?
  #### note that rv$resp_tbl is out of scope to the global file.
})


##### Required inputs -----
## Task, and block (difficulty)
s_blocks        <- c("baseline", "corNoise", "mnComb")
s_task_question <- c("Which variables contribute more than average to the cluster seperation between 'a' and 'b'.")

## Survey questions; n = 21 = 9 + 12
s_survey_questions <- c("What sex are you?",
                        "What age group do you belong to?",
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

## Load training data and tour paths
root <- ("~/R/spinifex_study/apps/data")# here("apps/data/") ## Filepaths cannot be too long....
factor_model <- c("baseLn_EEE", "baseLn_EEV", "baseLn_banana",
                  "corNoise_EEE", "corNoise_EEV", "corNoise_banana",
                  "mnComb_EEE", "mnComb_EEV", "mnComb_banana")
sim_fps       <- paste0(root, "/", factor_model, ".rda")
tpath_fps     <- paste0(root, "/tpath_", factor_model, ".rda")
MMP_clSep_fps <- paste0(root, "/MMP_clSep_", factor_model, ".rda")
for(i in 1:length(factor_model)){
  ## Load sims and tpaths by the obj name stored in .rda files.
  load(sim_fps[i])
  load(tpath_fps[i])
  load(MMP_clSep_fps[i])
}

## Load data and tour paths for task eval
# dat_len <- 12L
# s_dat <- s_tpath <- list()
# for (i in 1:dat_len){
#   s_dat[[i]] <- readRDS(
#     here::here(paste0("apps/data/simulation_data", sim_series + i, ".rds"))
#   )
#   s_tpath[[i]] <- readRDS(
#     here::here(paste0("apps/data/grand_tpath", sim_series + i, ".rds"))
#   )
# }



##### Global variable initialization -----
n_trainings        <- 4 #length(s_t_dat)         ## ~4
n_factors          <- length(fct_nm_vect)        ## ~3
n_blocks           <- length(s_blocks)           ## ~3
n_survey_questions <- length(s_survey_questions) ## ~21

PC_cap             <- 4 ## Number of principal components to choose from.
pal                <- "Dark2"

#### Define section start pages,
## may need radial changes when changing section sizes
## intro is pg 1; video training is pg 2
training_start_pg <- 3L
task_start_pg <- (training_start_pg + 2 + 1) + 1
## ~ pg7;(3+2+1+1; train_st, 2 task sets, splash pg, start on new pg)
survey_start_pg <- task_start_pg + n_factors * n_blocks + 1
## ~ pg14, (7+3*3+1; task_st, 3*3 factor*block, start on new pg)

##### UI START -----
### header_ui -----
header_ui <- fluidPage(
  titlePanel("Multivariate data visualization study"),
  actionButton("next_pg_button", "Next page")
)

##### sidebar_ui ----
sidebar_ui <- conditionalPanel(
  condition = "output.section_nm == 'training' || output.section_nm == 'task'",
  sidebarPanel(width = 3,
    ##### _Training text -----
    conditionalPanel(
      condition = "output.section_nm == 'training'",
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
        p("Change to the radial tour. You can select which components are on
          the axes. Using the drop-down, select the variable with the largest
          line segment. Use the slider to change the variable's contribution.
          Watch how the contributions and clusters move as a result. Select a
          change the y-axis to PC3 and back, notice that this resets the
          projection."),
      ), ## Close coditionalPanel()
      conditionalPanel( ## Rraining task 1, pg 2
        condition = "output.task == 2",
        strong("The data points are colored by their cluster again.
               Variables that have a large
               contribution in line with two clusters are important to
               distinguish them. However, you cannot rule out that variables
               with a small contribution are unimportant.
               Use this information to identify which variables distinguish
               the 2 clusters.")
      ), ## Close coditionalPanel()
      hr()
    ), ### Close training text coditionalPanel()
    
    ##### _Training control inputs -----
    ## Factor selection
    conditionalPanel(condition = "output.section_nm == 'training' && output.section_pg < 6",
                     radioButtons(inputId = "factor", label = "Factor",
                                  choices = fct_nm_vect,
                                  selected = fct_nm_vect[1],
                                  inline = TRUE),
                     fluidRow(column(6, radioButtons(inputId = "simFactor", label = "Simulation factor",
                                                     choices = list("Baseline" = "baseLn",
                                                                    "Correlated noise dimensions" = "corNoise", 
                                                                    "Mean diff. in combination of dim." = "mnComb"), 
                                                     selected = "baseLn")),
                              column(6, radioButtons(inputId = "simModel", label = "Model",
                                                     choices = c("EEE", "EEV", "banana"), selected = "EEE"))
                     )
    ), 
    conditionalPanel(
      ## PCA axis selection
      condition = "(output.factor_nm == 'pca') ||
                  (output.section_nm == 'training' && input.factor == 'pca')",
      fluidRow(column(6, radioButtons(inputId = "x_axis", label = "x axis",
                                      choices = paste0("PC", 1:PC_cap), selected =  "PC1")),
               column(6, radioButtons(inputId = "y_axis", label = "y axis",
                                      choices = paste0("PC", 1:PC_cap), selected =  "PC2"))
      )
    ),
    ## Grand restart button
    conditionalPanel(
      condition = "(output.factor_nm == 'grand') ||
                  (output.section_nm == 'training' && input.factor == 'grand')"#,
      #actionButton("grand_restart", "Restart grand tour")
    ),
    ## Radial mvar dropdown selection
    conditionalPanel(
      condition = "(output.factor_nm == 'radial') ||
                  (output.section_nm == 'training' && input.factor == 'radial')",
      radioButtons(inputId = "manip_var_nm", label = "Manip variable:",
                  choices =  "V1", selected = "V1")
    ), ## Close conditionalPanel()
    
    ##### _Task response input -----
    ## Task 2
    conditionalPanel(
      condition = "(output.plot_active == true) ", 
      checkboxGroupInput(
        inputId = "task_response",
        label   = s_task_question,
        choices = "V1",
        inline  = TRUE
      )
    ),
    ## Dev_tool disp
    conditionalPanel(
      condition = "output.plot_active == true", #" &&do_disp_dev_tools == true",
      p("___"),
      p("response: "),
      p("MMP ClSep: "),
      p("Variable marks: "),
      p("Task marks: ")
    ) ## Close sidebarPanel()
  ) ## Close conditionalPanel(), end sidebar_ui section
)
  

##### Init survey columns -----
surv_lab <- HTML("<div style=\"width:300px;\">
                    <div style=\"float:left;\">strongly disagree</div>
                    <div style=\"float:right;\">strongly agree</div>
                  </div>")
s_fct_start <- 9
col_p1 <- column(4,
                 h3(this_factor_nm_order[1]),
                 hr(),
                 h4(s_survey_questions[s_fct_start + 1]),
                 sliderInput(paste0("survey", s_fct_start + 1),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(s_survey_questions[s_fct_start + 2]),
                 sliderInput(paste0("survey", s_fct_start + 2),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(s_survey_questions[s_fct_start + 3]),
                 sliderInput(paste0("survey", s_fct_start + 3),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(s_survey_questions[s_fct_start + 4]),
                 sliderInput(paste0("survey", s_fct_start + 4),
                             label = surv_lab, min = 1, max = 9, value = 5)
)

col_p2 <- column(4,
                 h3(this_factor_nm_order[2]),
                 hr(),
                 h4(s_survey_questions[s_fct_start + 5]),
                 sliderInput(paste0("survey", s_fct_start + 5),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(s_survey_questions[s_fct_start + 6]),
                 sliderInput(paste0("survey", s_fct_start + 6),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(s_survey_questions[s_fct_start + 7]),
                 sliderInput(paste0("survey", s_fct_start + 7),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(s_survey_questions[s_fct_start + 8]),
                 sliderInput(paste0("survey", s_fct_start + 8),
                             label = surv_lab, min = 1, max = 9, value = 5)
)
col_p3 <- column(4,
                 h3(this_factor_nm_order[3]),
                 hr(),
                 h4(s_survey_questions[s_fct_start + 9]),
                 sliderInput(paste0("survey", s_fct_start + 9),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(s_survey_questions[s_fct_start + 10]),
                 sliderInput(paste0("survey", s_fct_start + 10),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(s_survey_questions[s_fct_start + 11]),
                 sliderInput(paste0("survey", s_fct_start + 11),
                             label = surv_lab, min = 1, max = 9, value = 5),
                 h4(s_survey_questions[s_fct_start + 12]),
                 sliderInput(paste0("survey", s_fct_start + 12),
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
    condition = "(output.section_nm == 'training' && output.section_pg != 6) ||
      output.section_nm == 'task'", ## output.section_pg == 6 is splash page.
    htmlOutput("plot_msg"),
    plotOutput("pca_plot", height = "auto"),
    plotlyOutput("radial_plot", height = "auto"),
    plotlyOutput("grand_plot", height = "auto")
  ), ## Close plot conditional panel

  ### _Survey mainPanel -----
  conditionalPanel(
    condition = "output.section_nm == 'survey'",
    conditionalPanel(
      condition = "output.is_saved == 0",
      selectInput("survey1", label = s_survey_questions[1],
                  choices = c("decline to answer", "female", "male",
                              "intersex, non-binary, or other")
      ),
      selectInput("survey2", label = s_survey_questions[2],
                  choices = c("decline to answer", "19 or younger", "20 to 29",
                              "30 to 39", "40 or older")
      ),
      selectInput("survey3", label = s_survey_questions[3],
                  choices = c("decline to answer", "fluent",
                              "conversational", "less than conversational")
      ),
      selectInput("survey4", label = s_survey_questions[4],
                  choices = c("decline to answer", "high school",
                              "undergraduate", "honors, masters, mba", "doctorate")
      ),
      h3("To what extent do you agree with the following statements?"),
      strong(s_survey_questions[5]),
      sliderInput("survey5", label = surv_lab,
                  min = 1, max = 9, value = 5),
      strong(s_survey_questions[6]),
      sliderInput("survey6",label = surv_lab,
                  min = 1, max = 9, value = 5),
      strong(s_survey_questions[7]),
      sliderInput("survey7",label = surv_lab,
                  min = 1, max = 9, value = 5),
      strong(s_survey_questions[8]),
      sliderInput("survey8",label = surv_lab,
                  min = 1, max = 9, value = 5),
      strong(s_survey_questions[9]),
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


##### UI, combine panels -----
ui <- fluidPage(useShinyjs(), ## Required in ui to use shinyjs.
                header_ui,
                sidebar_ui,
                #TODO: PART OF DISPLAYING MAIN_UD cause JS NOT TO EVAL.
                main_ui,
                ## Dev tools, displays nothing when do_disp_dev_tools == FALSE:
                actionButton("browser", "browser()"),
                textOutput("dev_msg"),
                tableOutput("resp_tbl")
)



##### App local functions below: -----
app_render_ <- function(frames, ## paste over spinifex render to add size
                        axes = "left",
                        alpha = 1,
                        cluster = NULL,
                        ...){
  ## Initialize
  if(length(frames) == 2)
    data_frames  <- data.frame(frames[[2]])
  basis_frames   <- data.frame(frames[[1]])
  manip_var      <- attributes(frames$basis_frames)$manip_var
  n_frames       <- max(basis_frames$frame)
  p              <- nrow(basis_frames) / n_frames
  d              <- ncol(basis_frames) - 2
  angle          <- seq(0, 2 * pi, length = 360)
  circ           <- data.frame(x = cos(angle), y = sin(angle))
  ## Scale basis axes
  if(axes != "off"){
    zero         <- app_set_axes_position(0, axes)
    circ         <- app_set_axes_position(circ, axes)
    basis_frames <- data.frame(app_set_axes_position(basis_frames[, 1:2], axes),
                               basis_frames[, (d+1):ncol(basis_frames)])
  }
  ## manip var axes asethetics
  axes_col <- rep("red", p)
  axes_siz <- rep(0.3, p)
  axes_col[manip_var] <- "blue"
  axes_siz[manip_var] <- .6

  x_max <- max(data_frames[, 1], circ[, 1])
  x_min <- min(data_frames[, 1], circ[, 1])
  y_max <- max(data_frames[, 2], circ[, 2])
  y_min <- min(data_frames[, 2], circ[, 2])
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
                   axis.title.x = element_blank(),     ## Remove axis titles for grand
                   axis.title.y = element_blank(),     ## Remove axis titles for grand
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
        data = data_frames, size = 3, alpha = alpha,
        mapping = ggplot2::aes(x = x, y = y, frame = frame,
                               color = cluster,
                               fill  = cluster,
                               shape = cluster)
      )
    )

  if(axes != "off"){
    gg <- gg +
      ## Circle path
      ggplot2::geom_path(
        data = circ, color = "grey80", size = .3, inherit.aes = F,
        mapping = ggplot2::aes(x = x, y = y)
      ) +
      ## Basis axes segments
      suppressWarnings( ## Suppress for unused aes "frame".
        ggplot2::geom_segment(
          data = basis_frames, size = axes_siz, colour = axes_col,
          mapping = ggplot2::aes(x = x,
                                 y = y,
                                 xend = zero[, 1], yend = zero[, 2],
                                 frame = frame)
        )
      ) +
      ## Basis axes text labels
      suppressWarnings( ## Suppress for unused aes "frame".
        ggplot2::geom_text(
          data = basis_frames,
          mapping = ggplot2::aes(x = x, y = y,
                                 frame = frame, label = lab),
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
           ...){
    
    if(is.null(basis) & !is.null(data)){
      message("NULL basis passed. Initializing random basis.")
      basis <- tourr::basis_random(n = ncol(data))
    }
    
    p <- nrow(basis)
    m_sp <- create_manip_space(basis, manip_var)
    r_m_sp <- rotate_manip_space(manip_space = m_sp, theta, phi)
    
    basis_frames <- cbind(as.data.frame(r_m_sp), frame = 1)
    colnames(basis_frames) <- c("x", "y", "z", "frame")
    if(!is.null(data)){
      if(rescale_data){data <- tourr::rescale(data)}
      data_frames  <- cbind(as.data.frame(data %*% r_m_sp), frame = 1)
      data_frames[, 1] <- scale(data_frames[, 1], scale = FALSE)
      data_frames[, 2] <- scale(data_frames[, 2], scale = FALSE)
      colnames(data_frames) <- c("x", "y", "z", "frame")
    }
    
    ## Add labels, attribute, and list
    basis_frames$lab <-
      if(!is.null(lab)){
        rep(lab, nrow(basis_frames) / length(lab))
      }else{
        if(!is.null(data)){abbreviate(colnames(data), 3)
        }else{paste0("V", 1:p)}
      }
    
    attr(basis_frames, "manip_var") <- manip_var
    
    frame <- if(!is.null(data)){
      list(basis_frames = basis_frames, data_frames = data_frames)
    } else list(basis_frames = basis_frames)
    
    gg <- app_render_(frames = frame, ...) +
      ggplot2::coord_fixed() +
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
    
    return(gg)
  }

app_html_red <- function(string){
  paste0("<strong><span style='color:red'>", string, "</span><strong>")
}


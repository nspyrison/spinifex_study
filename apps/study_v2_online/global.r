##### study_v2_online\global.r

##### global.r, spinifex_study -----
### Setup -----
library("shiny")
library("spinifex")
library("ggplot2")
# library("tidyr")
library("dplyr")
library("plotly")
library("GGally")
library("lubridate") ## For timer
library("loggit")    ## For logging
library("shinyjs")   ## advanced control of html elements (disabling tabs)
require("V8")        ## req for shinyjs::extendShinyjs()

## Global variable controlers
do_log            <- F
do_disp_dev_tools <- F
sim_series        <- 300 ## Iteration of data to look at. Expects even hundred.

##### Required inputs -----
block_difficulties <- c("easy", "hard")
task_header        <- "Rate the relative importance of ANY/ALL variables in terms of distinugishing between the given clusters."
task_questions     <- c("Very important distinguishing clusters 'a' from 'b'",
                        "Somewhat important distinguishing clusters 'a' from 'b'",
                        "Very important distinguishing clusters 'b' from 'c'",
                        "Somewhat important distinguishing clusters 'b' from 'c'")
## Survey questions; n = 21 = 9 + (4*3)
survey_questions <- c("What sex are you?",
                      "What age group do you belong to?",
                      "What is your English proficiency?",
                      "What is your highest completed education?",
                      "I am experienced with data visualization.",
                      "I am experienced with tabular data.",
                      "I am experienced with clustering classification techniques.",
                      "I am experienced with multivariate statistical analysis.",
                      "I am experienced with machine learning.", ## If you add a question here, update .surv_fct_col_start
                      rep(c("I was already familiar with this visualization.",
                            "I found this visualization easy to use.",
                            "I felt confident in my answers with this visualization.",
                            "I liked using this visualization."), 3)
)


##### Startup initialization variables
## Context, "onStart()" and onStop()
context_line  <- paste0("Spinifex STUDY, --- (spinifex v", packageVersion("spinifex"),
                        ") --- Started ", Sys.time())
this_Sys.info <- paste(Sys.info()[1:5], collapse = ", ")
context_msg   <- paste(sep = " \n",
                       context_line,
                       paste0("Log file: ", log_file), 
                       paste0("Group number: ", log_num, "."),
                       paste0("Sys.info()[1:5]: ", this_Sys.info)
)

## Local app js to handle disabling tabs
app_jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"
## Local app css to format disabled tabs
app_css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"

##### Logging
## Logging format: loggit("INFO", "<main msg>", "<detail>")
## Set log file, finding the first unused number, will need to write to a google sheet or otherwise store a file.
log_base <- paste0("log_", Sys.info()[4], "_")
log_num  <- 1
log_file <- ""
if (do_log == T){
  log_name <- sprintf(paste0(log_base, "%03d"), log_num)
  log_file <- paste0(log_name, ".json")
  while (file.exists(log_file)) { ## Find an unused log number
    log_num  <- log_num + 1
    log_name <- sprintf(paste0(log_base, "%03d"), log_num)
    log_file <- paste0(log_name, ".json")
  }
  set_logfile(log_file)
} else { ## when do_log != T
  log_num  <- sample(1:3, 1)
  log_file <- "Logging is off! Log and responses not being recorded."
}
message(paste0("do_log, log_file: ", do_log, log_file))


## Set group (factor order) based on log number mod 3
fct_ord_latin_sq <- rbind(c(1, 2, 3), ## grp 1; "pca", "grand", "manual"
                          c(2, 3, 1), ## grp 2; "grand", "manual", "pca"
                          c(3, 1, 2)  ## grp 3; "manual", "pca", "grand"
)
this_group <- 1 + (log_num - 1) %% 3  ## Expects [1,2,3] 
this_factor_order    <- fct_ord_latin_sq[this_group, ]
this_factor_nm_order <- c("pca", "grand", "manual")[this_factor_order]


## onStart();
message(context_msg)
loggit("INFO", "=====Spinifex study app start.=====")

onStop(function() {
  message(context_msg)
  loggit("INFO", "=====Spinifex study app stop.=====")
  set_logfile(logfile = NULL, confirm = TRUE)
  ## Try to autosave if not saved and do_log == T?
  #### note that rv$resp_tbl is out of scope to the global file.
})


## Load training data and tour paths
t_dat_len <- 4
s_t_dat <- s_t_tpath <-  list()
for (i in 1:t_dat_len) {
  s_t_dat[[i]]   <- readRDS(
    paste0("../data/simulation_data_t", i, ".rds")
  )
  s_t_tpath[[i]] <- readRDS(
    paste0("../data/grand_tpath_t", i, ".rds")
  )
}

## Load data and tour paths
dat_len   <- 12
s_dat <- s_tpath <-  list()
for (i in 1:dat_len) {
  s_dat[[i]] <- readRDS(
    paste0("../data/simulation_data", sim_series + i, ".rds")
  )
  s_tpath[[i]] <- readRDS(
    paste0("../data/grand_tpath", sim_series + i, ".rds")
  )
}


##### Global variable initialization -----
n_trainings        <- length(s_t_dat)            ## ~4
n_factors          <- length(fct_nm_vect)        ## ~3
n_tasks            <- length(task_header)        ## ~1
n_task2_questions  <- length(s_task2_questions)  ## ~4
n_blocks           <- length(block_difficulties) ## ~2
n_survey_questions <- length(s_survey_questions) ## ~21
PC_cap             <- 4 ## Number of principal components to choose from.
pal                <- "Dark2"

##### UI START -----
##### sidebar_ui ----
##TODO may want to split for training/evaluation
sidebar_x <- conditionalPanel(
  ##TODO CHANGE CONDITION
  condition = "output.section_nm == 'training' || output.section_nm == 'task'", 
  sidebarPanel(
    ##TODO:
    p("Sidebar content here!")
  ) ## Close sidebarPanel()
) ## Close conditionalPanel(), end sidebar_ui section

##### _Survey mainPanel -----
## survey init
.surv_css <- HTML("<div style=\"width:300px;\">
                      <div style=\"float:left;\">strongly disagree</div>
                      <div style=\"float:right;\">strongly agree</div>
                    </div>")
.surv_fct_col_start <- 9 ## Number of last question before breaking out into factor specific columns
.surv_fct_col1 <- column(4, 
                         h3(this_factor_nm_order[1]),
                         hr(),
                         h4(s_survey_questions[.surv_fct_col_start + 1]),
                         sliderInput(paste0("survey", .surv_fct_col_start + 1), 
                                     label = .surv_lab, min = 1, max = 9, value = 5),
                         h4(s_survey_questions[.surv_fct_col_start + 2]),
                         sliderInput(paste0("survey", .surv_fct_col_start + 2),
                                     label = .surv_lab, min = 1, max = 9, value = 5),
                         h4(s_survey_questions[.surv_fct_col_start + 3]),
                         sliderInput(paste0("survey", .surv_fct_col_start + 3),
                                     label = .surv_lab, min = 1, max = 9, value = 5),
                         h4(s_survey_questions[.surv_fct_col_start + 4]),
                         sliderInput(paste0("survey", .surv_fct_col_start + 4),
                                     label = .surv_lab, min = 1, max = 9, value = 5)
)

.surv_fct_col2 <- column(4, 
                         h3(this_factor_nm_order[2]),
                         hr(),
                         h4(s_survey_questions[.surv_fct_col_start + 5]),
                         sliderInput(paste0("survey", .surv_fct_col_start + 5), 
                                     label = .surv_lab, min = 1, max = 9, value = 5),
                         h4(s_survey_questions[.surv_fct_col_start + 6]),
                         sliderInput(paste0("survey", .surv_fct_col_start + 6),
                                     label = .surv_lab, min = 1, max = 9, value = 5),
                         h4(s_survey_questions[.surv_fct_col_start + 7]),
                         sliderInput(paste0("survey", .surv_fct_col_start + 7),
                                     label = .surv_lab, min = 1, max = 9, value = 5),
                         h4(s_survey_questions[.surv_fct_col_start + 8]),
                         sliderInput(paste0("survey", .surv_fct_col_start + 8),
                                     label = .surv_lab, min = 1, max = 9, value = 5)
)
.surv_fct_col3 <- column(4, 
                         h3(this_factor_nm_order[3]),
                         hr(),
                         h4(s_survey_questions[.surv_fct_col_start + 9]),
                         sliderInput(paste0("survey", .surv_fct_col_start + 9), 
                                     label = .surv_lab, min = 1, max = 9, value = 5),
                         h4(s_survey_questions[.surv_fct_col_start + 10]),
                         sliderInput(paste0("survey", .surv_fct_col_start + 10),
                                     label = .surv_lab, min = 1, max = 9, value = 5),
                         h4(s_survey_questions[.surv_fct_col_start + 11]),
                         sliderInput(paste0("survey", .surv_fct_col_start + 11),
                                     label = .surv_lab, min = 1, max = 9, value = 5),
                         h4(s_survey_questions[.surv_fct_col_start + 12]),
                         sliderInput(paste0("survey", .surv_fct_col_start + 12),
                                     label = .surv_lab, min = 1, max = 9, value = 5)
)

mainpanel_survey <- mainPanel(
  conditionalPanel( ##TODO consider removing
    condition = "output.is_saved == 0",
    ## Factor independant questions
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
    sliderInput("survey5", label = .surv_lab,
                min = 1, max = 9, value = 5),
    strong(s_survey_questions[6]),
    sliderInput("survey6",label = .surv_lab,
                min = 1, max = 9, value = 5),
    strong(s_survey_questions[7]),
    sliderInput("survey7",label = .surv_lab,
                min = 1, max = 9, value = 5),
    strong(s_survey_questions[8]),
    sliderInput("survey8",label = .surv_lab,
                min = 1, max = 9, value = 5),
    strong(s_survey_questions[9]),
    sliderInput("survey9",label = .surv_lab,
                min = 1, max = 9, value = 5),
    ## Factor dependant questions
    fluidRow(.surv_fct_col1, .surv_fct_col2, .surv_fct_col3),
    hr(),
    ## Save and thanks
    actionButton("save_resp", "save responses"),
    htmlOutput("save_msg"),
    conditionalPanel(
      condition = "output.is_saved == 1",
      h4("Your responses and survey have been saved!"),
      h3("Thank you for participating!")
    )
  ) ## close survey condition panel
) ## close mainPanel() End of main_ui section.


##### main_ui -----
#TODO: Need to break out main panels for each section
mainpanel_x <- mainPanel(
  
)


### DEV helping displays:
dev_tools <- p("") ## FALSE/default, don't display anything
if (do_disp_dev_tools == TRUE){
  dev_tools <- fluidPage(
    actionButton("browser", "browser()"),
    verbatimTextOutput("dev_msg"),
    tableOutput("resp_tbl")
  )
}
##### UI, combine panels -----
ui <- fluidpage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = app_jscode),
  shinyjs::inlineCSS(app_css),
  ## Content structure
  navbarPage(title = "Multivariate visualization study", id = "navbarid",
             tabPanel(title = "Intro", ## id and value args not needed
                      p("Intro content")
             ),
             tabPanel(title = "Training", ## id and value args not needed
                      p("Training  content here.")
             ),
             tabPanel(title = "Evaluation", ## id and value args not needed
                      p("Evaluation content here.")
             ),
             tabPanel(title = "Survey", ## id and value args not needed
                      p("Survey content here.")
             ),
             dev_tools
  ) ## Close content navbarPage
) ## Close ui fluidpage


##### App local functions: -----
## Define some functions unique to this app to be called in app.r
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
  paste0("<strong><span style='color:red'>", string, "</span><strong>")
}

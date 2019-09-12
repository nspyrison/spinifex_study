### Global init for each factor variant.

##### Initialization -----
library("ggplot2")
library("spinifex")
library("shiny")

### Required inputs
demo_dat <- tourr::flea[, 1:6]
s_dat <- list(olive, wine, mtcars)
uo_dat_names <- c("olive", "wine", "mtcars")
s_blocks <- c("n", "d", "s")
s_block_names <- c("clusters, n", "important variable, d", "correlated variables, s")
s_block_questions <- c("How many clusters exist?",
                       "How few dimensions could the data be represented in?",
                       "Which dimensions are highly correlated?")
s_survey_questions <- c("This visualization was easy to use.", ### INPUT
                        "I am confident of my answers.",
                        "This visualization is easily understandable.",
                        "I would recomend using this visualization.",
                        "I am an expert on multivarite data and related visualiztion.",
                        "I have broad experience with data disualization.",
                        "I had previous knowledge of this visualization.")

n_blocks <- length(s_blocks)
n_reps <- length(s_dat)
s_blockrep_id <- paste0(rep(s_blocks, each = n_reps), rep(1:n_reps, n_reps))

dat_order <- sample(1:n_reps, n_reps, replace = F)
o_s_dat <- s_dat[dat_order] # Ordered Set of DATa
col_dataset <- c(rep(uo_dat_names[dat_order], n_reps), 
                 rep(NA, length(s_survey_questions)))

###### Sample and order data ----
### Create samples, of the set of data with point jitter 
s_samp_dat <- NULL
for (i in 1:n_reps) {
  n <- nrow(o_s_dat[[i]])
  row_samp <- sample(1:n, n, replace = T)
  dat_samp <- o_s_dat[[i]][row_samp, ]
  
  jitter <- function(x){
    if (is.numeric(x)) {
      swing <- 1 / 40 * (max(x) - min(x))
      x <- x + runif(n, -swing, swing)
    }
    else NULL # Null non-numeric columns, will be droped.
  }
  
  l_x <- lapply(dat_samp, jitter)
  col_remaining <- length(unlist(l_x)) / n
  df_x <- data.frame(matrix(unlist(l_x), ncol = col_remaining, byrow = F))
  colnames_x <- names(l_x)[unlist(lapply(l_x, is.numeric))]
  colnames(df_x) <- colnames_x
  
  s_samp_dat[[i]] <- df_x
}

###### Text sets -----
intro_header_row <- paste0("Introduction -- ", s_block_names)
m_blockrep_id <- matrix(paste0("Task -- ", s_blockrep_id), ncol = n_reps)
s_header_text <- c(rbind(intro_header_row , m_blockrep_id), "All tasks completed. Continue to the Survey tab.")

blank_row <- rep("", n_blocks)
m_questions <- matrix(rep(s_block_questions, each = n_reps), ncol = n_reps)
s_question_text <- c(rbind(blank_row, m_questions), "")

intro_top_row <- paste0(c("In this section you will be asked to determine the number of clusters contained in the data. ",
                          "In this section you will be asked to determine the number of how few variables accurately portray the variation in the data. ",
                          "In this section you will be asked to determine which variables are highly correlated. "), 
                        "Each task contains different data, with the display as demonstrated below.")
m_blank <- matrix("", ncol = n_reps, nrow = n_blocks)
s_top_text <- c(rbind(intro_top_row, m_blank), "")

intro_bottom_row <- "You have 2 minutes to study the display before being prompted to submit your answer."
s_bottom_text <- c(rbind(intro_bottom_row, m_blank), "")

##### tabPanels -----
### Task panels
panel_task <- tabPanel("Tasks", 
                       sidebarPanel(
                         selectInput("static_method", label = "method", 
                                     choices = c("pca", "splom"),
                                     selected = "pca"),
                         hr(), # horizontal line
                         actionButton("next_task_button", "Next task")
                       ),
                       mainPanel(textOutput('timer_disp'),
                                 verbatimTextOutput("header_text"),
                                 verbatimTextOutput("top_text"),
                                 plotOutput("task_plot"),
                                 verbatimTextOutput("question_text"),
                                 numericInput("task_response", "", "", min = 0, max = 100),
                                 verbatimTextOutput("response_msg"), 
                                 verbatimTextOutput("bottom_text")
                       )
)

### Introduction tabPanels
panel_study_intro <- tabPanel("Study introduction",
                              h3("Welcome to the study.")
)

### _Survey tabPanel 
panel_survey <-
  tabPanel("Survey", ### INPUT
           h3("How much do you agree with the following statments?"),
           h4(s_survey_questions[1]),
           sliderInput("ans_ease",
                       label = div(style='width:300px;',
                                   div(style='float:left;', 'strongly disagree'),
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5),
           h4(s_survey_questions[2]),
           sliderInput("ans_confidence",
                       label = div(style='width:300px;',
                                   div(style='float:left;', 'strongly disagree'),
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5),
           h4(s_survey_questions[3]),
           sliderInput("ans_understand",
                       label = div(style='width:300px;',
                                   div(style='float:left;', 'strongly disagree'),
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5),
           h4(s_survey_questions[4]),
           sliderInput("ans_use",
                       label = div(style='width:300px;',
                                   div(style='float:left;', 'strongly disagree'),
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5),
           h4(s_survey_questions[5]),
           sliderInput("ans_high_dim",
                       label = div(style='width:300px;',
                                   div(style='float:left;', 'strongly disagree'),
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5),
           h4(s_survey_questions[6]),
           sliderInput("ans_data_vis",
                       label = div(style='width:300px;',
                                   div(style='float:left;', 'strongly disagree'),
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5),
           h4(s_survey_questions[7]),
           sliderInput("ans_previous_knowledge",
                       label = div(style='width:300px;',
                                   div(style='float:left;', 'strongly disagree'),
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5)
  )

### Answer table columns
col_blockrep <- c(s_blockrep_id, paste0("survey", 1:7))
col_question <- c(rep(s_block_questions, each = n_reps),
                  s_survey_questions)

### Finalize responses panel
panel_finalize <- tabPanel("Review answers",
                           tableOutput("ans_tbl"),
                           actionButton("save_ans", "save results"),
                           verbatimTextOutput("save_msg"),
                           h4("Thank you for participating.")
)

##### UI, combine panels -----
ui <- fluidPage(
  navbarPage("Multivariate data visualization study",
             panel_study_intro,
             panel_task,
             panel_survey,
             panel_finalize)
  , verbatimTextOutput("dev_msg")
)
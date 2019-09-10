### Global init for each factor variant.

##### Initialization -----
library("ggplot2")
library("spinifex")
library("shiny")

demo_dat <- tourr::flea[, 1:6]  ### INPUT 
s_dat <- list(olive, wine, mtcars)  ### INPUT
uo_dat_names <- c("olive", "wine", "mtcars") ### INPUT
s_blocks <- c("n", "d", "s") ### INPUT
s_block_questions <- c("How many clusters exist?", ### INPUT
                       "How few dimensions could the data be represented in?",
                       "Which dimensions are highly correlated?")

s_block_num <- 1:length(s_blocks)
n_reps <- length(s_dat)
s_reps <- 1:n_reps
dat_order <- sample(1:n_reps, n_reps, replace = F)
o_s_dat <- s_dat[dat_order] # Ordered Set of DATa
col_dataset <- c(rep(uo_dat_names[dat_order], n_reps), 
                 rep(NA, 7)) # length of survey questions

###### Sample and order ----
### Create samples, of the set of data with point jitter 
s_samp_dat <- NULL
for (i in s_reps) {
  n <- nrow(o_s_dat[[i]])
  row_samp <- sample(1:n, n, replace=T)
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

###### ui tabPanels ----
### _Task tabPanels ----
# 'panel_',blck,rep
panel = NULL
for (i in s_block_num){
  for (rep in c("demo", s_reps)){
    blck <- s_blocks[i]
    quest <- s_block_questions[i]
    panel <- tabPanel(paste0("Task ", blck, rep), 
                      h2(paste0("Task ", blck, rep)),
                      plotOutput(paste0("plot_", blck, rep)),
                      numericInput(paste0("ans_", blck, rep), quest, "")
    )
    assign(paste0("panel_", blck, rep), panel)
  }
}

### _Introduction tabPanels -----
panel_study_intro <- tabPanel("Study introduction", ### INPUT
                              h3("Welcome to the study")
)
panel_n_intro <- tabPanel("Introduction -- clusters, n",
                          h3("In this section you will be asked to determine the number of clusters contained in the data."),
                          h4("Each task contains different data, with the display as demonstrated below."),
                          h4("You have 2 minutes to study the display before being prompted to submit your answer."),
                          plotOutput("plot_ndemo")
)
panel_d_intro <- tabPanel("Introduction -- important variables, d",
                          h3("In this section you will be asked to determine the number of how few variables accurately portray the variation in the data."),
                          h4("Each task contains different data, with the display as demonstrated below."),
                          h4("You have 2 minutes to study the display before being prompted to submit your answer."),
                          plotOutput("plot_ddemo")
)
panel_s_intro <- tabPanel("Introduction -- covariance, s",
                          h3("In this section you will be asked to determine which variables are highly correlated."),
                          h4("Each task contains different data, with the display as demonstrated below."),
                          h4("You have 2 minutes to study the display before being prompted to submit your answer."),
                          plotOutput("plot_sdemo")
)

### _Survey tabPanel ----
survey_questions <- c("This visualization was easy to use.", ### INPUT
                      "I am confident of my answers.",
                      "This visualization is easily understandable.",
                      "I would recomend using this visualization.",
                      "I am an expert on multivarite data and related visualiztion.",
                      "I have broad experience with data disualization.",
                      "I had previous knowledge of this visualization.")
panel_survey <- 
  tabPanel("Survey", ### INPUT
           h3("How much do you agree with the following statments."),
           h4(survey_questions[1]),
           sliderInput("ans_ease", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           ),
           h4(survey_questions[2]),
           sliderInput("ans_confidence", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           ),
           h4(survey_questions[3]),
           sliderInput("ans_understand", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           ),
           h4(survey_questions[4]),
           sliderInput("ans_use", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           ),
           h4(survey_questions[5]),
           sliderInput("ans_high_dim", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           ),
           h4(survey_questions[6]),
           sliderInput("ans_data_vis", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           ),
           h4(survey_questions[7]),
           sliderInput("ans_previous_knowledge", 
                       label = div(style='width:300px;', 
                                   div(style='float:left;', 'strongly disagree'), 
                                   div(style='float:right;', 'strongly agree')),
                       min = 1, max = 9, value = 5
           )
  )

### Answer table columns
col_blockrep <- c(paste0(rep(s_blocks, each = n_reps), s_reps),
                  paste0("survey", 1:7))
col_question <- c(rep(s_block_questions, each = n_reps),
                  survey_questions)

panel_finalize <- tabPanel("Review answers",
                           tableOutput("ans_tbl"),
                           actionButton("save_ans", "save results"),
                           verbatimTextOutput("save_msg"),
                           h4("Thank you for participating.")
)


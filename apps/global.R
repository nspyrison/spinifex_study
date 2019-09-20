### Global init for each factor variant.

##### Initialization -----
library("ggplot2")
library("spinifex")
library("shiny")
library("tidyr")
library("mvtnorm")
library("plotly")


### Required inputs
intro_dat <- tourr::rescale(tourr::flea[, 1:6])
attr(intro_dat, "cluster") <- tourr::flea$species
n_reps <- 3
s_blocks <- c("n", "d", "s")
s_block_names <- c("clusters, n", "important variable, r", "correlated variables, s")
s_block_questions <- c("How many clusters exist?",
                       "Rank the variables in order of importance for distinguishing groups.",
                       "dummy 3rd question")
s_survey_questions <- c("This visualization was easy to use.", ### INPUT
                        "I am confident of my answers.",
                        "This visualization is easily understandable.",
                        "I would recomend using this visualization.",
                        "I am an expert on multivarite data and related visualiztion.",
                        "I have broad experience with data disualization.",
                        "I had previous knowledge of this visualization.")

n_blocks <- length(s_blocks)
s_blockrep_id <- paste0(rep(s_blocks, each = n_reps), rep(1:n_reps, n_reps))
n_task_pgs <- n_blocks * (n_reps + 1)

###### Simulate clusters
simulate_clusters <- function(p = 10, pnoise = 4, cl = 4){
  #p = 10; pnoise = 4; cl = 4
  x <- NULL
  ncl <- NULL
  mncl <- NULL
  vc <- NULL
  for (i in 1:cl) {
    n <- sample(30:150, 1)
    ### TODO: double check that vc is doing what we want.
    # vc_original <- matrix(sample(seq(-0.1, 0.7, by = 0.1), 1), nrow = p, ncol = p) # Add some association
    # print(vc_original) # not as expected
    ### making vc random, symetric, definite matrix:
    vc <- matrix(sample(seq(-.1, 0.7, by = 0.1), p * p, replace = T), nrow = p) 
    ind <- lower.tri(vc) 
    vc[ind] <- t(vc)[ind] 
    vc <- lqmm::make.positive.definite(vc) # Variance-covariance matrix
    diag(vc) <- 1
    mn <- c(sample(seq(-3, 3, 1), p - pnoise, replace = T), rep(0, pnoise))
    x <- rbind(x, rmvnorm(n = n, mean = mn, vc))
    ncl <- c(ncl, n)
    mncl <- rbind(mncl, mn)
  }
  x <- scale(x)
  x <- as.data.frame(x)
  
  # Show color on plots to check clustering
  cluster <- factor(rep(letters[1:cl], ncl))
  cluster_col <- col_of(cluster)
  
  # Scramble rows and columns
  x.indx <- sample(1:nrow(x))
  y.indx <- sample(1:ncol(x))
  x <- x[x.indx, y.indx]
  cluster <- cluster[x.indx]
  
  attr(x, "ncl") <- ncl         # number of obs in each cluster
  attr(x, "mncl") <- mncl       # mean of each cluster*variable
  attr(x, "vc") <- vc           # variance-covariance matrix
  attr(x, "cluster") <- cluster # culter factor
  attr(x, "x.indx") <- x.indx   # order variables were scrambled in
  return(x)
}

s_dat <- list(intro_dat) # intro data (flea) is first
for (i in 1:n_reps){
  this_sim <- simulate_clusters(p = 10, pnoise = 4, cl = 4)
  colnames(this_sim) <- paste0("V", 1:10)
  s_dat[[length(s_dat) + 1]] <- this_sim
}
df_simulation <- NULL
for (i in 2: length(s_dat)) {
  this_df <- data.frame(s_dat[[i]], simulation_id = i-1)
  df_simulation <- rbind(df_simulation, this_df)
}
ndf_simulation <- tidyr::nest(df_simulation, -simulation_id)
col_ndf_simulation <- rbind(ndf_simulation, ndf_simulation, ndf_simulation, ### INPUT
                            NA, NA, NA, NA, NA, NA, NA)
col_sim_id <- col_ndf_simulation$simulation_id
col_sim    <- col_ndf_simulation$data # List of different dim df, doesn't stay nested.


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

##### tabPanels (UI objs) -----
### Task panels
l_choices <- list(NULL)
for (i in 1:5){ 
  l_choices[[i]] <- i
}
names(l_choices) <- paste0("choice ", 1:5)
panel_task <- tabPanel(
  "Tasks", 
  sidebarPanel(
    numericInput("sim_p", label = "Number of variables, p", value = 10),
    numericInput("sim_pnoise", label = "Number of noise variables, pnoise", value = 4),
    numericInput("sim_cl", label = "Number of clusters, cl", value = 4),
    fluidRow(column(6, radioButtons(inputId = "x_axis", label = "x axis", choices = "PC1")),
             column(6, radioButtons(inputId = "y_axis", label = "y axis", choices = "PC2"))),
    hr(), # horizontal line
    actionButton("next_task_button", "Next task")
  ),
  mainPanel(textOutput('timer_disp'),
            # verbatimTextOutput("header_text"),
            # verbatimTextOutput("top_text"),
            plotOutput("task_pca"),
            plotlyOutput("task_gtour")#,
            # verbatimTextOutput("question_text"),
            # checkboxGroupInput(inputId="test1", 
            #                    label=div(style='width:358px;',
            #                              div(style='float:left;', 'most important'),
            #                              div(style='float:right;', 'least important')), 
            #                    choices=1:9, inline = TRUE),
            # checkboxGroupInput(inputId="test2", label="Variable 2", choices=1:9, inline = TRUE),
            # checkboxGroupInput(inputId="test3", label="Variable 3", choices=1:9, inline = TRUE),
            # checkboxGroupInput(inputId="test4", label="Variable 4", choices=1:9, inline = TRUE),
            # checkboxGroupInput(inputId="test5", label="Variable 5", choices=1:9, inline = TRUE),
            # checkboxGroupInput(inputId="test6", label="Variable 6", choices=1:9, inline = TRUE),
            # checkboxGroupInput(inputId="test7", label="Variable 7", choices=1:9, inline = TRUE),
            # checkboxGroupInput(inputId="test8", label="Variable 8", choices=1:9, inline = TRUE),
            # checkboxGroupInput(inputId="test9", label="Variable 9", choices=1:9, inline = TRUE),
            # verbatimTextOutput("response_msg"), 
            # verbatimTextOutput("bottom_text")
  )
)

### Introduction tabPanels
panel_study_intro <- tabPanel("Study introduction",
                              h3("Welcome to the study.")
)

### Survey tabPanel 
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
             #panel_study_intro,
             panel_task,
             panel_survey,
             panel_finalize)
  , verbatimTextOutput("dev_msg")
)


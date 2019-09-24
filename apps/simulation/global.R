### Global init for each factor variant.

##### Initialization -----
library("ggplot2")
library("spinifex")
library("shiny")
library("tidyr")
library("mvtnorm")
library("plotly")


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
  
  
  attr(x, "ncl") <- ncl            # number of obs in each cluster
  attr(x, "mncl") <- mncl          # mean of each cluster*variable
  attr(x, "vc") <- vc              # variance-covariance matrix
  attr(x, "cluster") <- cluster    # culter factor
  attr(x, "col_reorder") <- y.indx # order variables were scrambled in
  return(x)
}


##### tabPanels (UI objs) -----
### Task panels
panel_task <- tabPanel(
  "Tasks", 
  sidebarPanel(
    numericInput("sim_p", label = "Number of variables, p", value = 10),
    numericInput("sim_pnoise", label = "Number of noise variables, pnoise", value = 4),
    numericInput("sim_cl", label = "Number of clusters, cl", value = 4),
    fluidRow(column(6, radioButtons(inputId = "x_axis", label = "x axis", choices = "PC1")),
             column(6, radioButtons(inputId = "y_axis", label = "y axis", choices = "PC2"))),
    hr(), # horizontal line
    actionButton("save_sim", "save simulation"),
    verbatimTextOutput("save_msg")
  ),
  mainPanel(textOutput('timer_disp'),
            plotOutput("task_pca"),
            plotlyOutput("task_gtour", height = 600)
  )
)

### Introduction tabPanels
panel_study_intro <- tabPanel("Study introduction",
                              h3("Welcome to the study.")
)





##### UI, combine panels -----
ui <- fluidPage(navbarPage("Multivariate data visualization study",
                           panel_task
                          )
  , verbatimTextOutput("dev_msg")
)

